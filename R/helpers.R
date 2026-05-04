#' Get the column types of a tibble
#'
#' (internal function not for user interaction)
#' @param input [data.frame][data.frame]\cr table from which to get column
#'   types.
#' @importFrom checkmate assertDataFrame
#' @importFrom tibble tibble
#' @importFrom dplyr summarise_all left_join pull
#' @importFrom tidyr gather
#' @importFrom stringr str_c

.getColTypes <- function(input = NULL){

  assertDataFrame(x = input)

  types <- tibble(col_type = c("character", "integer", "numeric", "double", "logical", "Date"),
                  code = c("c", "i", "n", "d", "l", "D"))

  out <- input %>%
    summarise_all(class) %>%
    gather(col_name, col_type) %>%
    left_join(y = types, by = "col_type") %>%
    pull("code") %>%
    str_c(collapse = "")

  return(out)

}


#' Match target terms with a vocabulary
#'
#' This function takes a table and replaces the values of target columns with
#' harmonised values from the project vocabulary (ontology or gazetteer).
#' Unmatched terms are queued for interactive assignment via match_builder.
#' @param table [`data.frame(1)`][data.frame]\cr a table with columns to
#'   harmonise.
#' @param columns [`character(.)`][character]\cr the columns containing the
#'   terms to match; must appear left-to-right from broadest to most specific
#'   level.
#' @param dataseries [`character(1)`][character]\cr the source dataseries name.
#' @param ontology [`character(1)`][character]\cr name of the vocabulary
#'   to match against (e.g. \code{"gazetteer"}, \code{"commodity"}).
#' @param colsAsClass [`logical(1)`][logical]\cr if TRUE, column names are
#'   matched against level labels in levels.csv to determine the hierarchy
#'   position. If FALSE, all columns are treated as a flat list.
#' @param groupMatches [`logical(1)`][logical]\cr whether to collapse multiple
#'   matches per row with " | ".
#' @param stringdist [`logical(1)`][logical]\cr whether to use string distance
#'   for fuzzy suggestions (passed to match_builder).
#' @param strictMatch [`logical(1)`][logical]\cr if TRUE, disable the
#'   different-parent recovery logic.
#' @param simplify [`list(.)`][list]\cr normalisation steps applied to the
#'   first column before matching. Named list; valid names: "squish",
#'   "lowercase", "dashes", "duplpunct", "remove", "replace", "chartr".
#' @param beep [`integerish(1)`][integer]\cr sound to play when interactive
#'   input is needed (passed to beepr::beep).
#' @param verbose [`logical(1)`][logical]\cr print matching details.
#' @return The input table with additional columns: \code{id} (canonical ID),
#'   \code{match} (dataseries label that was matched), \code{external}
#'   (original source label).
#' @importFrom checkmate assertDataFrame assertCharacter assertIntegerish
#'   assertLogical assertList assertNames
#' @importFrom dplyr pull filter select mutate distinct anti_join bind_rows
#'   left_join arrange rename if_else group_by summarise across
#' @importFrom tibble tibble add_column
#' @importFrom tidyselect all_of any_of where
#' @importFrom stringr str_replace_all str_squish str_to_lower str_remove_all
#'   str_extract
#' @importFrom tidyr unite separate_wider_delim fill pivot_wider
#' @importFrom sf st_drop_geometry st_sf
#' @export

.matchOntology <- function(table = NULL, columns = NULL, dataseries = NULL,
                           ontology = NULL, colsAsClass = TRUE,
                           groupMatches = FALSE, stringdist = TRUE,
                           strictMatch = FALSE, simplify = NULL,
                           beep = NULL, verbose = FALSE){

  assertDataFrame(x = table, min.cols = length(columns))
  assertCharacter(x = columns, any.missing = FALSE)
  assertCharacter(x = dataseries, len = 1, any.missing = FALSE)
  assertCharacter(x = ontology, len = 1, any.missing = FALSE)
  assertIntegerish(x = beep, len = 1, lower = 1, upper = 11, null.ok = TRUE)
  assertLogical(x = colsAsClass, len = 1, any.missing = FALSE)
  assertLogical(x = groupMatches, len = 1, any.missing = FALSE)
  assertLogical(x = verbose, len = 1, any.missing = FALSE)
  assertLogical(x = stringdist, len = 1, any.missing = FALSE)
  assertLogical(x = strictMatch, len = 1, any.missing = FALSE)
  assertList(x = simplify, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  if (!is.null(simplify)) {
    assertNames(x = names(simplify),
                subset.of = c("squish", "lowercase", "dashes", "duplpunct",
                              "remove", "replace", "chartr"))
  }

  # --- setup ----------------------------------------------------------------

  levels <- .read_levels(ontology)
  allCols <- levels$label

  if (colsAsClass) {
    assertSubset(x = head(columns, 1), choices = allCols)
    allCols <- allCols[which(allCols %in% head(columns, 1)) :
                       which(allCols %in% tail(columns, 1))]
  } else {
    allCols <- columns
  }

  # remove white-space and dots from all character columns
  table <- table |>
    mutate(across(where(is.character), function(x) {
      str_replace_all(trimws(x), "[.]", "")
    }))

  # detect which hierarchy levels are missing from the input table and fill
  # with a placeholder so the level-by-level loop can proceed
  fixParent <- NULL
  for (i in seq_along(allCols)) {
    if (i == 1) next
    if (!allCols[i] %in% names(table)) {
      table <- add_column(table, !!allCols[i] := NA_character_,
                          .after = allCols[i - 1])
      fixParent <- c(fixParent, allCols[i])
    }
    table <- table |>
      mutate(!!allCols[i] := if_else(is.na(!!sym(allCols[i])), "___",
                                     !!sym(allCols[i])))
  }
  toOut <- table

  # handle sf input
  if (inherits(table, "sf")) {
    tab <- st_drop_geometry(table)
    remakeSF <- TRUE
  } else {
    tab <- table
    remakeSF <- FALSE
  }

  # --- level-by-level matching loop -----------------------------------------

  for (i in seq_along(allCols)) {

    # distinct combinations up to and including current level
    tempTab <- tab |>
      distinct(across(all_of(allCols[1:i]))) |>
      filter(!is.na(!!sym(tail(allCols[1:i], 1))))

    # apply simplify normalisation to the first column, keeping original for
    # joining back at the end
    tempTab <- tempTab |>
      mutate(orig_ = !!sym(allCols[i]),
             !!allCols[i] := !!sym(allCols[i]))

    if (!is.null(simplify)) {
      for (j in seq_along(simplify)) {
        nm <- names(simplify)[j]
        if (nm == "squish") {
          tempTab <- tempTab |> mutate(!!allCols[i] := str_squish(!!sym(allCols[i])))
        } else if (nm == "lowercase") {
          tempTab <- tempTab |> mutate(!!allCols[i] := str_to_lower(!!sym(allCols[i])))
        } else if (nm == "dashes") {
          tempTab <- tempTab |>
            mutate(!!allCols[i] := str_replace_all(!!sym(allCols[i]),
              c("\u2010|\u2011|\u2012|\u2013|\u2014|\u2015|\u2212|\ufe58|\ufe63|\uff0d|--" = "-")))
        } else if (nm == "duplpunct") {
          tempTab <- tempTab |>
            mutate(!!allCols[i] := str_replace_all(!!sym(allCols[i]), "([[:punct:]])\\1", "\\1"))
        } else if (nm == "remove") {
          tempTab <- tempTab |>
            mutate(!!allCols[i] := str_remove_all(!!sym(allCols[i]),
                                                   paste0(simplify[[j]], collapse = "|")))
        } else if (nm == "replace") {
          tempTab <- tempTab |>
            mutate(!!allCols[i] := str_replace_all(!!sym(allCols[i]),
                                                    simplify[[j]][1], simplify[[j]][2]))
        } else if (nm == "chartr") {
          tempTab <- tempTab |>
            mutate(!!allCols[i] := chartr(old = simplify[[j]][1],
                                           new = simplify[[j]][2],
                                           x = !!sym(allCols[i])))
        }
      }
    }

    # level_id of the current column (used for parent resolution)
    current_level_id <- levels$level_id[levels$label == allCols[i]]

    # --- case: missing intermediate level -----------------------------------
    # this level was not in the input table; inherit the canonical ID from the
    # previous level's result so children can be matched under the correct parent
    if (allCols[i] %in% fixParent) {

      tempConcepts <- tempTab |>
        left_join(newConcepts, by = allCols[1:(i - 1)]) |>
        select(label = allCols[i], external = allCols[i],
               parent_id = id) |>
        mutate(id = NA_character_)

    } else {

      # --- lookup in mappings table -----------------------------------------
      if (i == 1) {

        looked_up <- .lookup_mappings(
          labels     = tempTab[[allCols[i]]],
          vName      = ontology,
          dataseries = dataseries
        )
        isTop <- TRUE

      } else {

        # resolve canonical parent IDs from previous level's result
        tempTab2 <- tempTab |>
          left_join(newConcepts, by = allCols[1:(i - 1)]) |>
          select(label = allCols[i], parent_id = id)

        looked_up <- .lookup_mappings(
          labels     = tempTab2$label,
          vName      = ontology,
          dataseries = dataseries,
          parent_id  = tempTab2$parent_id
        )

        isTop <- !is.null(fixParent) && fixParent == allCols[i - 1]

      }

      matches   <- looked_up |> filter(!is.na(canonical_id))
      resolved  <- looked_up |> filter(is.na(canonical_id) & note %in% c("ignore", "new"))
      toMatch   <- looked_up |> filter(is.na(canonical_id) & !note %in% c("ignore", "new"))

      # --- case: same-level different-parent recovery -----------------------
      # term exists in mappings under a sibling parent at the correct level;
      # use it and record the reassignment rather than sending to match_builder
      diffParent <- NULL
      if (nrow(toMatch) > 0) {

        diffParent <- .lookup_mappings(
          labels     = toMatch$source_label,
          vName      = ontology,
          dataseries = dataseries
          # no parent_id constraint — search across all parents at this level
        ) |>
          filter(!is.na(canonical_id)) |>
          left_join(
            .read_terms(ontology) |>
              select(canonical_id = id, term_level = class),
            by = "canonical_id"
          ) |>
          filter(term_level == allCols[i])

        if (nrow(diffParent) > 0) {
          toMatch  <- toMatch  |> anti_join(diffParent, by = "source_label")
          matches  <- matches  |> bind_rows(diffParent |> select(source_label, canonical_id, note))
        } else {
          diffParent <- NULL
        }
      }

      # --- case: genuinely unmatched — queue for match_builder --------------
      if (nrow(toMatch) > 0) {

        toMatch <- toMatch |> mutate(class = allCols[i])

        newMatches <- match_builder(
          new        = toMatch,
          topLevel   = isTop,
          source     = dataseries,
          ontology   = ontology,
          stringdist = stringdist
        )

        confirmed <- newMatches |>
          filter(!is.na(canonical_id))

        newly_resolved <- newMatches |>
          filter(is.na(canonical_id) & note %in% c("ignore", "new"))

        if (nrow(confirmed) > 0) {
          .write_mappings(ontology, confirmed |>
                            mutate(source = dataseries) |>
                            select(source_label, source, canonical_id, parent_id, note))
        }

        if (nrow(newly_resolved) > 0) {
          .write_mappings(ontology, newly_resolved |>
                            mutate(source = dataseries) |>
                            select(source_label, source, canonical_id, parent_id, note))
        }

        new_territories <- bind_rows(resolved, newly_resolved) |>
          filter(note == "new")

        tempConcepts <- matches |>
          select(source_label, parent_id, canonical_id, note) |>
          bind_rows(confirmed |> select(source_label, parent_id, canonical_id, note)) |>
          bind_rows(new_territories |> select(source_label, parent_id, canonical_id, note)) |>
          arrange(source_label)

      } else {
        new_territories <- resolved |> filter(note == "new")

        tempConcepts <- matches |>
          select(source_label, parent_id, canonical_id, note) |>
          bind_rows(new_territories |> select(source_label, parent_id, canonical_id, note)) |>
          arrange(source_label)
      }

      # join canonical labels onto tempConcepts for output reconstruction
      terms <- .read_terms(ontology)
      tempConcepts <- tempConcepts |>
        left_join(terms |> select(id, label, class, parent_id),
                  by = c("canonical_id" = "id")) |>
        mutate(external = source_label,
               label = if_else(is.na(label), source_label, label),
               class = if_else(is.na(class), allCols[i], class),
               is_new = is.na(canonical_id) & !is.na(note) & note == "new",
               has_broader = if_else(is_new, parent_id.x, parent_id.y)) |>
        rename(id = canonical_id) |>
        select(id, label, class, has_broader, external, is_new)

      # --- different-parent: fix parent chain in newConcepts ----------------
      if (i != 1 && !strictMatch && !is.null(diffParent)) {

        # walk ancestors of the actual canonical parent to fill in the
        # parent columns that the input table was missing
        fixedParents <- diffParent |>
          select(canonical_id) |>
          distinct() |>
          left_join(terms |> select(id, parent_id), by = c("canonical_id" = "id")) |>
          rename(has_new_broader = canonical_id, has_broader = parent_id)

        # rebuild ancestor labels for each fixed parent up to level i-1
        ancestor_labels <- lapply(fixedParents$has_new_broader, function(cid) {
          ancs <- .get_ancestors(cid, ontology)
          # trim to the levels we care about (allCols[1:(i-1)])
          n <- length(allCols[1:(i - 1)])
          if (length(ancs) >= n) tail(ancs, n) else c(rep(NA_character_, n - length(ancs)), ancs)
        })
        ancestor_df <- as.data.frame(do.call(rbind, ancestor_labels),
                                     stringsAsFactors = FALSE)
        names(ancestor_df) <- allCols[1:(i - 1)]
        ancestor_df$has_new_broader <- fixedParents$has_new_broader
        ancestor_df$has_broader     <- fixedParents$has_broader

        ancestor_df <- ancestor_df |>
          unite(col = "new_label", any_of(allCols[1:(i - 1)]),
                sep = "][", remove = FALSE)

        newConcepts <- diffParent |>
          left_join(ancestor_df, by = c("canonical_id" = "has_new_broader")) |>
          select(id = canonical_id, has_broader, new_label,
                 any_of(allCols)) |>
          distinct() |>
          bind_rows(newConcepts)
      }

    }

    # restore original (pre-simplify) label for output
    if (!is.null(simplify) && nrow(tempConcepts) > 0) {
      orig_map <- tempTab |> select(simplified = allCols[i], orig = orig_)
      tempConcepts <- tempConcepts |>
        left_join(orig_map, by = c("external" = "simplified")) |>
        mutate(external = if_else(!is.na(orig), orig, external)) |>
        select(-orig)
    }

    # accumulate results level by level
    if (i == 1) {
      newConcepts <- tempConcepts |>
        rename(!!allCols[i] := external, new_label = label)
    } else {
      newConcepts <- tempConcepts |>
        rename(!!allCols[i] := external) |>
        left_join(
          newConcepts |> select(has_broader = id, any_of(allCols), new_label, is_new) |> distinct(),
          by = "has_broader"
        ) |>
        mutate(is_new = if_else(is_new.x | (!is.na(is_new.y) & is_new.y), TRUE, FALSE)) |>
        select(-is_new.x, -is_new.y) |>
        unite(col = "new_label", new_label, label, sep = "][", remove = TRUE)
    }

  }

  # --- join results back onto input table -----------------------------------

  toOut <- table |>
    select(-any_of("id")) |>
    unite(col = "external", all_of(allCols), sep = "][", remove = FALSE) |>
    select(-allCols[!allCols %in% columns]) |>
    left_join(newConcepts, by = columns, relationship = "many-to-many") |>
    select(-all_of(columns), -allCols[!allCols %in% columns]) |>
    separate_wider_delim(cols = new_label, delim = "][", names = allCols) |>
    mutate(match = if_else(!is.na(id), "close",
                           if_else(!is.na(is_new) & is_new, "new", NA_character_))) |>
    select(-any_of("is_new"))

  if (remakeSF) toOut <- st_sf(toOut)

  if (groupMatches) {
    matchCols <- c(allCols, "id", "match", "external", "has_broader", "class")
    toOut <- toOut |>
      group_by(across(-any_of(matchCols))) |>
      summarise(across(.cols = any_of(matchCols),
                       .fns  = ~paste0(.x, collapse = " | ")),
                .groups = "drop")
  }

  out <- toOut |>
    select(all_of(allCols), id, match, external, has_broader, class, everything())

  return(out)

}

#' Read the levels table for a vocabulary
#' @param vName [`character(1)`][character]\cr vocabulary name
#' @return tibble with columns: level_id, label, parent_level
#' @importFrom arrow read_parquet
#' @importFrom tibble as_tibble

.read_levels <- function(vName) {
  as_tibble(read_parquet(
    file.path(.adb_state$path, "vocabularies", "stage3",
              paste0(vName, "_levels.parquet")),
    mmap = FALSE))
}


#' Read the terms table for a vocabulary
#' @param vName [`character(1)`][character]\cr vocabulary name
#' @return tibble with columns: id, label, class, parent_id, description,
#'   valid_from, valid_until, successor_id
#' @importFrom arrow read_parquet
#' @importFrom tibble as_tibble

.read_terms <- function(vName) {
  read_parquet(
    file.path(.adb_state$path, "vocabularies", "stage3",
              paste0(vName, "_terms.parquet")),
    mmap = FALSE)
}


#' Read the mappings table for one dataseries against one vocabulary
#' @param vName [`character(1)`][character]\cr vocabulary name
#' @param dataseries [`character(1)`][character]\cr dataseries name
#' @return tibble with columns: source_label, source, canonical_id, note
#'   or an empty tibble with those columns if the file doesn't exist
#' @importFrom arrow read_parquet
#' @importFrom tibble tibble as_tibble
#' @importFrom readr col_character

.read_mappings <- function(vName, dataseries) {
  f <- file.path(.adb_state$path, "vocabularies", "mappings",
                 paste0(vName, "_", dataseries, ".csv"))
  if (!file.exists(f)) {
    return(tibble(source_label = character(), source = character(),
                  canonical_id = character(), parent_id = character(),
                  note = character()))
  }
  out <- read_csv(f, col_types = cols(.default = col_character()), show_col_types = FALSE)
  if (!"parent_id" %in% names(out))
    out$parent_id <- NA_character_
  core <- c("source_label", "source", "canonical_id", "parent_id", "note")
  extras <- setdiff(names(out), core)
  out[, c(core, extras)]
}


#' Read all mappings files for one vocabulary
#' @param vName [`character(1)`][character]\cr vocabulary name
#' @return named list of tibbles, one per dataseries
#' @importFrom arrow read_parquet
#' @importFrom tibble as_tibble
#' @importFrom stats setNames

.read_all_mappings <- function(vName) {
  dir <- file.path(.adb_state$path, "vocabularies", "mappings")
  prefix <- paste0("^", vName, "_")
  files <- list.files(dir, pattern = paste0(prefix, ".*[.]csv$"),
                      full.names = TRUE)
  nms <- sub(prefix, "", sub("[.]csv$", "", basename(files)))
  out <- lapply(files, function(f) as_tibble(read_csv(f, show_col_types = FALSE)))
  setNames(out, nms)
}


#' Append new rows to a mappings file, ignoring duplicates
#'
#' Required columns in \code{new_rows}: \code{source_label}, \code{source},
#' \code{canonical_id}. Any additional columns (e.g. source-specific codes,
#' descriptions, alternative labels) are preserved in the parquet output.
#' Different writes can carry different extra columns; missing extras become
#' NA via \code{bind_rows}.
#'
#' @param vName [`character(1)`][character]\cr vocabulary name
#' @param new_rows tibble; see Description for required columns
#' @importFrom arrow write_parquet
#' @importFrom dplyr bind_rows distinct arrange

.write_mappings <- function(vName, new_rows) {
  dataseries <- unique(new_rows$source)
  if (length(dataseries) != 1) {
    stop("new_rows must contain exactly one dataseries in the 'source' column.")
  }
  existing <- .read_mappings(vName, dataseries)
  # replace any note="new" placeholder rows for labels that now have a real canonical_id
  resolved_labels <- new_rows$source_label[!is.na(new_rows$canonical_id)]
  if (length(resolved_labels) > 0) {
    existing <- existing |>
      filter(!(source_label %in% resolved_labels & note == "new"))
  }
  out <- bind_rows(existing, new_rows) |>
    distinct() |>
    arrange(canonical_id, source_label)
  write_csv(out,
            file.path(.adb_state$path, "vocabularies", "mappings",
                      paste0(vName, "_", dataseries, ".csv")))
  invisible(out)
}


#' Append new terms to <vName>_terms.parquet, ignoring duplicates
#' @param vName [`character(1)`][character]\cr vocabulary name
#' @param new_rows tibble with columns matching the terms schema
#' @importFrom arrow write_parquet
#' @importFrom dplyr bind_rows distinct arrange

.write_terms <- function(vName, new_rows) {
  existing <- .read_terms(vName)
  out <- bind_rows(existing, new_rows) |>
    distinct() |>
    arrange(id)
  write_parquet(out,
                file.path(.adb_state$path, "vocabularies", "stage3",
                          paste0(vName, "_terms.parquet")))
  invisible(out)
}


#' Look up canonical IDs for a set of source labels
#'
#' Primary lookup used by .matchOntology. Returns the canonical_id for each
#' source_label, or NA if not yet mapped.
#'
#' @param labels [`character(.)`][character]\cr source labels to look up
#' @param vName [`character(1)`][character]\cr vocabulary name
#' @param dataseries [`character(1)`][character]\cr dataseries name
#' @param parent_id [`character(.)`][character]\cr optional canonical parent IDs
#'   to restrict the lookup (same length as labels, or NULL)
#' @return tibble with columns: source_label, canonical_id, note
#' @importFrom dplyr left_join select filter
#' @importFrom tibble tibble

.lookup_mappings <- function(labels, vName, dataseries, parent_id = NULL) {
  mappings <- .read_mappings(vName, dataseries)
  terms    <- .read_terms(vName)

  query <- tibble(source_label = labels,
                  parent_id    = if (is.null(parent_id)) NA_character_ else parent_id)

  if (is.null(parent_id)) {
    result <- query |>
      left_join(mappings |> select(source_label, canonical_id, note),
                by = "source_label")
  } else {
    # restrict to mappings whose canonical concept has the expected parent;
    # rows with NA canonical_id (note="new"/"ignore") match on source_label only
    mappings_with_parent <- mappings |>
      filter(!is.na(canonical_id)) |>
      left_join(terms |> select(canonical_id = id, term_parent = parent_id),
                by = "canonical_id")

    mappings_na <- mappings |>
      filter(is.na(canonical_id))

    result <- query |>
      left_join(mappings_with_parent |>
                  select(source_label, canonical_id, note, term_parent),
                by = c("source_label", "parent_id" = "term_parent")) |>
      left_join(mappings_na |> select(source_label, parent_id, canonical_id_na = canonical_id, note_na = note),
                by = c("source_label", "parent_id")) |>
      mutate(canonical_id = if_else(is.na(canonical_id), canonical_id_na, canonical_id),
             note         = if_else(is.na(note), note_na, note)) |>
      select(-canonical_id_na, -note_na)
  }

  result
}


#' Get the children of a canonical concept
#'
#' Used to build the candidate set for fuzzy matching within a resolved parent.
#'
#' @param parent_id [`character(1)`][character]\cr canonical ID of the parent
#' @param vName [`character(1)`][character]\cr vocabulary name
#' @return tibble with columns: id, label, class
#' @importFrom dplyr filter select

.get_children <- function(parent_id, vName) {
  terms <- .read_terms(vName)
  terms |>
    filter(parent_id %in% !!parent_id) |>
    select(id, label, class)
}


#' Get the full ancestor chain for a canonical concept
#'
#' Used to build parent_context display in match_builder.
#'
#' @param id [`character(1)`][character]\cr canonical ID
#' @param vName [`character(1)`][character]\cr vocabulary name
#' @return character vector of labels from root to parent (not including id itself)
#' @importFrom dplyr filter pull

.get_ancestors <- function(id, vName) {
  terms <- .read_terms(vName)
  ancestors <- character()
  current <- terms$parent_id[terms$id == id]
  while (length(current) == 1 && !is.na(current)) {
    row <- terms[terms$id == current, ]
    ancestors <- c(row$label, ancestors)
    current <- row$parent_id
  }
  ancestors
}

