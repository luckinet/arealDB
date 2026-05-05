#' Normalise registered vocabulary contributors
#'
#' Processes vocabulary stage 2 files into stage 3. The first contributor to a
#' given vocabulary becomes its \emph{backbone}: \code{<vName>_terms.parquet}
#' and \code{<vName>_levels.parquet} are written from its reshaped contents,
#' with canonical IDs auto-generated as dot-paths. Every subsequent
#' contributor is treated as a \emph{mapping}: its labels are looked up
#' against the existing backbone (with \code{\link{match_builder}} for any
#' unmatched labels) and persisted as
#' \code{<vName>_<dName>.parquet} in \code{vocabularies/mappings/}.
#'
#' @section Input table formats:
#' The table produced by \code{\link[tabshiftr]{reorganise}} must contain a
#' \code{label} column and must be in one of two forms:
#'
#' \strong{Term-grain} (one row per term at any hierarchy level): requires
#' columns \code{label}, \code{class}, and \code{parent_label}. Optional
#' columns \code{description}, \code{valid_from}, \code{valid_until}, and
#' \code{successor_id} are carried through. Any further columns are written
#' to the mapping file as extras.
#'
#' \strong{Leaf-grain} (one row per leaf node, ancestor terms embedded as
#' columns): do not include \code{label} or \code{class} columns. Instead,
#' name every column after the class it represents (e.g. \code{global},
#' \code{region}, \code{country}), ordered left-to-right from the broadest
#' to the finest level. The function pivots to long form internally: column
#' names become \code{class}, values become \code{label}, and
#' \code{parent_label} is derived from column order. Only the backbone branch
#' supports leaf-grain input; mapping contributors must supply term-grain.
#'
#' @param input [`character(1)`][character]\cr path of a single stage 2
#'   vocabulary file. If \code{NULL} (default), every file in
#'   \code{vocabularies/stage2/} matching \code{pattern} is processed.
#' @param pattern [`character(1)`][character]\cr optional regular expression
#'   restricting which files are processed.
#' @param beep [`integerish(1)`][integer]\cr optional sound code passed to
#'   \code{\link[beepr]{beep}} when interactive matching is required.
#' @param verbose [`logical(1)`][logical]\cr be verbose about matching.
#' @return Invisibly returns the rows of \code{inv_vocabulary_files} that
#'   were processed.
#' @family normalise functions
#' @importFrom arrow read_parquet write_parquet
#' @importFrom checkmate assertFileExists assertCharacter assertLogical
#'   assertIntegerish
#' @importFrom dplyr arrange bind_cols bind_rows distinct filter group_by
#'   left_join mutate pull row_number select transmute ungroup lag
#' @importFrom stringr str_pad str_split
#' @importFrom readr read_csv cols
#' @importFrom tabshiftr reorganise
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of any_of where
#' @importFrom beepr beep
#' @export

normVocabulary <- function(input = NULL, pattern = NULL,
                           beep = NULL, verbose = FALSE){

  # library(checkmate); intPaths <- arealDB:::.adb_state$path; input = NULL; pattern = "gazetteer__unsd"; beep = NULL; verbose = FALSE

  intPaths <- .adb_state$path

  assertCharacter(x = pattern, len = 1, null.ok = TRUE)
  assertIntegerish(x = beep, len = 1, lower = 1, upper = 11, null.ok = TRUE)
  assertLogical(x = verbose, len = 1)

  if(is.null(input)){
    input <- list.files(path = paste0(intPaths, "/vocabularies/stage2"),
                        full.names = TRUE, pattern = pattern)
    input <- input[!file.info(input)$isdir]
  } else {
    assertFileExists(x = input, access = "r")
  }

  inventory <- readRDS(paste0(intPaths, "/inventory.rds"))
  inv_vocabularies <- inventory$vocabularies
  assertNames(x = colnames(inv_vocabularies),
              permutation.of = c("vocID", "datID", "name", "description", "version", "licence_link", "stage2_name", "schema", "stage1_name", "stage1_url", "download_date", "status", "notes"))

  ret <- NULL

  for(i in seq_along(input)){

    thisInput <- input[i]
    file_name <- basename(thisInput)

    # parse <vName>__<dName>.csv
    stem <- sub("[.]csv$", "", file_name)
    parts <- str_split(stem, "__")[[1]]
    if(length(parts) != 2){
      message("\n--- ", i, " / ", length(input),
              " skipping (unrecognised filename) ", file_name, " ---")
      next
    }
    vName <- parts[1]
    dName <- parts[2]

    if(!file_name %in% inv_vocabularies$stage2_name){
      message("\n--- ", i, " / ", length(input),
              " skipping (not registered) ", file_name, " ---")
      next
    }

    termsExists   <- file.exists(file.path(intPaths, "vocabularies", "stage3",
                                           paste0(vName, "_terms.parquet")))
    mappingExists <- file.exists(file.path(intPaths, "vocabularies", "mappings",
                                           paste0(vName, "_", dName, ".parquet"))) ||
                     file.exists(file.path(intPaths, "vocabularies", "mappings",
                                           paste0(vName, "_", dName, ".csv")))
    alreadyDone <- inv_vocabularies$status[inv_vocabularies$stage2_name == file_name] == "normalised" ||
                   (termsExists && mappingExists)
    if(alreadyDone){
      message("\n--- ", i, " / ", length(input),
              " skipping (already normalised) ", file_name, " ---")
      next
    }

    message("\n--- ", i, " / ", length(input), " ", file_name, " ---")

    # resolve schema
    voc_row <- inv_vocabularies[inv_vocabularies$stage2_name == file_name, ]
    schemaName <- voc_row$schema
    schema <- readRDS(paste0(intPaths, "/vocabularies/schemas/", schemaName, ".rds"))

    # read CSV → coerce all to character (stage 2 contract)
    message("--> reading stage 2 file ...")
    temp <- read.csv(file = thisInput, header = FALSE, strip.white = TRUE, as.is = TRUE, colClasses = "character", na.strings = schema@format$na, encoding = "UTF-8") %>%
      as_tibble() %>%
      mutate(across(where(is.character), ~na_if(x = ., y = "")))

    message("    reorganising with '", schemaName, "' ...")
    reshaped <- temp |> reorganise(schema = schema)

    has_label  <- "label"        %in% names(reshaped)
    has_class  <- "class"        %in% names(reshaped)
    has_parent <- "parent_label" %in% names(reshaped)

    if(has_label && has_class && !has_parent){
      stop("reorganised table has 'label' and 'class' but is missing 'parent_label'.")
    }

    if(!has_label && !has_class){
      # leaf-grain: all columns are hierarchy levels ordered left (broadest) to
      # right (finest). Column names become 'class', values become 'label',
      # parent_label is derived from the preceding level via lag(), computed
      # per original row so each term gets the correct parent.
      lvl_order <- names(reshaped)

      reshaped <- reshaped |>
        mutate(.row = row_number()) |>
        pivot_longer(
          cols      = all_of(lvl_order),
          names_to  = "class",
          values_to = "label"
        ) |>
        mutate(class = factor(class, levels = lvl_order)) |>
        arrange(.row, class) |>
        group_by(.row) |>
        mutate(parent_label = lag(label)) |>
        ungroup() |>
        mutate(class = as.character(class)) |>
        select(-".row") |>
        distinct(label, class, .keep_all = TRUE) |>
        filter(!is.na(label))
    } else if(!has_label){
      stop("reorganised table is missing required column 'label'.")
    }

    # determine role: backbone if no terms file yet (or empty)
    termsFile <- file.path(intPaths, "vocabularies", "stage3",
                           paste0(vName, "_terms.parquet"))
    levelsFile <- file.path(intPaths, "vocabularies", "stage3",
                            paste0(vName, "_levels.parquet"))
    isBackbone <- !file.exists(termsFile) ||
                  nrow(read_parquet(termsFile, mmap = FALSE)) == 0

    if(isBackbone){

      # ----- BACKBONE BRANCH -----
      message("    -> building a new backbone ...")

      required <- c("label", "class", "parent_label")
      missing  <- setdiff(required, names(reshaped))
      if(length(missing) > 0){
        stop("backbone vocabulary is missing required column(s): ",
             paste(missing, collapse = ", "))
      }

      # build levels: derive (class, parent_class) by joining each row's
      # parent_label back to the table to get the parent's class
      levels <- reshaped |>
        distinct(class, parent_label) |>
        left_join(reshaped |> select(label, parent_class = class),
                  by = c("parent_label" = "label")) |>
        distinct(class, parent_class) |>
        filter(!is.na(class)) |>
        group_by(class) |>
        mutate(parent_class = parent_class[!is.na(parent_class)][1]) |>
        distinct() |>
        ungroup()

      # walk the class tree top-down to order levels
      walked <- character()
      remaining <- levels
      while(nrow(remaining) > 0){
        next_top <- remaining |>
          filter(is.na(parent_class) | parent_class %in% walked) |>
          pull(class)
        if(length(next_top) == 0){
          stop("vocabulary levels do not form a tree (cycle or orphan).")
        }
        walked <- c(walked, next_top)
        remaining <- remaining |> filter(!class %in% walked)
      }

      levels_out <- tibble(
        level_id = seq_along(walked),
        label = walked,
        parent_level = vapply(walked, function(cl){
          pc <- levels$parent_class[levels$class == cl]
          if(length(pc) == 0 || is.na(pc)) NA_character_ else pc
        }, character(1)))

      # append any sub-national levels declared in db_info$level that the
      # backbone vocabulary doesn't define (e.g. ADM1, ADM2 are country-specific
      # and will never appear in a global gazetteer backbone)
      db_meta <- tryCatch({
        e <- new.env(); load(file.path(intPaths, "db_info.RData"), envir = e); e$db_info
      }, error = function(e) NULL)
      if (!is.null(db_meta) && length(db_meta$level) > 1) {
        extra_levels <- db_meta$level[!db_meta$level %in% levels_out$label]
        if (length(extra_levels) > 0) {
          all_levels <- c(levels_out$label, extra_levels)
          extra_rows <- tibble(
            level_id    = seq(nrow(levels_out) + 1, nrow(levels_out) + length(extra_levels)),
            label       = extra_levels,
            parent_level = vapply(extra_levels, function(lv) {
              pos <- match(lv, all_levels)
              if (pos > 1) all_levels[pos - 1] else NA_character_
            }, character(1))
          )
          levels_out <- bind_rows(levels_out, extra_rows)
        }
      }

      write_parquet(levels_out, levelsFile)

      # build terms with auto-generated dot-path IDs, top-down
      terms_acc <- tibble(id = character(), label = character(),
                          class = character(), parent_id = character(),
                          description = character(), valid_from = character(),
                          valid_until = character(), successor_id = character())

      for(cl in walked){
        rows <- reshaped |>
          filter(class == cl) |>
          distinct(label, parent_label, .keep_all = TRUE) |>
          arrange(label)

        if(nrow(terms_acc) == 0){
          rows <- rows |> mutate(parent_id = NA_character_)
        } else {
          rows <- rows |>
            left_join(terms_acc |> select(parent_label = label, parent_id = id),
                      by = "parent_label")
        }

        rows <- rows |>
          group_by(parent_id) |>
          mutate(suffix = str_pad(row_number(), width = 3, pad = "0")) |>
          ungroup() |>
          mutate(id = ifelse(is.na(parent_id),
                             paste0(".", suffix),
                             paste0(parent_id, ".", suffix)))

        new_terms <- tibble(
          id = rows$id,
          label = rows$label,
          class = cl,
          parent_id = rows$parent_id,
          description  = if("description"  %in% names(rows)) rows$description  else NA_character_,
          valid_from   = if("valid_from"   %in% names(rows)) rows$valid_from   else NA_character_,
          valid_until  = if("valid_until"  %in% names(rows)) rows$valid_until  else NA_character_,
          successor_id = if("successor_id" %in% names(rows)) rows$successor_id else NA_character_)

        terms_acc <- bind_rows(terms_acc, new_terms)
      }

      write_parquet(terms_acc, termsFile)

      # also record the backbone contributor's own labels as a mapping
      joined <- reshaped |>
        distinct() |>
        left_join(terms_acc |> select(label, class, canonical_id = id),
                  by = c("label", "class"))

      mappings_out <- tibble(source_label = joined$label,
                             source       = dName,
                             canonical_id = joined$canonical_id,
                             note         = NA_character_)
      extras <- setdiff(names(joined),
                        c("label", "class", "parent_label", "description",
                          "valid_from", "valid_until", "successor_id",
                          "canonical_id"))
      if(length(extras) > 0){
        mappings_out <- bind_cols(mappings_out, joined[, extras, drop = FALSE])
      }
      write_csv(mappings_out, na = "",
                file.path(intPaths, "vocabularies", "mappings",
                          paste0(vName, "_", dName, ".csv")))

    } else {

      # ----- MAPPING BRANCH -----
      message("    -> matching against existing backbone ...")

      if(!"source_label" %in% names(reshaped)){
        stop("mapping vocabulary is missing required column 'source_label'.")
      }
      extras <- setdiff(names(reshaped), "source_label")

      # try existing mappings first (so re-runs / partial fills work)
      looked_up <- .lookup_mappings(labels     = reshaped$source_label,
                                    vName      = vName,
                                    dataseries = dName)
      reshaped <- reshaped |>
        mutate(canonical_id = looked_up$canonical_id)

      unmatched_idx <- which(is.na(reshaped$canonical_id))
      if(length(unmatched_idx) > 0){
        message("    ", length(unmatched_idx), " label(s) need matching ...")
        new_in <- tibble(source_label = reshaped$source_label[unmatched_idx],
                         parent_id    = NA_character_,
                         canonical_id = NA_character_,
                         class        = vName,
                         note         = NA_character_)
        if(length(extras) > 0){
          new_in <- bind_cols(new_in,
                              reshaped[unmatched_idx, extras, drop = FALSE])
        }
        resolved <- match_builder(new        = new_in,
                                  topLevel   = TRUE,
                                  source     = dName,
                                  ontology   = vName,
                                  stringdist = TRUE)
        if(!is.null(resolved) && nrow(resolved) > 0){
          # write ignore/new decisions to mappings so user isn't re-prompted
          resolved_notes <- resolved |>
            filter(is.na(canonical_id) & note %in% c("ignore", "new"))
          if (nrow(resolved_notes) > 0) {
            .write_mappings(vName, resolved_notes |>
                              mutate(source = dName) |>
                              select(source_label, source, canonical_id, parent_id, note))
          }

          lookup <- resolved |>
            filter(!is.na(canonical_id)) |>
            select(source_label, canonical_id_resolved = canonical_id)
          reshaped <- reshaped |>
            left_join(lookup, by = "source_label") |>
            mutate(canonical_id = ifelse(is.na(canonical_id),
                                         canonical_id_resolved,
                                         canonical_id)) |>
            select(-canonical_id_resolved)
        }
      }

      mappings_out <- tibble(source_label = reshaped$source_label,
                             source       = dName,
                             canonical_id = reshaped$canonical_id,
                             note         = NA_character_)
      if(length(extras) > 0){
        mappings_out <- bind_cols(mappings_out, reshaped[, extras, drop = FALSE])
      }
      mappings_out <- distinct(mappings_out)

      write_parquet(mappings_out,
                    file.path(intPaths, "vocabularies", "mappings",
                              paste0(vName, "_", dName, ".parquet")))
    }

    # mark this contributor as normalised in the inventory
    inventory$vocabularies$status[inventory$vocabularies$stage2_name == file_name] <- "normalised"
    saveRDS(object = inventory, file = paste0(intPaths, "/inventory.rds"))

    ret <- bind_rows(ret, voc_row)
  }

  if(!is.null(beep)) beep(beep)
  invisible(ret)
}
