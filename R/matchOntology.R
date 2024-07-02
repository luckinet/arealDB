#' Match target terms with an ontology
#'
#' This function takes a table to replace the values of various columns with
#' harmonised values listed in the project specific gazetteer.
#' @param table [`data.frame(1)`][data.frame]\cr a table that contains columns
#'   that should be harmonised by matching with the gazetteer.
#' @param columns [`character(1)`][character]\cr the columns containing the
#'   concepts
#' @param dataseries [`character(1)`][character]\cr the source dataseries from
#'   which territories are sourced.
#' @param ontology [\code{onto}]\cr path where the ontology/gazetteer is stored.
#' @param beep [`integerish(1)`][integer]\cr Number specifying what sound to be
#'   played to signal the user that a point of interaction is reached by the
#'   program, see \code{\link[beepr]{beep}}.
#' @param colsAsClass [`logical(1)`][logical]\cr whether to match \code{columns}
#'   by their name with the respective classes, or with concepts of all classes.
#' @param groupMatches [`logical(1)`][logical]\cr whether or not to group
#'   harmonized concepts when there are more than one match (for example for
#'   broader or narrower matches).
#' @param stringdist [`logical(1)`][logical]\cr whether or not to use string
#'   distance to find matches (should not be used for large datasets/when a
#'   memory error is shown).
#' @param strictMatch [`logical(1)`][logical]\cr whether or not matches are
#'   strict, i.e., there should be clear one-to-one relationships and no changes
#'   in broader concepts.
#' @param verbose [`logical(1)`][logical]\cr whether or not to give detailed
#'   information on the process of this function.
#' @return Returns a table that resembles the input table where the target
#'   columns were translated according to the provided ontology.
#' @importFrom checkmate assertDataFrame assertCharacter assertIntegerish
#'   assertLogical
#' @importFrom utils head
#' @importFrom ontologics load_ontology new_source get_concept new_mapping
#'   make_tree
#' @importFrom purrr map_dfr
#' @importFrom dplyr pull filter select mutate distinct bind_cols rename
#'   everything left_join rename_with na_if
#' @importFrom tibble tibble
#' @importFrom tidyselect all_of any_of where
#' @importFrom stringr str_split_i str_replace str_replace_all
#' @importFrom tidyr separate_rows separate pivot_wider fill pivot_longer
#'   separate_longer_delim separate_wider_delim contains
#' @importFrom sf st_drop_geometry
#' @export

matchOntology <- function(table = NULL, columns = NULL, dataseries = NULL,
                          ontology = NULL, beep = NULL, colsAsClass = TRUE,
                          groupMatches = FALSE, stringdist = TRUE, strictMatch = FALSE,
                          verbose = FALSE){

  # table = thisTable; columns = targetCols; dataseries = dSeries; ontology = gazPath; colsAsClass = TRUE; groupMatches = FALSE; stringdist = TRUE; strictMatch = FALSE

  assertDataFrame(x = table, min.cols = length(columns))
  assertCharacter(x = columns, any.missing = FALSE)
  assertCharacter(x = dataseries, len = 1, any.missing = FALSE)
  assertIntegerish(x = beep, len = 1, lower = 1, upper = 11, null.ok = TRUE)
  assertLogical(x = colsAsClass, len = 1, any.missing = FALSE)
  assertLogical(x = groupMatches, len = 1, any.missing = FALSE)
  assertLogical(x = verbose, len = 1, any.missing = FALSE)

  # set internal paths
  ontoPath <- ontology
  ontoMatching <- str_split(string = ontoPath, pattern = "[.]")[[1]][1]
  if(!testDirectoryExists(ontoMatching)){
    dir.create(ontoMatching)
  }

  allCols <- get_class(ontology = ontoPath) %>%
    pull(label)

  if(colsAsClass){
    assertSubset(x = head(columns, 1), choices = allCols)
    allCols <- allCols[which(allCols %in% head(columns, 1)) : which(allCols %in% tail(columns, 1))]
    withClass <- "class"
  } else {
    allCols <- columns
    withClass <- NULL
  }

  # remove white-space and dots
  table <- table %>%
    mutate(across(where(is.character),
                  function(x){
                    temp <- trimws(x)
                    str_replace_all(string = temp, pattern = "[.]", replacement = "")
                  }))

  # fill from left to right
  fixParent <- NULL
  for(i in seq_along(allCols)){
    if(i == 1) next
    if(!allCols[i] %in% names(table)){
      table <- add_column(.data = table, !!allCols[i] := NA_character_, .after = allCols[i-1])
      fixParent <- c(fixParent, allCols[i])
    }
    table <- table %>%
      mutate(!!allCols[i] := if_else(is.na(!!sym(allCols[i])), !!sym(allCols[i-1]), !!sym(allCols[i])))
  }
  toOut <- table

  type <- str_split(tail(str_split(string = ontoPath, pattern = "/")[[1]], 1), "[.]")[[1]][1]

  # make a new dataseries, in case it doesn't exist yet
  if(!dataseries %in% get_source(ontology = ontoPath)$label){
    new_source(name = dataseries, date = Sys.Date(),
               ontology = ontoPath)
  }

  # get the current source
  srcID <- get_source(label = dataseries, ontology = ontoPath) %>%
    pull(id)

  # prepare object to write into
  if(inherits(x = table, what = "sf")){
    tab <- table %>%
      st_drop_geometry()
    remakeSF <- TRUE
  } else {
    tab <- table
    remakeSF <- FALSE
  }

  for(i in seq_along(allCols)){

    # extract the target column and its parents
    tempTab <- tab %>%
      distinct(across(all_of(allCols[1:i]))) %>%
      filter(!is.na(!!sym(tail(allCols[1:i], 1))))

    if(allCols[i] %in% fixParent){

      tempConcepts <- tempTab %>%
        left_join(newConcepts, by = allCols[1:(i-1)]) %>%
        select(label = allCols[i], external = allCols[i], has_broader = id) |>
        mutate(id = NA_character_)

    } else {

      # identify whether concepts were already defined as external concepts...
      if(i == 1){

        tempTab <- tempTab %>%
          select(label = allCols[i])

        parentLen <- get_class(ontology = ontoPath) %>%
          filter(label == allCols[i]) %>%
          pull(id)

        if(length(parentLen) != 0){
          parentLen <- length(str_split(parentLen, "[.]")[[1]])-1
        } else {
          parentLen <- 0
        }

        externalConcepts <- get_concept(label = tempTab$label, has_source = srcID,
                                        external = TRUE, ontology = ontoPath) %>%
          mutate(len = lengths(str_split(has_broader, "[.]"))) %>%
          filter(len == parentLen) %>%
          select(-len) %>%
          left_join(tibble(label = tempTab$label), ., by = "label") %>%
          mutate(class = allCols[i],
                 has_source = srcID)

      } else {

        # first, transform the parents into the column 'has_broader'
        tempTab <- tempTab %>%
          left_join(newConcepts, by = allCols[1:(i-1)]) %>%
          select(label = allCols[i], has_broader = id)

        externalConcepts <- get_concept(label = tempTab$label, has_source = srcID,
                                        has_broader = tempTab$has_broader,
                                        external = TRUE, ontology = ontoPath) %>%
          left_join(tempTab |> select(label, has_broader), ., by = c("label", "has_broader")) %>%
          mutate(class = allCols[i],
                 has_source = srcID)

      }

      # if not all external concepts have an ID, edit the matches...
      toMatch <- externalConcepts %>%
        filter(is.na(id))
      matches <- externalConcepts %>%
        filter(!is.na(id))

      if(dim(toMatch)[1] != 0){

        newMatches <- edit_matches(new = toMatch,
                                   topLevel = if_else(i == 1, TRUE, FALSE),
                                   source = dataseries,
                                   ontology = ontology,
                                   matchDir = paste0(ontoMatching, "/"),
                                   stringdist = stringdist,
                                   verbose = verbose,
                                   beep = beep)

        if(i != 1){
          if(allCols[i-1] %in% fixParent){
            toMatch$has_broader <- "fix_parent"
            newConcepts$id <- "fix_parent"
          }
        }

        # if(!all(c("label", "class", "id", "description", "match", "external", "has_broader") %in% colnames(newMatches))){
        #   stop("some columns are missing in 'newMatches'")
        # }
        # if(dim(newMatches)[1] > dim(toMatch)[1]){
        #   stop("'newMatches' has not the number of elements as 'toMatch'")
        # }

        newMappings <- newMatches |>
          filter(!is.na(match))

        if(dim(newMappings)[1] != 0){
          new_mapping(new = newMappings$external,
                      target = newMappings %>% select(id, label, class, has_broader),
                      source = dataseries,
                      match = newMappings$match,
                      certainty = 3,
                      ontology = ontoPath,
                      verbose = verbose,
                      beep = beep)
        }

        # in case concepts were matched in another parent, those parents need to be corrected in 'newConcepts'
        if(i != 1 & !strictMatch){

          parentMappings <- newMappings |>
            rename(has_new_broader = has_broader) |>
            left_join(toMatch |> select(external = label, has_broader), by = c("external"))
          # separate_wider_regex(id, c(id_new = ".*", "[.]", rest = ".*"), cols_remove = FALSE) |>
          #   filter(has_broader == id_new) |>
          # this becomes really problematic when there are external concepts where the name is duplicated, but they are different territories, because it gives false joins

          if(any(parentMappings$has_new_broader != parentMappings$has_broader)){
            message("-------> new parents when matching <------- ")

            tempTab_broader <- make_tree(id = parentMappings$has_new_broader, ontology = ontoPath, reverse = TRUE) |>
              filter(class %in% allCols) |>
              pivot_wider(names_from = class, values_from = label) |>
              fill(allCols[1:(i-1)]) |>
              filter(if_any(allCols[i-1], ~ !is.na(.))) |>
              unite(col = "new_label", any_of(allCols), sep = "][", remove = FALSE) |>
              select(has_new_broader = id, new_label)

            newConcepts <- parentMappings |>
              left_join(tempTab_broader, by = "has_new_broader") |>
              left_join(newConcepts |> select(has_broader = id, any_of(allCols)) |> distinct(), by = "has_broader") |>
              select(id = has_new_broader, new_label, any_of(allCols)) |>
              distinct() |>
              bind_rows(newConcepts)

          }
        }

        tempConcepts <- matches |>
          select(label, has_broader) |>
          bind_rows(newMappings |> select(label = external, has_broader)) |>
          arrange(label)

      } else {

        tempConcepts <- matches |>
          select(label, has_broader) |>
          arrange(label)

      }

      # ... and query the ontology again, this should now include the newly created
      # concepts as well (except those that were to be ignored)

      if(i == 1){
        externalConcepts <- get_concept(label = tempConcepts$label, has_source = srcID,
                                        external = TRUE, ontology = ontoPath) %>%
          mutate(len = lengths(str_split(has_broader, "[.]"))) %>%
          filter(len == parentLen) |>
          left_join(tibble(label = tempConcepts$label), ., by = "label") %>%
          mutate(class = allCols[i])

      } else {
        externalConcepts <- get_concept(label = tempConcepts$label, has_source = srcID,
                                        has_broader = tempConcepts$has_broader,
                                        external = TRUE, ontology = ontoPath) %>%
          left_join(tempConcepts |> select(label, has_broader), ., by = c("label", "has_broader")) %>%
          mutate(class = allCols[i])
      }

      if(dim(tempConcepts)[1] != 0){

        tempConcepts <- get_concept(str_detect(has_close_match, paste0(externalConcepts$id, collapse = "|")) |
                                      str_detect(has_broader_match, paste0(externalConcepts$id, collapse = "|")) |
                                      str_detect(has_narrower_match, paste0(externalConcepts$id, collapse = "|")) |
                                      str_detect(has_exact_match, paste0(externalConcepts$id, collapse = "|")),
                                    ontology = ontoPath) %>%
          pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                       names_to = "match", values_to = "external") %>%
          separate_longer_delim(cols = external, delim = " | ") %>%
          filter(!is.na(external)) %>%
          mutate(externalID = str_split_i(external, "[.]", 1),
                 match = str_replace(string = match, pattern = "has_", replacement = ""),
                 match = str_replace(string = match, pattern = "_match", replacement = "")) %>%
          select(-external) %>%
          distinct() %>%
          left_join(externalConcepts %>% select(externalID = id, external = label), by = "externalID") %>%
          filter(!is.na(external)) %>%
          filter(match != "exact") %>%
          select(-externalID)

      } else {

        tempConcepts <- tibble(id = character(), label = character(), description = character(), class = character(),
                               has_broader = character(), match = character(), external = character())

      }

    }

    # fix the parent of the current level
    # if(i != 1){
    #   if(allCols[i-1] %in% fixParent){
    #     theseConcepts <- tempConcepts %>%
    #       filter(external %in% unique(table[[allCols[i]]])) %>%
    #       select(external, id = has_broader)
    #     parentConcepts <- newConcepts %>% # probably this also needs to have  |> distinct()
    #       select(id, allCols[i-1]) %>%
    #       left_join(theseConcepts, ., by = "id") %>%
    #       select(!!allCols[i] := external, allCols[i-1])
    #
    #     table <- table %>%
    #       select(-allCols[i-1]) %>%
    #       left_join(parentConcepts, by = allCols[i]) %>%
    #       select(all_of(allCols), everything())
    #   }
    # }

    if(i == 1){
      newConcepts <- tempConcepts %>%
        rename(!!allCols[i] := external, new_label = label)
    } else {
      newConcepts <- tempConcepts %>%
        rename(!!allCols[i] := external) %>%
        left_join(newConcepts %>% select(has_broader = id, any_of(allCols), new_label) |> distinct(), by = "has_broader") %>%
        unite(col = "new_label", new_label, label, sep = "][", remove = TRUE)
    }

  }

  # ... to join them to the input table
  toOut <- table %>%
    select(-any_of("id")) %>%
    unite(col = "external", all_of(allCols), sep = "][", remove = FALSE) %>%
    left_join(newConcepts, by = allCols, relationship = "many-to-many") %>%
    select(-all_of(allCols)) %>%
    separate_wider_delim(cols = new_label, delim = "][", names = allCols)

  if(remakeSF){
    toOut <- toOut %>%
      st_sf()
  }

  if(groupMatches){
    matchCols <- c(allCols, "id", "match", "external", "has_broader", "class", "description")
    toOut <- toOut %>%
      group_by(across(-matchCols)) %>%
      summarise(across(.cols = matchCols, .fns = ~paste0(.x, collapse = " | ")))
  }

  out <- toOut %>%
    select(all_of(allCols), id, match, external, has_broader, class, description, everything())

  return(out)

}
