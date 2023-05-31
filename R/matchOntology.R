#' Match target terms with an ontology
#'
#' This function takes a table to replace the values of various columns with
#' harmonised values listed in the project specific gazetteer.
#' @param table [\code{character(1)}]\cr a table that contains columns that
#'   should be harmonised by matching with the gazetteer.
#' @param columns [\code{character(1)}]\cr the columns containing the concepts
#' @param dataseries [\code{character(1)}]\cr the source dataseries from which
#'   territories are sourced.
#' @param ontology [\code{onto}]\cr either a path where the ontology/gazetteer
#'   is stored, or an already loaded ontology.
#' @param beep [\code{integerish(1)}]\cr Number specifying what sound to be
#'   played to signal the user that a point of interaction is reached by the
#'   program, see \code{\link[beepr]{beep}}.
#' @param verbose [`logical(1)`][logical]\cr whether or not to give detailed
#'   information on the process of this function.
#' @return Returns a tibble that resembles the input table where the target
#'   columns were translated according to the provided ontology.
#' @importFrom checkmate assertFileExists
#' @importFrom utils head
#' @importFrom ontologics load_ontology new_source get_concept new_mapping
#'   make_tree edit_matches
#' @importFrom purrr map_dfr
#' @importFrom dplyr pull filter select mutate distinct bind_cols rename
#'   everything left_join rename_with na_if
#' @importFrom tibble tibble
#' @importFrom tidyselect all_of any_of where
#' @importFrom stringr str_split_i str_replace str_replace_all
#' @importFrom tidyr separate_rows separate pivot_wider fill pivot_longer
#'   separate_wider_delim contains
#' @importFrom sf st_drop_geometry
#' @export

matchOntology <- function(table = NULL, columns = NULL, dataseries = NULL,
                          ontology = NULL, verbose = FALSE, beep = NULL){

  assertIntegerish(x = beep, len = 1, lower = 1, upper = 11, null.ok = TRUE)

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))
  ontoPath <- ontology

  allCols <- get_class(ontology = ontoPath) %>%
    pull(label)

  assertSubset(x = head(columns, 1), choices = allCols)
  allCols <- allCols[which(allCols %in% head(columns, 1)) : which(allCols %in% tail(columns, 1))]

  # remove white-space and dots
  table <- table %>%
    mutate(across(where(is.character),
                  function(x){
                    temp <- trimws(x)
                    str_replace_all(string = temp, pattern = "[.]", replacement = "")
                  }))

  # fill from left to right
  for(i in seq_along(allCols)){
    if(i == 1) next
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

  # # prepare object to write into
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

    # identify whether concepts were already defined as external concepts...
    if(i == 1){

      tempTab <- tempTab %>%
        select(label = allCols[i])

      externalConcepts <- get_concept(label = tempTab$label, has_source = srcID,
                                      external = TRUE, ontology = ontoPath) %>%
        left_join(tibble(label = tempTab$label), ., by = "label") %>%
        mutate(class = allCols[i])

    } else {

      # first, transform the parents into the column 'has_broader'
      tempTab <- tempTab %>%
        left_join(newConcepts, by = allCols[1:(i-1)]) %>%
        select(label = allCols[i], has_broader = id)

      externalConcepts <- get_concept(label = tempTab$label, has_source = srcID,
                                      has_broader = tempTab$has_broader,
                                      external = TRUE, ontology = ontoPath) %>%
        left_join(tibble(label = tempTab$label, has_broader = tempTab$has_broader), ., by = c("label", "has_broader")) %>%
        mutate(class = allCols[i])

    }

    # ... if this is not the case, the only path forward is to first create mappings with the ontology
    if(any(is.na(externalConcepts$id))){

      relatedConcepts <- edit_matches(new = externalConcepts$label,
                                      target = externalConcepts %>% select(has_broader, class),
                                      source = dataseries,
                                      ontology = ontology,
                                      matchDir = paste0(intPaths, "/meta/", type, "/"),
                                      verbose = verbose,
                                      beep = beep)

      tempConcepts <- relatedConcepts %>%
        pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                     names_to = "match", values_to = "external") %>%
        separate_rows(external, sep = " \\| ") %>%
        mutate(external = na_if(x = external, y = ""),
               match = str_replace(string = match, pattern = "has_", replacement = ""),
               match = str_replace(string = match, pattern = "_match", replacement = "")) %>%
        distinct() %>%
        filter(!is.na(external)) %>%
        filter(!is.na(id)) %>%
        filter(class == allCols[i]) %>%
        arrange(id)

      if(dim(tempConcepts)[1] != 0){
        new_mapping(new = tempConcepts$external,
                    target = tempConcepts %>% select(id, label, class, has_broader),
                    source = dataseries,
                    match = tempConcepts$match,
                    certainty = 3,
                    ontology = ontoPath,
                    verbose = verbose,
                    beep = beep)
      }


    } else {

      tempConcepts <-  get_concept(str_detect(has_close_match, paste0(externalConcepts$id, collapse = "|")) |
                                    str_detect(has_broader_match, paste0(externalConcepts$id, collapse = "|")) |
                                    str_detect(has_narrower_match, paste0(externalConcepts$id, collapse = "|")) |
                                    str_detect(has_exact_match, paste0(externalConcepts$id, collapse = "|")),
                                  ontology = ontoPath) %>%
        pivot_longer(cols = c(has_broader_match, has_close_match, has_exact_match, has_narrower_match),
                     names_to = "match", values_to = "external") %>%
        separate_rows(external, sep = " \\| ") %>%
        filter(!is.na(external)) %>%
        mutate(externalID = str_split_i(external, "[.]", 1),
               match = str_replace(string = match, pattern = "has_", replacement = ""),
               match = str_replace(string = match, pattern = "_match", replacement = "")) %>%
        select(-external) %>%
        distinct() %>%
        left_join(externalConcepts %>% select(externalID = id, external = label), by = "externalID") %>%
        filter(!is.na(external)) %>%
        select(-externalID) %>%
        filter(class == allCols[i])

    }

    if(i == 1){
      newConcepts <- tempConcepts %>%
        rename(!!allCols[i] := external, new_label = label)
    } else {
      newConcepts <- tempConcepts %>%
        rename(!!allCols[i] := external) %>%
        left_join(newConcepts %>% select(has_broader = id, any_of(allCols), new_label), by = "has_broader") %>%
        unite(col = "new_label", new_label, label, sep = "][", remove = TRUE)
    }

  }

  # ... to join them to the input table
  toOut <- table %>%
    unite(col = "external", all_of(allCols), sep = "][", remove = FALSE) %>%
    left_join(newConcepts, by = allCols, relationship = "many-to-many") %>%
    select(-all_of(allCols)) %>%
    separate_wider_delim(cols = new_label, delim = "][", names = allCols)

  if(remakeSF){
    toOut <- toOut %>%
      st_sf()
  }

  out <- toOut %>%
    select(all_of(allCols), id, match, external, has_broader, class, description, everything())

  return(out)

}
