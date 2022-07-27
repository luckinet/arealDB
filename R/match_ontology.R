#' Match territories in a table with an ontology/gazetteer
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
#' @param verbose [`logical(1)`][logical]\cr whether or not to give detailed
#'   information on the process of this function.
#' @importFrom checkmate assertFileExists
#' @importFrom ontologics load_ontology new_source get_concept new_mapping
#' @importFrom purrr map_dfr
#' @importFrom dplyr pull filter select mutate distinct bind_cols rename
#'   everything left_join
#' @importFrom tibble tibble
#' @importFrom tidyselect all_of
#' @importFrom tidyr separate_rows separate pivot_wider
#' @importFrom sf st_drop_geometry
#' @export

match_ontology <- function(table = NULL, columns = NULL, dataseries = NULL,
                           ontology = NULL, verbose = FALSE){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))

  # load the ontology
  if(inherits(x = ontology, what = "onto")){
    gaz <- ontology
  } else {
    assertFileExists(x = ontology, access = "rw", extension = "rds")
    gaz <- load_ontology(path = ontology)
  }

  # extract all non-harmoniesd concepts
  temp <- map_dfr(.x = seq_along(columns), .f = function(ix){

    table %>%
      pull(columns[ix]) %>%
      unique() %>%
      tibble(label = ., class = columns[ix])

  }) %>%
    filter(!is.na(label))

  # make a new dataseries, in case it doesn't exist yet
  if(!dataseries %in% gaz@sources$label){
    gaz <- new_source(name = dataseries, ontology = gaz)
  }

  # extract all harmonised concepts, including those that may be not available
  # (na) ...
  harmonisedConc <- temp %>%
    get_concept(x = ., na.rm = FALSE, ontology = gaz)

  # ... for if new concepts are still missing from the ontology, they need to be
  # mapped
  if(any(is.na(harmonisedConc$id))){

    gaz <- new_mapping(new = harmonisedConc$label, target = harmonisedConc %>% select(class, has_broader),
                       source = dataseries, certainty = 3, ontology = gaz,
                       matchDir = paste0(intPaths, "/meta/concepts/"), verbose = verbose)

    write_rds(x = gaz, file = ontology)

  }

  # write the new concepts into 'table'
  if(inherits(x = table, what = "sf")){
    tab <- table %>%
      st_drop_geometry()
  } else {
    tab <- table
  }

  for(i in seq_along(columns)){

    theColumn <- columns[i]

    # ... and assign them to the respective column ...
    if(i == 1){

      new <- tab %>%
        select(label = all_of(theColumn)) %>%
        distinct() %>%
        arrange(label) %>%
        mutate(class = theColumn)

      newConcepts <- get_concept(x = new, ontology = gaz, mappings = "all") %>%
        select(-has_broader_match, -has_close_match, -has_exact_match, -has_narrower_match) %>%
        arrange(label) %>%
        rename(target := label) %>%
        select(-class, -description) %>%
        bind_cols(new %>% select(!!theColumn := label))

      temp <- table %>%
        left_join(newConcepts, by = theColumn) %>%
        select(-all_of(theColumn)) %>%
        rename(!!theColumn := target)

    } else {

      new <- temp %>%
        st_drop_geometry() %>%
        select(label = all_of(theColumn), has_broader = id) %>%
        distinct() %>%
        arrange(label)

      newConcepts <- get_concept(x = new, ontology = gaz, mappings = "all") %>%
        # select(-has_broader_match, -has_close_match, -has_exact_match, -has_narrower_match) %>%
        separate_rows(has_broader_match, has_close_match, has_exact_match, has_narrower_match, sep = " \\| ") %>%
        rename(target := label) %>%
        # select(-class, -description) %>%
        bind_cols(new %>% select(!!theColumn := label)) %>%
        mutate(target = if_else(!class %in% theColumn, !!sym(theColumn), target),
               has_broader = if_else(!class %in% theColumn, id, has_broader),
               id = if_else(!class %in% theColumn, paste0(id, gaz@classes$harmonised$id[1]), id)) %>%
        select(!!theColumn, target, id, has_broader)

      temp <- temp %>%
        mutate(has_broader = id) %>%
        select(-id) %>%
        left_join(newConcepts, by = c(theColumn, "has_broader")) %>%
        select(-all_of(theColumn)) %>%
        rename(!!theColumn := target)
    }

  }

  out <- temp %>%
    select(all_of(columns), id, everything()) %>%
    select(-has_broader)

  return(out)

}
