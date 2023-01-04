#' Match territories in a table with an ontology/gazetteer
#'
#' This function takes a table to replace the values of various columns with
#' harmonised values listed in the project specific gazetteer.
#' @param table [\code{character(1)}]\cr a table that contains columns that
#'   should be harmonised by matching with the gazetteer.
#' @param columns [\code{character(1)}]\cr the columns containing the concepts
#' @param dataseries [\code{character(1)}]\cr the source dataseries from which
#'   territories are sourced.
#' @param all_cols [`logical(1)`][logical]\cr whether or not to output all
#'   tentative columns (for debugging), or only the essential columns.
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

matchOntology <- function(table = NULL, columns = NULL, dataseries = NULL,
                          ontology = NULL, verbose = FALSE, all_cols = FALSE){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))
  ontoPath <- ontology

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
  } else {
    tab <- table
  }

  # replace values with an empty name with a placeholder
  # for(i in seq_along(columns)){
  #
  #   theColumn <- columns[i]
  #   tab <- tab %>%
  #     mutate(across(all_of(theColumn), trimws),
  #            unnamed = if_else(!!sym(theColumn) %in% c("", " "), TRUE, FALSE),
  #            iter = cumsum(unnamed),
  #            iter = if_else(unnamed, iter, 0L),
  #            !!theColumn := if_else(!!sym(theColumn) %in% c("", " "), paste0("unnamed_", iter), !!sym(theColumn))) %>%
  #     select(-unnamed, -iter)
  #
  # }

  theColumns <- NULL
  for(i in seq_along(columns)){

    theColumn <- columns[i]
    theColumns <- c(theColumns, theColumn)

    temp <- tab %>%
      distinct(across(all_of(theColumns))) %>%
      filter(!is.na(!!sym(theColumn)))

    # extract all harmonised concepts, including those that may be not available
    # (na) ...
    harmonisedConc <- get_concept(table = temp, per_class = TRUE, ontology = ontoPath)

    # ... for if new concepts are still missing from the ontology, they need to be
    # mapped
    if(any(is.na(harmonisedConc$id))){

      new_mapping(new = trimws(harmonisedConc$label),
                  target = harmonisedConc %>% select(class, has_broader),
                  source = dataseries,
                  certainty = 3,
                  ontology = ontoPath,
                  matchDir = paste0(intPaths, "/meta/", type, "/"),
                  verbose = verbose)

      # new = trimws(harmonisedConc$label); target = harmonisedConc %>% select(class, has_broader); source = dataseries; certainty = 3; ontology = ontoPath;  matchDir = paste0(intPaths, "/meta/", type, "/"); match = NULL; type = "concept"; lut = NULL; verbose = FALSE

      # write the new concepts into 'table'
      if(i == 1){

        new <- tab %>%
          select(label = all_of(theColumn)) %>%
          distinct() %>%
          arrange(label)

        newConcepts <- get_concept(table = new, ontology = ontoPath) %>%
          filter(has_source == srcID & class %in% theColumn) %>%
          rename(target := external) %>%
          select(-class, -description, !!theColumn := target)

        # a little check that the join will be successful
        assertNames(x = newConcepts %>% pull(!!theColumn) %>% unique(),
                    subset.of = table %>% pull(!!theColumn) %>% unique() %>% trimws())

        toOut <- table %>%
          left_join(newConcepts, by = theColumn) %>%
          rename(external = all_of(theColumn), !!theColumn := label)

        if(!all_cols){
          toOut <- toOut %>%
            select(-external, -match, -has_source, -has_broader)
        }

      } else {

        new <- toOut %>%
          st_drop_geometry() %>%
          select(label = all_of(theColumn), has_broader = id) %>%
          distinct() %>%
          arrange(label)

        newConcepts <- get_concept(table = new, ontology = ontoPath) %>%
          filter(has_source == srcID) %>%
          rename(target := external) %>%
          mutate(!!theColumn:= target,
                 target = if_else(!class %in% theColumn, !!sym(theColumn), target),
                 has_broader = if_else(!class %in% theColumn, id, has_broader),
                 id = if_else(!class %in% theColumn, paste0(id, get_class(ontology = ontoPath)$id[1]), id)) %>%
          select(!!theColumn, label, id, has_broader)

        toOut <- toOut %>%
          mutate(has_broader = id) %>%
          select(-id) %>%
          left_join(newConcepts, by = c(theColumn, "has_broader")) %>%
          select(-all_of(theColumn), -has_broader) %>%
          rename(!!theColumn := label)

        # if(!all_cols){
        #   toOut <- toOut %>%
        #     select(-external, -match, -has_source, -has_broader)
        # }
      }

    } else {
      if(i == 1){
        toOut <- table %>%
          left_join(harmonisedConc %>% select(!!theColumn := label, id), theColumn)
      } else {
        toOut <- toOut %>%
          rename(has_broader = id) %>%
          left_join(harmonisedConc %>% select(!!theColumn := label, has_broader, id), c(theColumn, "has_broader")) %>%
          select(-has_broader)
      }
    }
  }

  out <- toOut %>%
    select(all_of(columns), id, everything())

  return(out)

}
