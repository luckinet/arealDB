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

matchOntology <- function(table = NULL, columns = NULL, dataseries = NULL,
                          ontology = NULL, verbose = FALSE){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))
  ontoPath <- ontology

  type <- str_split(tail(str_split(string = ontoPath, pattern = "/")[[1]], 1), "[.]")[[1]][1]

  # # set internal objects
  # intPaths <- paste0(getOption(x = "adb_path"))
  #
  # # get objects
  # inv_tables <- read_csv(paste0(intPaths, "/inv_tables.csv"), col_types = "iiiccccDccccc")
  # inv_dataseries <- read_csv(paste0(intPaths, "/inv_dataseries.csv"), col_types = "icccccc")
  #
  # # check validity of arguments
  # assertChoice(x = dataseries, choices = inv_dataseries$name)
  #
  # datID <- ifelse(length(inv_tables$datID) == 0, 1,
  #                 inv_dataseries$datID[grep(pattern = dataseries, x = inv_dataseries$name)])
  # sources <- inv_tables$source_file[inv_tables$datID %in% datID]
  # schemas <- inv_tables$schema[inv_tables$datID %in% datID]
  #
  # message("--> extracting concepts of variable '", variable,"'...")
  #
  # if(length(sources) == 0){
  #   stop("no tables have been defined for this dataseries.")
  # }
  #
  # out <- NULL
  # for(i in seq_along(sources)){
  #
  #   message("    from table '", sources[i], "' ...")
  #
  #   algorithm <- readRDS(file = paste0(intPaths, "/meta/schemas/", schemas[i], ".rds"))
  #   thisTable <- read.csv(file = paste0(intPaths, "/adb_tables/stage2/", sources[i]),
  #                         header = FALSE,
  #                         strip.white = TRUE,
  #                         as.is = TRUE,
  #                         na.strings = algorithm@format$na,
  #                         encoding = "UTF-8") %>%
  #     as_tibble()
  #
  #   temp <- thisTable %>%
  #     reorganise(schema = algorithm)
  #
  #   assertChoice(x = variable, choices = names(temp))
  #
  #   concepts <- pull(temp, variable)
  #   out <- c(out, concepts)
  #
  # }
  #
  # out <- unique(out)




  # # load the ontology
  # if(inherits(x = ontology, what = "onto")){
  #   gaz <- ontology
  # } else {
  #   assertFileExists(x = ontology, access = "rw", extension = "rds")
  #   gaz <- load_ontology(path = ontology)
  # }

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

  theColumns <- NULL
  for(i in seq_along(columns)){

    theColumn <- columns[i]
    theColumns <- c(theColumns, theColumn)

    temp <- tab %>%
      distinct(across(all_of(theColumns))) %>%
      mutate(class = theColumn) %>%
      select(label = all_of(theColumn), class, has_broader = theColumns[i-1])

    if(i != 1){

      findBroader <- temp %>%
        select(label = has_broader) %>%
        distinct()

      broaderConc <- get_concept(table = findBroader, ontology = ontoPath) %>%
        filter(has_source == srcID & class == theColumns[i-1]) %>%
        left_join(findBroader, ., by = "label") %>%
        select(id) %>%
        bind_cols(temp %>% distinct(has_broader))

      temp <- temp %>%
        left_join(broaderConc, by = "has_broader") %>%
        select(label, class, has_broader = id)

    }

    # extract all harmonised concepts, including those that may be not available
    # (na) ...
    harmonisedConc <- get_concept(table = temp, ontology = ontoPath, mappings = TRUE) %>%
      distinct(label, class, id, has_broader)

    if(dim(harmonisedConc)[1] != dim(temp)[1]){
      stop("improve matching here")
    }

    # ... for if new concepts are still missing from the ontology, they need to be
    # mapped
    if(any(is.na(harmonisedConc$id))){

      new_mapping(new = harmonisedConc$label,
                  target = harmonisedConc %>% select(class, has_broader),
                  source = dataseries,
                  certainty = 3,
                  ontology = ontoPath,
                  matchDir = paste0(intPaths, "/meta/", type, "/"),
                  verbose = verbose)

      # write the new concepts into 'table'
      if(i == 1){

        new <- tab %>%
          select(label = all_of(theColumn)) %>%
          distinct() %>%
          arrange(label)

        newConcepts <- get_concept(table = new, ontology = ontoPath) %>%
          filter(has_source == srcID) %>%
          rename(target := external) %>%
          select(-class, -description, !!theColumn := target)

        toOut <- table %>%
          left_join(newConcepts, by = "al1") %>%
          filter(!is.na(id)) %>%
          select(-all_of(theColumn), -match, -has_source, -has_broader) %>%
          rename(!!theColumn := label)

      } else {

        new <- toOut %>%
          st_drop_geometry() %>%
          select(label = all_of(theColumn), has_broader = id) %>%
          distinct() %>%
          arrange(label)

        newConcepts <- get_concept(table = new, ontology = ontoPath) %>%
          rename(target := external) %>%
          bind_cols(new %>% select(!!theColumn := label)) %>%
          mutate(target = if_else(!class %in% theColumn, !!sym(theColumn), target),
                 has_broader = if_else(!class %in% theColumn, id, has_broader),
                 id = if_else(!class %in% theColumn, paste0(id, get_class(ontology = ontoPath)$id[1]), id)) %>%
          select(!!theColumn, label, id, has_broader)

        toOut <- toOut %>%
          mutate(has_broader = id) %>%
          select(-id) %>%
          left_join(newConcepts, by = c(theColumn, "has_broader")) %>%
          select(-all_of(theColumn), -has_broader) %>%
          rename(!!theColumn := label)
      }

    } else {
      if(i == 1){
        toOut <- table %>%
          left_join(harmonisedConc %>% select(!!theColumn := label, id), theColumn)
      } else {
        toOut <- toOut %>%
          select(-id) %>%
          left_join(harmonisedConc %>% select(!!theColumn := label, id), theColumn)
      }
    }


  }

  out <- toOut %>%
    select(all_of(columns), id, everything())

  return(out)

}
