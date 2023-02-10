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
#' @param beep [\code{integerish(1)}]\cr Number specifying what sound to be
#'   played to signal the user that a point of interaction is reached by the
#'   program, see \code{\link[beepr]{beep}}.
#' @param exact_class [`logical(1)`][logical]\cr whether or not to extract only
#'   the classes in \code{table}, or also all parent classes.
#' @param verbose [`logical(1)`][logical]\cr whether or not to give detailed
#'   information on the process of this function.
#' @importFrom checkmate assertFileExists
#' @importFrom utils head
#' @importFrom ontologics load_ontology new_source get_concept new_mapping
#'   make_tree
#' @importFrom purrr map_dfr
#' @importFrom dplyr pull filter select mutate distinct bind_cols rename
#'   everything left_join rename_with
#' @importFrom tibble tibble
#' @importFrom tidyselect all_of any_of where
#' @importFrom stringr str_split_i
#' @importFrom tidyr separate_rows separate pivot_wider fill pivot_longer
#'   separate_wider_delim contains
#' @importFrom sf st_drop_geometry
#' @export

matchOntology <- function(table = NULL, columns = NULL, dataseries = NULL,
                          ontology = NULL, verbose = FALSE, all_cols = FALSE,
                          beep = NULL, exact_class = TRUE){

  assertIntegerish(x = beep, len = 1, lower = 1, upper = 11, null.ok = TRUE)

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))
  ontoPath <- ontology

  allCols <- get_class(ontology = ontoPath) %>%
    pull(label)
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

  tab <- tab %>%
    distinct(across(all_of(allCols))) %>%
    filter(!is.na(!!sym(tail(allCols, 1))))

  # extract all harmonised concepts, including those that may be not available
  # (na) ...
  harmonisedConc <- get_concept(table = tab, per_class = TRUE, ontology = ontoPath)

  # ... for if new concepts are still missing from the ontology, they need to be
  # mapped
  if(any(is.na(harmonisedConc$id))){

    new_mapping(new = trimws(harmonisedConc$label),
                target = harmonisedConc %>% select(class, has_broader),
                source = dataseries,
                certainty = 3,
                ontology = ontoPath,
                matchDir = paste0(intPaths, "/meta/", type, "/"),
                verbose = verbose,
                beep = beep)
    # new = trimws(harmonisedConc$label); target = harmonisedConc %>% select(class, has_broader); source = dataseries; certainty = 3; ontology = ontoPath;  matchDir = paste0(intPaths, "/meta/", type, "/"); match = NULL; type = "concept"; lut = NULL; verbose = FALSE

    # then construct a datastructure that contains all (new) concepts ...
    externalTerms <- get_concept(external = TRUE, ontology = ontoPath) %>%
      select(id, external = label, has_source)

    newConcepts <- get_concept(table = tab %>% select(label = tail(allCols, 1)), ontology = ontoPath) %>%
      filter(has_source == srcID & class %in% tail(allCols, 1) & match == "close") %>%
      make_tree(reverse = TRUE, ontology = ontoPath) %>%
      filter(class %in% allCols) %>%
      pivot_longer(cols = c(has_close_match, has_broader_match, has_narrower_match, has_exact_match),
                   names_to = "match", values_to = "external") %>%
      filter(!is.na(external)) %>%
      separate_rows(external, sep = " \\| ") %>%
      filter(str_detect(external, dataseries)) %>%
      rowwise() %>%
      mutate(match = str_split_1(match, "_")[2],
             externalID = str_split_1(external, "[.]")[[1]][1],
             external = NULL) %>%
      left_join(externalTerms, by = c("externalID" = "id")) %>%
      mutate(toExport = external) %>%
      pivot_wider(id_cols = c(id, has_broader, match, toExport, has_source, externalID), names_from = "class", values_from = c("label", "external")) %>%
      select(-externalID) %>%
      fill(contains(head(allCols, -1)), .direction = "down") %>%
      filter(!is.na(!!sym(paste0("external_", tail(allCols, 1))))) %>%
      unite(col = "label", paste0("label_", allCols), sep = "][") %>%
      rename_with(.cols = starts_with("external_"), .fn = ~ str_split_i(string = .x, pattern = "_", i = 2))

    # ... to join them to the input table
    toOut <- table %>%
      left_join(newConcepts, by = allCols, multiple = "all") %>%
      select(-all_of(allCols)) %>%
      separate_wider_delim(cols = label, delim = "][", names = allCols) %>%
      rename(external = toExport)

    if(remakeSF){
      toOut <- toOut %>%
        st_sf()
    }

  } else {
    if(i == 1){
      toOut <- table %>%
        left_join(harmonisedConc %>% select(!!tail(allCols, 1) := label, id), tail(allCols, 1))
    } else {
      toOut <- toOut %>%
        select(-any_of(c("external", "has_broader", "match", "has_source"))) %>%
        rename(has_broader = id) %>%
        left_join(harmonisedConc %>% select(!!tail(allCols, 1) := label, has_broader, id), c(tail(allCols, 1), "has_broader"))
    }
    toOut <- toOut %>%
      mutate(match = "exact",
             has_source = "1",
             external = !!sym(tail(allCols, 1)))
  }

  if(!all_cols){
    out <- toOut %>%
      select(-external, -has_broader, -match, -has_source) %>%
      select(all_of(allCols), id, everything())
  } else {
    out <- toOut %>%
      select(-any_of(c("has_broader"))) %>%
      select(all_of(allCols), id, match, external, has_source, everything())
  }

  return(out)



  # break compilation here ##############
  # theColumns <- NULL
  # for(i in seq_along(allCols)){
  #
  #   theColumn <- allCols[i]
  #
  #   if(!theColumn %in% columns){
  #     fillPrevious <- TRUE
  #     next
  #   }
  #
  #   theColumns <- c(theColumns, theColumn)
  #
  #   if(i == 1){
  #     temp <- tab %>%
  #       distinct(across(all_of(theColumns))) %>%
  #       filter(!is.na(!!sym(theColumn)))
  #   } else {
  #     temp <- toOut %>%
  #       st_drop_geometry() %>%
  #       distinct(across(all_of(theColumns))) %>%
  #       select(all_of(theColumns))
  #   }
  #
  #   # extract all harmonised concepts, including those that may be not available
  #   # (na) ...
  #   harmonisedConc <- get_concept(table = temp, per_class = TRUE, ontology = ontoPath)
  #
  #   # ... for if new concepts are still missing from the ontology, they need to be
  #   # mapped
  #   if(any(is.na(harmonisedConc$id))){
  #
  #     new_mapping(new = trimws(harmonisedConc$label),
  #                 target = harmonisedConc %>% select(class, has_broader),
  #                 source = dataseries,
  #                 certainty = 3,
  #                 ontology = ontoPath,
  #                 matchDir = paste0(intPaths, "/meta/", type, "/"),
  #                 verbose = verbose,
  #                 beep = beep)
  #
  #     # new = trimws(harmonisedConc$label); target = harmonisedConc %>% select(class, has_broader); source = dataseries; certainty = 3; ontology = ontoPath;  matchDir = paste0(intPaths, "/meta/", type, "/"); match = NULL; type = "concept"; lut = NULL; verbose = FALSE
  #
  #     # write the new concepts into 'table'
  #     if(i == 1){
  #
  #       new <- tab %>%
  #         select(label = all_of(theColumn)) %>%
  #         distinct() %>%
  #         arrange(label)
  #
  #       if(exact_class){
  #         newConcepts <- get_concept(table = new, ontology = ontoPath) %>%
  #           filter(has_source == srcID & class %in% theColumn)
  #       } else {
  #         newConcepts <- get_concept(table = new, ontology = ontoPath) %>%
  #           filter(has_source == srcID)
  #       }
  #       newConcepts <- newConcepts %>%
  #         rename(target := external) %>%
  #         select(-class, -description, !!theColumn := target)
  #
  #       # a little check that the join will be successful
  #       assertNames(x = newConcepts %>% pull(!!theColumn) %>% unique(),
  #                   subset.of = table %>% pull(!!theColumn) %>% unique() %>% trimws())
  #
  #       toOut <- table %>%
  #         left_join(newConcepts, by = theColumn) %>%
  #         rename(external = all_of(theColumn), !!theColumn := label)
  #
  #     } else {
  #
  #       new <- toOut %>%
  #         st_drop_geometry() %>%
  #         select(label = all_of(theColumn), has_broader = id) %>%
  #         distinct() %>%
  #         arrange(label) %>%
  #         mutate(lvl = length(str_split(has_broader, "[.]")[[1]]))
  #
  #       filterClasses <- get_class(ontology = ontology) %>%
  #         filter(label %in% "al3")
  #       filterClassLevel <- length(str_split(string = filterClasses$id, pattern = "[.]")[[1]])
  #
  #       if(all(new$lvl < filterClassLevel-1)){
  #         new <- new %>%
  #           select(-has_broader)
  #         targetCols <- c(theColumn, "label")
  #       } else {
  #         targetCols <- c(theColumn, "has_broader", "label")
  #       }
  #
  #       if(exact_class){
  #         newConcepts <- get_concept(table = new %>% select(-lvl), ontology = ontoPath) %>%
  #           filter(has_source == srcID & class %in% theColumn)
  #       } else {
  #         newConcepts <- get_concept(table = new %>% select(-lvl), ontology = ontoPath) %>%
  #           filter(has_source == srcID)
  #       }
  #       newConcepts <- newConcepts %>%
  #         rename(!!theColumn := external) %>%
  #         mutate(has_broader = if_else(!class %in% theColumn, id, has_broader),
  #                id = if_else(!class %in% theColumn, paste0(id, get_class(ontology = ontoPath)$id[1]), id)) %>%
  #         select(-class, -description) %>%
  #         group_by(across(all_of(targetCols))) %>%
  #         summarise(match = paste0(unique(match), collapse = " | "),
  #                   id = paste0(unique(id), collapse = " | "),
  #                   has_broader = paste0(unique(has_broader), collapse = " | "),
  #                   has_source = paste0(unique(has_source), collapse = " | ")) %>%
  #         ungroup()
  #
  #       tempOut <- toOut %>%
  #         select(-any_of(c("external", "has_broader", "match", "has_source")))
  #
  #       if(all(new$lvl < filterClassLevel-1)){
  #         tempOut <- tempOut %>%
  #           select(-id)
  #       } else {
  #         tempOut <- tempOut %>%
  #           rename(has_broader = id)
  #       }
  #
  #       toOut <- tempOut %>%
  #         left_join(newConcepts, by = targetCols[-which(targetCols %in% "label")]) %>%
  #         rename(external = all_of(theColumn), !!theColumn := label)
  #
  #       if(fillPrevious){
  #         toOut <- toOut %>%
  #           select(id = has_broader) %>%
  #           st_drop_geometry() %>%
  #           get_concept(ontology = ontoPath) %>%
  #           select(id, !!allCols[i-1] := label) %>%
  #           distinct() %>%
  #           filter(!is.na(id)) %>%
  #           left_join(toOut, ., by = c("has_broader" = "id"))
  #       }
  #
  #     }
  #
  #   }
  #
  #   fillPrevious <- FALSE
  # }

}
