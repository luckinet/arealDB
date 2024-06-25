#' Update an ontology
#'
#' This function takes a table (spatial) and updates all territorial concepts in
#' the provided gazetteer.
#' @param table [\code{character(1)}]\cr a table that contains a match column as
#'   the basis to update the gazetteer.
#' @param threshold [\code{numeric(1)}]\cr a threshol value above which matches
#'   are updated in the gazetteer.
#' @param dataseries [\code{character(1)}]\cr the source dataseries of the
#'   external concepts for which the gazetteer shall be updated.
#' @param ontology [\code{onto}]\cr path where the ontology/gazetteer is stored.
#' @return called for its side-effect of updating a gazetteer
#' @importFrom checkmate assertNumeric assertCharacter
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr rowwise mutate distinct filter select left_join
#' @importFrom stringr str_split_1 str_replace_all
#' @importFrom tidyr separate_rows separate
#' @importFrom ontologics get_concept new_concept new_mapping
#' @export

updateOntology <- function(table = NULL, threshold = NULL, dataseries = NULL,
                           ontology = NULL){

  assertNumeric(x = threshold, len = 1, lower = 0, upper = 100, any.missing = FALSE)
  assertCharacter(x = dataseries, len = 1, any.missing = FALSE)
  ontoPath <- ontology

  longTable <- table %>%
    rowwise() %>%
    mutate(parentID = paste0(head(str_split_1(gazID, "[.]"), -1), collapse = ".")) %>%
    separate_rows("match", sep = " \\| ") %>%
    mutate(match = str_replace_all(match, "\\[|\\]", "")) %>%
    separate(col = match, into = c("match", "amount"), sep = " ", fill = "right") %>%
    separate(col = amount, into = c("amount", "targetID"), sep = "_", fill = "right") %>%
    separate(col = amount, into = c("source_overlap", "target_overlap"), sep = "<>", fill = "right") %>%
    mutate(source_overlap = suppressWarnings(as.numeric(source_overlap)),
           target_overlap = suppressWarnings(as.numeric(target_overlap))) %>%
    distinct()

  # 1. include new hamonised concepts in the ontology, in case they deviate more than 'threshold'
  newConcepts <- longTable %>%
    filter(new_name)
  newConcepts <- newConcepts %>%
    select(id = parentID, external, gazClass) %>%
    distinct() %>%
    left_join(get_concept(id = newConcepts$parentID, ontology = ontoPath), by = "id") %>%
    select(external, gazClass, id, label, class)

  if(dim(newConcepts)[1] != 0){

    new_concept(new = newConcepts$external,
                broader = newConcepts %>% select(id, label, class),
                description = paste0("external concept originating in the dataseries '", dataseries, "'"),
                class = newConcepts$gazClass,
                ontology =  ontoPath)
    temp <- get_concept(has_broader = newConcepts$id, label = newConcepts$external, str_detect(description, !!dataseries), ontology = ontoPath) %>%
      arrange(label) %>%
      select(id, label, class, has_broader) %>%
      group_by(label, class, has_broader) %>%
      filter(row_number() == 1) %>% # in case an entry is various times in the table, filter only the first one
      ungroup()
    new_mapping(new = sort(newConcepts$external),
                target = temp,
                source = dataseries, match = "close", certainty = 3, type = "concept", ontology = ontoPath)

  }

  # 2. define mappings of the new concepts with harmonised concepts
  harmOnto <- get_concept(id = longTable$targetID, ontology = ontoPath)

  if(dim(harmOnto)[1] != 0){

    if(any(longTable$match == "close")){
      newClose <- longTable %>%
        filter(match == "close")
      newClose <- harmOnto %>%
        filter(id %in% newClose$targetID) %>%
        select(id, label, class, has_broader) %>%
        left_join(newClose %>% select(id = targetID, external), ., by = "id")

      assertCharacter(x = newClose$id, any.missing = FALSE)
      assertCharacter(x = newClose$label, any.missing = FALSE)
      assertCharacter(x = newClose$class, any.missing = FALSE)
      new_mapping(new =  newClose$external,
                  target = newClose %>% select(id, label, class, has_broader),
                  source = dataseries, match = "close", certainty = 3, type = "concept", ontology = ontoPath)
    }

    if(any(longTable$match == "narrower")){
      newNarrower <- longTable %>%
        filter(match == "narrower")
      newNarrower <- harmOnto %>%
        filter(id %in% newNarrower$targetID) %>%
        select(id, label, class, has_broader) %>%
        left_join(newNarrower %>% select(id = targetID, external), ., by = "id")

      assertCharacter(x = newNarrower$id, any.missing = FALSE)
      assertCharacter(x = newNarrower$label, any.missing = FALSE)
      assertCharacter(x = newNarrower$class, any.missing = FALSE)
      new_mapping(new = newNarrower$external,
                  target = newNarrower %>% select(id, label, class, has_broader),
                  source = dataseries, match = "narrower", certainty = 3, type = "concept", ontology = ontoPath)
    }

    if(any(longTable$match == "broader")){
      newBroader <- longTable %>%
        filter(match == "broader")
      newBroader <- harmOnto %>%
        filter(id %in% newBroader$targetID) %>%
        select(id, label, class, has_broader) %>%
        left_join(newBroader %>% select(id = targetID, external), ., by = "id")

      assertCharacter(x = newBroader$id, any.missing = FALSE)
      assertCharacter(x = newBroader$label, any.missing = FALSE)
      assertCharacter(x = newBroader$class, any.missing = FALSE)
      new_mapping(new = newBroader$external,
                  target = newBroader %>% select(id, label, class, has_broader),
                  source = dataseries, match = "broader", certainty = 3, type = "concept", ontology = ontoPath)
    }

  }

}
