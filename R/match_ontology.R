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
#' @param from_meta [\code{logical(1)}]\cr whether or not to load the matches
#'   from previous matching tables, or construct them from \code{table} and
#'   \code{columns}.
#' @param verbose [\code{logical(1)}]\cr whether or not to give detailed
#'   information on the prcess of this function.
#' @importFrom ontologics load_ontology get_concept new_concept new_mapping
#'   new_source
#' @importFrom purrr map
#' @importFrom stringr str_replace_all
#' @importFrom dplyr bind_cols
#' @importFrom tidyr separate_rows separate
#' @importFrom sf st_drop_geometry
#' @importFrom utils tail head
#' @importFrom stats na.omit
#' @export

match_ontology <- function(table = NULL, columns = NULL, dataseries = NULL,
                           ontology = NULL, from_meta = FALSE, verbose = FALSE){

  # table = temp; columns = unitCols; dataseries = dSeries; ontology = gazPath; from_meta = FALSE; verbose = FALSE

  assertLogical(x = from_meta, len = 1, any.missing = FALSE)
  assertLogical(x = verbose, len = 1, any.missing = FALSE)

  intPaths <- paste0(getOption(x = "adb_path"))

  if(inherits(x = ontology, what = "onto")){
    gazPath <- NULL
  } else {
    assertFileExists(x = ontology, access = "rw", extension = "rds")
    gazPath <- ontology
    theName <- tail(str_split(string = ontology, "/")[[1]], 1)
    theName <- head(str_split(string = theName, pattern = "[.]")[[1]], 1)

    gaz <- load_ontology(path = gazPath)
  }

  theClasses <- gaz@classes
  subClasses <- theClasses$harmonised$label[1:which(theClasses$harmonised$label == columns)]
  parentClass <- theClasses$harmonised$label[which(theClasses$harmonised$label == columns) - 1]

  # first, identify from where to take matches ----
  if(from_meta){

    toMatch <- list.files(paste0(intPaths, "/meta/concepts"), full.names = TRUE)

  } else {
    assertDataFrame(x = table)
    assertCharacter(x = columns, any.missing = FALSE)
    assertCharacter(x = dataseries, len = 1, any.missing = FALSE)

    toMatch <- "table"

    concepts <- map(.x = seq_along(columns), .f = function(ix){

      table %>%
        pull(columns[ix]) %>%
        unique()

    }) %>%
      unlist()
    concepts <- concepts[!is.na(concepts)]

    # determine those concepts, that are not yet defined in the gazetteer
    missingConcepts <- get_concept(x = tibble(label = concepts, class = columns), missing = TRUE, ontology = gaz, mappings = c("close", "narrower"))
    inclConcepts <- get_concept(x = tibble(label = concepts, class = columns), ontology = gaz, mappings = c("close", "narrower"))

    # build a table of external concepts and of harmonised concepts these should be overwritten with
    if(dim(missingConcepts)[1] != 0){

      relate <- gaz@concepts$harmonised %>%
        select(id, label, class) %>%
        left_join(inclConcepts, by = c("id", "label", "class")) %>%
        # mutate(external_id = NULL) %>%
        filter(!is.na(class)) %>%
        filter(class %in% subClasses)

      sortIn <- missingConcepts %>%
        select(id, label, class, description, everything()) %>%
        mutate(#external_id = NULL,
               sort_in = label,
               label = NA_character_,
               class = NA_character_)

      # put together the object that shall be edited by the user ...
      sortIn %>%
        bind_rows(relate) %>%
        write_csv(file = paste0(intPaths, "/matching.csv"), quote = "all", na = "")

      # ... and make them aware of their duty
      message("please edit the file '", paste0(intPaths, "/matching.csv"), "'")
      if(verbose){
        message("column description and tips \n\ncode       : filter by this column to jump to the subset you need to edit\nharmonised : concepts to which the new terms should be related \nclass      : the class of harmonised concepts \nclose      : in case a new concept is a close match to the harmonised concept, paste \n             it next to that concept, delimit several concepts with a '|' \nnarrower     : in case a new concept is not the same as any harmonised concept, paste \n             it next to that concept into which it is nested, delimit \n             several concepts with a '|' \nsort_in    : cut out these concepts and sort them either into 'close' or into 'narrower' \n\n-> values that were already successfully matched by previous translations are listed here, \n   however, altering them here doesn't change the ontology. \n\n-> any row that doesn't contain a value in the column 'code' will be discarded. Hence, \n   if you want a value to be ignored, simply don't paste it anywhere. \n\n-> do not change the values in the columns 'code', 'harmonised' and 'class', as they \n   are important to insert the new matches into the ontology. \n\n-> if a term shall be nested into a position that doesn't have a class, (for example, \n   because that class occurrs the first time with this term) first create that nested \n   class with 'new_class()'.")
      }
      done <- readline(" -> press any key when done: ")

    } else {
      # write an empty table
      tibble(id = NA_character_,
             note = NA_character_) %>%
        write_csv(file = paste0(intPaths, "/matching.csv"), quote = "all", na = "")
    }

  }

  # ... then align these matches with the ontology ----
  for(i in seq_along(toMatch)){

    if(from_meta){
      related <- read_csv(toMatch[i], col_types = cols(.default = "c"))

      dataseries <- tail(str_split(toMatch[i], "/")[[1]], 1)
      dataseries <- str_split(dataseries, "_")[[1]]
      dataseries <- str_split(dataseries[2], "[.]")[[1]][1]
    } else {
      related <- read_csv(paste0(intPaths, "/matching.csv"), col_types = cols(.default = "c")) %>%
        filter(!is.na(id))

      if(dim(related)[1] == 0){
        related <- NULL
      }
    }

    if(!is.null(related)){

      # make a new dataseries, in case it doesn't exist yet
      if(!dataseries %in% gaz@sources$label){
        gaz <- new_source(name = dataseries, ontology = gaz)
      }

      nestedMatch <- related %>%
        filter(!is.na(narrower)) %>%
        separate_rows(narrower, sep = " \\| ") %>%
        select(-close)

      closeMatch <- related %>%
        filter(!is.na(close)) %>%
        separate_rows(close, sep = " \\| ")

      # in case nested matches are present, derive the mappings table ...
      if(dim(nestedMatch)[1] != 0){

        newLvl <- get_concept(x = tibble(label = nestedMatch$harmonised, class = columns), ontology = gaz, tree = TRUE)
        newLvl <- nestedMatch %>%
          select(-class) %>%
          left_join(newLvl %>% select(has_broader, class), by = c("id" = "has_broader")) %>%
          group_by(narrower) %>%
          summarise(newClass = unique(class)) %>%
          ungroup()

        nestedMatch <- nestedMatch %>%
          left_join(newLvl, by = "narrower")

        mappings <- get_concept(x = nestedMatch %>% select(label = harmonised, class), ontology = gaz) %>%
          bind_cols(nestedMatch %>% select(new = narrower)) %>%
          mutate(target = label,
                 match = "narrower")
      } else {
        mappings <- tibble(id = character(), has_broader = character(), label = character(),
                           class = character(), new = character(), target = character(), match = character())
      }

      # in case close matches are present, derive the mappings table ...
      if(dim(closeMatch)[1] != 0){

        mappings <- get_concept(x = closeMatch %>% select(label = harmonised, class), ontology = gaz) %>%
          bind_cols(closeMatch %>% select(new = close)) %>%
          mutate(target = label,
                 match = "close") %>%
          bind_rows(mappings)

      }

      # ... and store them in the ontology
      gaz <- new_mapping(new = mappings$new,
                         target = mappings %>% select(id, label = target, class),
                         source = dataseries,
                         match = mappings$match,
                         certainty = 3,
                         ontology = gaz)

      write_rds(x = gaz, file = gazPath)
    }

  }

  # ... and finally extract the respective concepts and merge them with the input table ----
  if(!from_meta){

    for(i in seq_along(columns)){

      theColumn <- columns[i]

      # finally, extract the harmonised concepts ...
      newConcepts <- mappings %>%
        select(new, target, id) %>%
        rename(!!sym(theColumn) := new)

      # ... and assign them to the respective column ...
      if(i == 1){
        temp <- table %>%
          left_join(newConcepts, by = theColumn)
      } else {
        temp <- temp %>%
          left_join(newConcepts, by = c(theColumn, "broader")) %>%
          mutate(broader = code) %>%
          select(-class, -sourceName, -code)
      }

      temp <- temp %>%
        select(-all_of(theColumn)) %>%
        rename(!!theColumn := target)

    }
    out <- temp %>%
      select(all_of(columns), id, everything())

    # ... and store the newly defined matches as a dataseries specific matching table
    if(testFileExists(paste0(intPaths, "/meta/concepts/match_", dataseries, ".csv"))){
      prevMatches <- read_csv(paste0(intPaths, "/meta/concepts/match_", dataseries, ".csv"), col_types = cols(.default = "c"))
    } else {
      prevMatches <- NULL
    }

    if(!is.null(related)){

      newMatches <- mappings %>%
        group_by(id, has_broader, label, class, match) %>%
        summarise(new = paste0(new, collapse = " | ")) %>%
        ungroup() %>%
        pivot_wider(id_cols = c(id, has_broader, label, class), names_from = match, values_from = new) %>%
        mutate(source = dataseries) %>%
        select(id, harmonised = label, class, close, narrower, source) %>%
        distinct(id, harmonised, class, close, narrower, source)

      prevMatches %>%
        bind_rows(newMatches) %>%
        distinct() %>%
        arrange(id) %>%
        write_csv(file = paste0(intPaths, "/meta/concepts/match_", dataseries, ".csv"), append = FALSE, na = "")

    }

    return(out)

  } else {
    message("import successful.")
  }

}
