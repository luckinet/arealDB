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

  # table = temp; columns = unitCols; dataseries = dSeries; ontology = gazPath; from_meta = TRUE; verbose = FALSE

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
  subClasses <- theClasses$class[1:which(theClasses$class == columns)]

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
    missingConcepts <- get_concept(terms = concepts, class = columns, missing = TRUE, ontology = gaz)
    inclConcepts <- get_concept(terms = concepts, class = columns, ontology = gaz)

    # build a table of external concepts and of harmonised concepts these should be overwritten with
    if(dim(missingConcepts)[1] != 0){

      relate <- gaz@labels %>%
        select(code, harmonised = label_en, class) %>%
        left_join(inclConcepts %>% select(code, harmonised = label_en, class, close = external),
                  by = c("code", "harmonised", "class")) %>%
        filter(!is.na(class)) %>%
        mutate(#code = str_replace_all(string = code, "[.]", "_"),
               nested = NA_character_,
               sort_in = NA_character_) %>%
        filter(class %in% subClasses)

      sortIn <- missingConcepts %>%
        mutate(code = NA_character_,
               harmonised = NA_character_,
               class = NA_character_,
               close = NA_character_,
               nested = NA_character_) %>%
        select(code, harmonised, class, close, nested, sort_in = external)

      # put together the object that shall be edited by the user ...
      sortIn %>%
        bind_rows(relate) %>%
        write_csv(file = paste0(intPaths, "/matching.csv"), quote = "all", na = "")

      # ... and make them aware of their duty
      message("please edit the file '", paste0(intPaths, "/matching.csv"), "'")
      if(verbose){
        message("column description and tips \n\ncode       : filter by this column to jump to the subset you need to edit\nharmonised : concepts to which the new terms should be related \nclass      : the class of harmonised concepts \nclose      : in case a new concept is a close match to the harmonised concept, paste \n             it next to that concept, delimit several concepts with a '|' \nnested     : in case a new concept is not the same as any harmonised concept, paste \n             it next to that concept into which it is nested, delimit \n             several concepts with a '|' \nsort_in    : cut out these concepts and sort them either into 'close' or into 'nested' \n\n-> values that were already successfully matched by previous translations are listed here, \n   however, altering them here doesn't change the ontology. \n\n-> any row that doesn't contain a value in the column 'code' will be discarded. Hence, \n   if you want a value to be ignored, simply don't paste it anywhere. \n\n-> do not change the values in the columns 'code', 'harmonised' and 'class', as they \n   are important to insert the new matches into the ontology. \n\n-> if a term shall be nested into a position that doesn't have a class, (for example, \n   because that class occurrs the first time with this term) first create that nested \n   class with 'new_class()'.")
      }
      done <- readline(" -> press any key when done: ")

    } else {
      # write an empty table
      tibble(code = NA_character_,
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
        filter(!is.na(code))

      if(dim(related)[1] == 0){
        related <- NULL
      }
    }

    if(!is.null(related)){

      # make a new dataseries, in case it doesn't exist yet
      if(!dataseries %in% gaz@sources$sourceName){
        gaz <- new_source(name = dataseries, ontology = gaz)
      }

      # in case there are concepts that are merely nested into other concepts,
      # set these as new harmonised concepts
      if(any(!is.na(related$nested))){

        temp <- related %>%
          filter(!is.na(nested)) %>%
          separate_rows(nested, sep = "\\|") %>%
          select(-close)

        newLvl <- get_concept(terms = temp$harmonised, class = columns, ontology = gaz, tree = TRUE)
        newLvl <- temp %>%
          select(-class) %>%
          left_join(newLvl %>% select(broader, class), by = c("code" = "broader")) %>%
          group_by(nested) %>%
          summarise(newClass = unique(class)) %>%
          ungroup()

        # continue here by testing some more scenarios

        temp <- temp %>%
          left_join(newLvl, by = "nested")

        gaz <- get_concept(terms = temp$harmonised, ontology = gaz) %>%
          new_concept(new = temp$nested,
                      broader = .,
                      class = temp$newClass,
                      source = dataseries,
                      ontology = gaz)

      }

      # in case there are close matches with already harmonised concept, set the
      # respective mappings
      if(any(!is.na(related$close))){

        temp <- related %>%
          filter(!is.na(close)) %>%
          separate_rows(close, sep = "\\|")

        gaz <- get_concept(terms = temp$harmonised, class = columns, ontology = gaz) %>%
          new_mapping(concept = .,
                      new = temp$close,
                      match = "close",
                      source = dataseries,
                      certainty = 3,
                      ontology = gaz)

      }

      write_rds(x = gaz, file = gazPath)
    }


  }

  # ... and finally extract the respective concepts and merge them with the input table ----
  if(!from_meta){


    for(i in seq_along(columns)){

      theColumn <- columns[i]
      theConcepts <- table %>%
        as_tibble() %>%
        select(all_of(theColumn)) %>%
        distinct() %>%
        arrange(!!sym(theColumn))

      # finally, extract the harmonised concepts ...
      newConcepts <- get_concept(terms = unlist(theConcepts, use.names = FALSE), class = columns, ontology = gaz) %>%
        rename(!!sym(theColumn) := external)

      # ... and assign them to the respective column ...
      if(i == 1){
        temp <- table %>%
          left_join(newConcepts, by = theColumn) %>%
          mutate(broader = code) %>%
          select(-class, -sourceName, -code)
      } else {
        temp <- temp %>%
          left_join(newConcepts, by = c(theColumn, "broader")) %>%
          mutate(broader = code) %>%
          select(-class, -sourceName, -code)
      }

      temp <- temp %>%
        select(-all_of(theColumn)) %>%
        rename(!!theColumn := label_en)

    }
    table <- temp %>%
      select(all_of(columns), code = broader, everything())

    # ... and store the newly defined matches as a dataseries specific matching table
    if(testFileExists(paste0(intPaths, "/meta/concepts/match_", dataseries, ".csv"))){
      prevMatches <- read_csv(paste0(intPaths, "/meta/concepts/match_", dataseries, ".csv"), col_types = cols(.default = "c"))
    } else {
      prevMatches <- NULL
    }

    if(!is.null(related)){

      allNewConcepts <- get_concept(terms = concepts, class = columns, ontology = gaz)

      newMatches <- related %>%
        select(-sort_in, -code, -class) %>%
        filter(!is.na(close) | !is.na(nested)) %>%
        # separate_rows(close, sep = "\\|") %>%
        left_join(allNewConcepts, by = c("harmonised" = "label_en")) %>%
        # filter(!is.na(code)) %>%
        # separate(col = external, into = c("source", "match"), sep = "_", extra = "drop") %>%
        mutate(source = dataseries) %>%
        # select(code, harmonised, class, close, nested, source, match)
        select(code, harmonised, class, close, nested, source) %>%
        distinct(code, harmonised, class, close, nested, source)

      prevMatches %>%
        bind_rows(newMatches) %>%
        group_by(code, harmonised, class, close, source) %>%
        mutate(nested = paste0(na.omit(nested), collapse = "|")) %>%
        ungroup() %>%
        distinct() %>%
        arrange(code) %>%
        write_csv(file = paste0(intPaths, "/meta/concepts/match_", dataseries, ".csv"), append = FALSE, na = "")

    }

    return(table)

  } else {
    message("import successful.")
  }

}
