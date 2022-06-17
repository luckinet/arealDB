#' Match concepts in a table with a gazetteer
#'
#' @param table [\code{character(1)}]\cr a table that contains columns that should be harmonised by matching it with the gazetteer.
#' @param columns [\code{character(1)}]\cr the columns containing the concepts
#' @param dataseries [\code{character(1)}]\cr
#' @param from_meta [\code{logical(1)}]\cr
#' @importFrom ontologics get_concept new_concept set_mapping
#' @importFrom purrr map
#' @importFrom stringr str_replace_all
#' @importFrom dplyr bind_cols
#' @importFrom tidyr separate_rows separate
#' @importFrom sf st_drop_geometry
#' @importFrom utils tail
#' @export

match_gazetteer <- function(table = NULL, columns = NULL, dataseries = NULL, from_meta = FALSE){

  # table = newGeom; columns = unitCols; dataseries = dSeries; from_meta = FALSE

  intPaths <- paste0(getOption(x = "adb_path"))
  gazPath <- paste0(getOption(x = "gazetteer_path"))

  gaz <- read_rds(gazPath)
  harmonised <- gaz$mappings
  theClasses <- unique(harmonised$class)

  if(from_meta){

    toMatch <- list.files(paste0(intPaths, "/meta/concepts/"), full.names = TRUE)

  } else {

    toMatch <- "table"

    concepts <- map(.x = seq_along(columns), .f = function(ix){

      table %>%
        pull(columns[ix]) %>%
        unique()

    }) %>%
      unlist()

    explanation <- tibble(code = c("filter by this column to jump \nto the subset you need to edit", "", "code"),
                          harmonised = c("concepts to which the new \nterms should be related", "", "harmonised"),
                          class = c("the class of harmonised concepts", "", "class"),
                          close = c("in case a new concept is a \nclose match to the harmonised concept, \npaste it next to that concept", "", "close"),
                          nested = c("in case a new concept is not \nthe same as any harmonised concept, \npaste it next to that concept \ninto which it is nested", "", "nested"),
                          sort_in = c("cut out these concepts and sort them \neither into 'close' or into 'nested'", "", "sort_in"))

    # determine those concepts, that are not yet defined in the gazetteer
    missingConcepts <- get_concept(terms = concepts, missing = TRUE, path = gazPath)

    # build a table of external concepts and of harmonised concepts these should be overwritten with
    if(dim(missingConcepts)[1] != 0){

      relate <- harmonised %>%
        select(code, harmonised = label_en, class) %>%
        mutate(code = paste0("_", str_replace_all(string = code, "[.]", "_")),
               close = NA_character_,
               nested = NA_character_,
               sort_in = NA_character_)

      sortIn <- tibble(code = NA_character_,
                       harmonised = NA_character_,
                       class = NA_character_,
                       close = NA_character_,
                       nested = NA_character_,
                       sort_in = missingConcepts)

      # put together the object that shall be edited by the user ...
      explanation %>%
        bind_rows(sortIn) %>%
        bind_rows(relate) %>%
        write_csv(file = paste0(intPaths, "/matching.csv"), quote = "all", na = "", col_names = FALSE)

      # ... and make them aware of their duty
      message("please edit the file '", paste0(intPaths, "/matching.csv"), "'")
      done <- readline(" -> press any key when done: ")

    } else {
      # write an empty table
      tibble(code = c("", "", "code"),
             note = c("", "", "nothing to match for the current table")) %>%
        write_csv(file = paste0(intPaths, "/matching.csv"), quote = "all", na = "", col_names = FALSE)
    }

  }

  for(i in seq_along(toMatch)){

    if(from_meta){
      related <- read_csv(toMatch[i], col_types = cols(.default = "c"))

      dataseries <- tail(str_split(toMatch[i], "/")[[1]], 1)
      dataseries <- str_split(dataseries, "_")[[1]]
      dataseries <- str_split(dataseries[2], "[.]")[[1]][1]
    } else {
      related <- read_csv(paste0(intPaths, "/matching.csv"), col_types = cols(.default = "c"), skip = 2) %>%
        filter(!is.na(code))

      if(dim(related)[1] == 0){
        related <- NULL
      }
    }

    if(!is.null(related)){

      # in case there are concepts that are merely nested into other concepts,
      # set these as new harmonised concepts
      if(any(!is.na(related$nested))){

        nestedConcepts <- related %>%
          filter(!is.na(nested)) %>%
          separate_rows(nested, sep = ", ") %>%
          mutate(class = theClasses[which(theClasses %in% class) + 1])

        new_concept(new = nestedConcepts$nested,
                    broader = nestedConcepts$harmonised,
                    class = nestedConcepts$class,
                    source = dataseries,
                    path = gazPath)

        related <- related %>%
          bind_rows(tibble(harmonised = nestedConcepts$nested,
                           close = nestedConcepts$nested))

      }

      # in case there are close matches with already harmonised concept, set the
      # respective mappings
      if(any(!is.na(related$close))){

        temp <- related %>%
          filter(!is.na(close))

        set_mapping(concept = temp$harmonised,
                    external = temp$close,
                    match = "close",
                    source = dataseries,
                    certainty = 3,
                    path = gazPath)

      }
    }

  }

  if(!from_meta){


    for(i in seq_along(columns)){

      theColumn <- columns[i]
      theConcepts <- table %>%
        as_tibble() %>%
        select(all_of(theColumn)) %>%
        distinct() %>%
        arrange(!!sym(theColumn))

      # finally, extract the harmonised concepts ...
      newConcepts <- get_concept(terms = unlist(theConcepts, use.names = FALSE), path = gazPath) %>%
        rename(!!sym(theColumn) := external)

      # ... and assign them to the respective column ...
      if(i == 1){
        temp <- table %>%
          left_join(newConcepts, by = theColumn) %>%
          mutate(broader = code) %>%
          select(-class, -source, -code)
      } else {
        temp <- temp %>%
          left_join(newConcepts, by = c(theColumn, "broader")) %>%
          mutate(broader = code) %>%
          select(-class, -source, -code)
      }

      temp <- temp %>%
        select(-theColumn) %>%
        rename(!!theColumn := label_en)

    }
    table <- temp %>%
      # separate(col = broader, into = paste0(columns, "_id"), sep = "[.]") %>%
      select(columns, code = broader, everything())

    # ... and store the newly defined matches as a dataseries specific matching table
    if(testFileExists(paste0(intPaths, "/meta/concepts/match_", dataseries, ".csv"))){
      prevMatches <- read_csv(paste0(intPaths, "/meta/concepts/match_", dataseries, ".csv"), col_types = cols(.default = "c"))
    } else {
      prevMatches <- NULL
    }

    if(!is.null(related)){

      newMatches <- related %>%
        select(-sort_in, -code, -class) %>%
        filter(!is.na(close) | !is.na(nested)) %>%
        left_join(newConcepts, by = c("harmonised" = "label_en")) %>%
        separate(col = external, into = c("source", "match"), sep = "_", extra = "drop") %>%
        mutate(code = paste0("_", str_replace_all(string = code, "[.]", "_")),
               source = dataseries) %>%
        select(code, harmonised, class, close, nested, source, match)

      prevMatches %>%
        bind_rows(newMatches) %>%
        distinct() %>%
        write_csv(file = paste0(intPaths, "/meta/concepts/match_", dataseries, ".csv"), append = FALSE, na = "")

    }

    return(table)

  } else {
    message("import successful.")
  }

}
