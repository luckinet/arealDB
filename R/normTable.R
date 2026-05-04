#' Normalise data tables
#'
#' Harmonise and integrate data tables into standardised format
#' @param input [`character(1)`][character]\cr path of the file to normalise. If
#'   this is left empty, all files at stage two as subset by \code{pattern} are
#'   chosen.
#' @param pattern [`character(1)`][character]\cr an optional regular expression.
#'   Only dataset names which match the regular expression will be processed.
#' @param query [`character(1)`][character]\cr the expression that would be used
#'   in \code{\link[dplyr]{filter}} to subset a tibble in terms of the columns
#'   defined via the schema and given as a single character string, such as
#'   \code{"ADM0 == 'a_nation'"}.
#' @param ontoMatch [`character(.)`][character]\cr name of the column(s) that
#'   shall be matched with an ontology (defined in \code{\link{adb_init}}).
#' @param simplify [`list(.)`][list]\cr list of modules to simplify character
#'   strings before matching with the ontology. Possible modules are: \itemize{
#'      \item \code{"squish"}: logical of whether or not to replace duplicate whitespaces with single whitespaces,
#'      \item \code{"lowercase"}: logical of whether or not to set all strings to lower case,
#'      \item \code{"dashes"}: logical of whether or not to set all [dashes](https://en.wikipedia.org/wiki/Dash#Unicode) and hyphens to \code{-} (unicode symbol U+002D),
#'      \item \code{"duplpunct"}: logical of whether or not to remove duplicated punctation symbols (such as "- -" or `//`),
#'      \item \code{"remove"}: vector of symbols or regular expressions to remove,
#'      \item \code{"replace"}: vector of two elements, the first contains a string to match, the second a string to replace the former with,
#'      \item \code{"chartr"}: vector of two elements, the first contains n symbols to match, the second n symbols to replace the former with (see [chartr]).
#'   }
#'   These simplifications are aimed at reducing the need for manual
#'   translations, for example, when it's known that the tables of a dataseries
#'   have a lot of very similar symbols confused (for example en dash –, em
#'   dash — or minus sign −) or systematic deviations (for example " - "
#'   instead of merely "-") that could be rectified that way.
#' @param beep [`integerish(1)`][integer]\cr Number specifying what sound to be
#'   played to signal the user that a point of interaction is reached by the
#'   program, see \code{\link[beepr]{beep}}.
#' @param verbose [`logical(1)`][logical]\cr be verbose about translating terms
#'   (default \code{FALSE}). Furthermore, you can use
#'   \code{\link{suppressMessages}} to make this function completely silent.
#' @details To normalise data tables, this function proceeds as follows:
#'   \enumerate{
#'      \item Read in \code{input} and extract initial metadata from the file name.
#'      \item Employ the function [tabshiftr::reorganise()] to reshape \code{input} according to the respective schema description.
#'      \item The territorial names are matched with the gazetteer to harmonise new territorial names (at this step, the function might ask the user to edit the file 'matching.csv' to align new names with already harmonised names). \item Harmonise territorial unit names.
#'      \item store the processed data table at stage three.
#'   }
#' @family normalise functions
#' @return This function harmonises and integrates so far unprocessed data
#'   tables at stage two into stage three of the areal database. It produces for
#'   each main polygon (e.g. nation) in the registered data tables a file that
#'   includes all thematic areal data.
#' @examples
#' if(dev.interactive()){
#'   # build the example database
#'   adb_example(until = "normGeometry", path = tempdir())
#'
#'   # normalise all available data tables ...
#'   normTable()
#'
#'   # provide some options to simplify
#'   normTable(simplify = list(whitespace = TRUE,
#'                             replace = c("--", "-"),
#'                             chartr = c("æÜé", "aUe")))
#'
#'   # ... and check the result
#'   output <- read_parquet(paste0(tempdir(), "/tables/stage3/a_nation.parquet"))
#' }
#' @importFrom checkmate assertNames assertFileExists assertLogical assertList
#' @importFrom readr read_csv
#' @importFrom rlang exprs :=
#' @importFrom tabshiftr reorganise
#' @importFrom dplyr mutate select pull full_join bind_rows filter left_join distinct arrange na_if
#' @importFrom magrittr %>%
#' @importFrom readr read_csv cols
#' @importFrom stringr str_split str_detect str_replace_all
#' @importFrom tidyselect everything
#' @importFrom utils read.csv
#' @importFrom arrow read_parquet write_parquet
#' @importFrom beepr beep
#' @export

normTable <- function(input = NULL, pattern = NULL, query = NULL, ontoMatch = NULL,
                      simplify = NULL, beep = NULL, verbose = FALSE){

  # set internal paths
  intPaths <- .adb_state$path
  gazName <- "gazetteer"

  # get territorial context
  load(paste0(intPaths, "/db_info.RData"))
  topClass <- db_info$level
  topUnits <- .read_terms(gazName) %>%
    filter(class == topClass) %>%
    arrange(label)

  if(is.null(input)){
    input <- list.files(path = paste0(intPaths, "/tables/stage2"), full.names = TRUE, pattern = pattern)
  } else {
    assertFileExists(x = input, access = "r")
  }

  # set internal objects
  moveFile <- TRUE

  # get tables
  inventory <- readRDS(paste0(intPaths, "/inventory.rds"))
  inv_dataseries <- inventory$dataseries
  inv_tables <- inventory$tables

  # check validity of arguments
  assertCharacter(x = query, len = 1, null.ok = TRUE)
  assertNames(x = colnames(inv_tables),
              permutation.of = c("tabID", "datID", "geoID", "geography", "level", "start_period", "end_period", "stage2_name", "schema", "stage1_name", "stage1_url", "download_date", "update_frequency", "metadata_url", "metadata_path", "status", "notes"))
  assertNames(x = colnames(inv_dataseries),
              permutation.of = c("datID", "name", "description", "homepage", "version", "licence_link", "notes"))
  assertCharacter(x = ontoMatch, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  assertList(x = simplify, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  if(!is.null(simplify)){
    assertNames(x = names(simplify), subset.of = c("squish", "lowercase", "dashes", "duplpunct", "remove", "replace", "chartr"))
  }

  ret <- NULL
  for(i in seq_along(input)){

    thisInput <- input[i]

    # scrutinize file-name (the fields, which are delimited by "_" carry important information)
    pathStr <- str_split(thisInput, "/")[[1]]
    file_name <- pathStr[length(pathStr)]
    fields <- str_split(file_name, "_")[[1]]

    if(getOption("width")-(nchar(i)+nchar(length(input))+21+nchar(file_name)) <= 0){
      stop("please increase the width of the console, or choose smaller file names.")
    }

    if(!file_name %in% inv_tables$stage2_name){
      message("\n--- ", i, " / ", length(input), " skipping ", rep("-", times = getOption("width")-(nchar(i)+nchar(length(input))+21+nchar(file_name))), " ", file_name, " ---")
      next
    } else {
      message("\n--- ", i, " / ", length(input), " ", rep("-", times = getOption("width")-(nchar(i)+nchar(length(input))+13+nchar(file_name))), " ", file_name, " ---")
    }

    # get some variables
    lut <- inv_tables[grep(pattern = paste0("^", file_name, "$"), x = inv_tables$stage2_name),]
    if(file_name %in% lut$stage2_name){
      geoID <- lut$geoID
      tabID <- lut$tabID
      datID <- lut$datID
      thisSchema <- lut$schema

      gSeries <- inv_dataseries$name[inv_dataseries$datID == geoID]
      dSeries <- inv_dataseries$name[inv_dataseries$datID == datID]
    } else {
      stop(paste0("  ! the file '", file_name, "' has not been registered yet."))
    }

    algorithm <- readRDS(file = paste0(intPaths, "/tables/schemas/", thisSchema, ".rds"))
    if(!exists(x = "algorithm")){
      stop(paste0("please create the schema desciption '", algorithm, "' for the file '", file_name, "'.\n  --> See '?meta_default' for details"))
    }

    # reorganise data
    message("\n--> reading new data table ...")
    thisTable <- read.csv(file = thisInput, header = FALSE, strip.white = TRUE, as.is = TRUE, colClasses = "character", na.strings = algorithm@format$na, encoding = "UTF-8") %>%
      as_tibble() %>%
      mutate(across(where(is.character), ~na_if(x = ., y = "")))

    message("    reorganising table with '", thisSchema, "' ...")
    thisTable <- thisTable %>%
      reorganise(schema = algorithm)

    if(!is.null(query)){
      moveFile <- FALSE
      thisTable <- thisTable %>%
        filter(eval(parse(text = query)))
    }

    message("    harmonizing territory names ...")
    outCols <- .read_levels(gazName) %>%
      pull(label)
    targetCols <- outCols[outCols %in% colnames(thisTable)]
    outCols <- outCols[which(outCols %in% head(targetCols, 1)) : which(outCols %in% tail(targetCols, 1))]

    if(targetCols[1] != topClass){
      theUnits <- str_split(string = lut$stage2_name, pattern = "_")[[1]][1]
      # resolve raw source label to canonical gazetteer label
      unitMappings <- .read_mappings(gazName, gSeries) %>%
        filter(source == gSeries, !grepl("class_label", note) | is.na(note))
      topTerms <- .read_terms(gazName) %>% filter(class == topClass)
      unitMap <- unitMappings %>%
        filter(canonical_id %in% topTerms$id) %>%
        left_join(topTerms %>% select(id, label), by = c("canonical_id" = "id")) %>%
        select(source_label, canonical_label = label) %>%
        distinct()
      raw_stripped <- str_replace_all(trimws(theUnits), "[.]", "")
      match_row <- unitMap %>% filter(source_label == raw_stripped)
      if(nrow(match_row) == 1){
        theUnits <- match_row$canonical_label
      }
    } else {
      theUnits <- NULL
    }

    if(!topClass %in% targetCols){
      thisTable <- thisTable %>%
        add_column(tibble(!!topClass := theUnits), .before = targetCols[1])
      targetCols <- c(topClass, targetCols)
    }

    thatTable <- .matchOntology(table = thisTable,
                                columns = targetCols,
                                dataseries = dSeries,
                                ontology = gazName,
                                verbose = verbose,
                                beep = beep) %>%
      unite(col = "gazMatch", match, external, sep = "--", na.rm = TRUE) %>%
      rename(gazID = id, gazClass = class) %>%
      select(-any_of(c("has_broader", "description"))) %>%
      mutate(gazID = str_replace_all(string = gazID, pattern = "[.]", replacement = "-"))

    if(!is.null(ontoMatch)){
      message("    harmonizing thematic concepts ...")
      assertNames(x = ontoMatch, subset.of = names(thatTable))
      thatTable <- .matchOntology(table = thatTable,
                                  columns = ontoMatch,
                                  dataseries = dSeries,
                                  ontology = ontoMatch,
                                  colsAsClass = FALSE,
                                  simplify = simplify,
                                  beep = beep,
                                  verbose = verbose) %>%
        unite(col = "ontoMatch", match, external, sep = "--", na.rm = TRUE) %>%
        rename(ontoID = id, ontoName = all_of(ontoMatch), ontoClass = class) %>%
        select(ontoID, ontoID, ontoName, ontoMatch, ontoClass, everything(), -any_of(c("has_broader", "description")))
    }

    thatTable <- thatTable %>%
      mutate(gazID = str_replace_all(string = gazID, pattern = "-", replacement = "."),
             tabID = tabID,
             geoID = geoID)

    # derive output
    if(is.null(theUnits)){
      theUnits <- unique(eval(expr = parse(text = targetCols[1]), envir = thatTable)) %>%
        na.omit() %>%
        as.character()
    }

    for(j in seq_along(theUnits)){

      if(length(theUnits) != 1){
        tempOut <- thatTable %>%
          filter(.data[[targetCols[1]]] == theUnits[j])
      } else {
        tempOut <- thatTable
      }
      tempOut <- tempOut %>%
        unite(col = "gazName", all_of(sort(unique(c(targetCols, outCols)))), sep = ".", na.rm = TRUE) %>%
        mutate(gazName = if_else(gazName == "", NA, gazName)) %>%
        select(tabID, geoID, gazID, gazName, gazMatch, gazClass, everything()) %>%
        distinct()

      # append output to previous file
      outFile <- theUnits[j] %>%
        str_replace(pattern = "\\(", replacement = "\\\\(") %>%
        str_replace(pattern = "\\)", replacement = "\\\\)")
      avail <- list.files(path = paste0(intPaths, "/tables/stage3/"), pattern = paste0("^", outFile, ".parquet"))

      if(length(avail) == 1){

        prevData <- as_tibble(read_parquet(file = paste0(intPaths, "/tables/stage3/", theUnits[j], ".parquet"), mmap = FALSE))


        out <- tempOut %>%
          bind_rows(prevData, .) %>%
          distinct()

      } else if(length(avail) > 1){
        stop("the nation '", theUnits[j], "' exists several times in the output folder '/adb_tablse/stage3/'.")
      } else {
        out <- tempOut
      }

      write_parquet(x = out, sink = paste0(intPaths, "/tables/stage3/", theUnits[j], ".parquet"))

      ret <- bind_rows(ret, out)
    }

    if(moveFile){
      inventory$tables$status[inventory$tables$stage2_name == file_name] <- "normalised"
      saveRDS(object = inventory, file = paste0(intPaths, "/inventory.rds"))
    }

    gc()

  }

  if(!is.null(beep)) beep(beep)
  invisible(ret)

}
