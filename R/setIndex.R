#' Make and set an index
#'
#' Turn any table into a translation table.
#' @param input [\code{tibble(1)}]\cr a possibly already existing table based on
#'   which a translation table should be constructed.
#' @param output [\code{character(1)}]\cr name of the output file.
#' @param origin [\code{character(1)}]\cr column in \code{input} that contains
#'   the terms that should be mapped to a translation.
#' @param pid [\code{character(1)}]\cr column in \code{input} that contains the
#'   primary ID for the index table. If this is not given, an ID name is created
#'   as \code{paste0(str_sub(output, 1, 3), "ID")}
#' @param target [\code{character(1)}]\cr column in \code{input} that contains
#'   the standadrised terms to which \code{origin} should be mapped.
#' @param source [\code{character(1)}]\cr column in \code{input} that contains
#'   the source/provenance of the table.
#' @details To translate variables or territorial unit names one could use
#'   pre-existing translation tables, such as
#'   \href{https://en.wikipedia.org/wiki/Gazetteer}{gazetteers}. At the start of
#'   any project you would read in the translation tables that are relevant for
#'   your project, in case such tables do already exist. A table should be
#'   created for each variable that occurrs in your project, such as a table for
#'   territorial units, for species or for agricultural commodities.
#'
#'   However, often a translation table doesn't contain all required mappings,
#'   or does not yet exist. Even if the table for a particular variable does not
#'   yet exist, you should create an empty table that could capture upcoming
#'   terms of the respective variable. When terms of a variable are encountered
#'   that can't be matched in any of the provided translation tables, the
#'   function asks you to translate them by hand. This is based on the functions
#'   \code{\link{translateTerms}} and \code{\link{updateIndex}}, however, those
#'   functions don't need to be called manually, they are instead called
#'   automatically, when unknown terms occur.
#' @examples
#' library(readr)
#' setPath(root = "/home/se87kuhe/Nextcloud/LUCKINet/data/")
#'
#' # create index from an already existing table
#' read_csv(file = "species.csv") %>%
#'    setIndex(output = "mySpecies", origin = messySynonyms,
#'             target = scientificName)
#'
#' # create empty table
#' setIndex(output = "exoticVariable")
#' @importFrom checkmate assertCharacter
#' @importFrom stringr str_detect str_locate str_sub
#' @export

setIndex <- function(input = NULL, output = NULL, pid = NULL, origin = NULL, target = NULL,
                     source = NULL){

  # set internal paths
  intPaths <- getOption("dmt_path")

  # check validity of arguments
  assertTibble(x = input, null.ok = TRUE)
  assertCharacter(x = output, any.missing = FALSE)
  if(!is.null(input)){
    if(is.null(pid)){
      idName <- paste0(str_sub(string = "commodities", start = 1, end = 3), "ID")
    } else {
      idName <- pid
    }
    if(is.null(origin)){
      makeLUS <- TRUE
    } else {
      makeLUS <- FALSE
      origin <- as.character(origin)
      assertChoice(x = origin, choices = colnames(input))
    }
    if(is.null(target)){
      assertChoice(x = target, choices = colnames(input))
    } else {
      target <- as.character(target)
      assertChoice(x = target, choices = colnames(input))
    }
    if(!is.null(source)){
      source <- as.character(source)
      isColumn <- testChoice(x = source, choices = colnames(input))
    }
  }

  # if no input table is defined, create a new one
  if(is.null(input)){
    outTT <- tibble(origin = character(), target = character())
    outID <- tibble(!!idName := numeric(), target = character())
  } else {
    # otherwise, select the respective columns
    outTT <- input %>%
      select(origin = !!origin, target = !!target)
    outID <- input %>%
      select(!!idName, target = !!target, everything())
  }

  # construct source for outTT
  if(!any(colnames(outTT) == "source")){
    if(!is.null(source)){
      if(isColumn){
        outTT <- outTT %>%
          rename(source = !!source)
      } else {
        outTT <- outTT %>%
          mutate(source = paste0("makeIndex() from ", source, " on ", Sys.Date()))
      }
    } else {
      outTT <- outTT %>%
        mutate(source = paste0("makeIndex() on ", Sys.Date()))
    }
  }

  # construct ID for outID

  # if origin was not provided, create the look-up section
  if(makeLUS){
    outTT <- outTT %>%
      mutate(origin = NA_character_,
             source = "original") %>%
      select(origin, target, source)
  }


  # if the outputTT name doesn't contain 'tt' at the beginning, add it.
  if(!str_detect(string = output, pattern = "tt")){
    outputTT <- paste0("tt_", output)
  } else {
    loc <- str_locate(string = output, pattern = "tt")
    if(loc[1] != 1){
      outputTT <- paste0("tt_", output)
    }
  }

  # if the outputID name doesn't contain 'id' at the beginning, add it.
  if(!str_detect(string = output, pattern = "id")){
    outputID <- paste0("id_", output)
  } else {
    loc <- str_locate(string = output, pattern = "id")
    if(loc[1] != 1){
      outputID <- paste0("id_", output)
    }
  }

  # write the tables
  write_csv(x = outTT,
            path = paste0(intPaths, "/", outputTT, ".csv"),
            na = "")
  write_csv(x = outID,
            path = paste0(intPaths, "/", outputID, ".csv"),
            na = "")

}