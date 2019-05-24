#' Make and set index and translation tables
#'
#' Turn a table into a index and translation table.
#' @param input [\code{tibble(1)}]\cr a possibly already existing table based on
#'   which the output should be constructed.
#' @param variable [\code{character(1)}]\cr name of the variable and output file(s).
#' @param type [\code{character(1)}]\cr the type of table to create, either an
#'   index (\code{"id"}), a translation table (\code{"tt"}) or \code{"both"}
#'   (default).
#' @param origin [\code{character(1)}]\cr column in \code{input} that contains
#'   the terms that should be mapped to a translation.
#' @param pid [\code{character(1)}]\cr column in \code{input} that contains the
#'   primary ID for the index table. If this is not given, an ID name is created
#'   as \code{paste0(str_sub(variable, 1, 3), "ID")}
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
#'    setTables(variable = "mySpecies", origin = messySynonyms,
#'             target = scientificName)
#'
#' # create empty table
#' setTables(variable = "exoticVariable")
#' @importFrom checkmate assertCharacter
#' @importFrom stringr str_detect str_locate str_sub
#' @export

setTables <- function(input = NULL, variable = NULL, type = "both", pid = NULL,
                     origin = NULL, target = NULL){

  # set internal paths
  intPaths <- getOption("cT_path")

  # check validity of arguments
  assertTibble(x = input, null.ok = TRUE)
  assertCharacter(x = variable, any.missing = FALSE)
  assertChoice(x = type, choices = c("both", "tt", "id"))

  # derive some switched
  if(!is.null(input)){
    if(is.null(pid)){
      idName <- paste0(str_sub(string = variable, start = 1, end = 3), "ID")
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
  }

  if(type == "both"){
    makeTT <- makeID <- TRUE
  } else if(type == "id"){
    makeTT <- FALSE
    makeID <- TRUE
  } else {
    makeTT <- TRUE
    makeID <- FALSE
  }

  # if no translation table exists, create one
  if(makeTT){
    if(is.null(input)){
      # if no input table is defined, use an empty table
      outTT <- tibble(origin = character(),
                      target = character(),
                      source = character(),
                      date = date(),
                      cenID = character())
    } else {
      # otherwise, select the respective columns
      outTT <- input %>%
        mutate(source = "setTables()", date = Sys.Date(), cenID = NA_character_) %>%
        select(origin = !!origin, target = !!target, source, date, cenID)
    }

    # if origin was not provided, create the look-up section
    if(makeLUS){
      outTT <- outTT %>%
        mutate(origin = NA_character_,
               source = "original") %>%
        select(origin, target, source, date, cenID)
    }

    # if the variableTT name doesn't contain 'tt' at the beginning, add it.
    if(!str_detect(string = variable, pattern = "tt")){
      variableTT <- paste0("tt_", variable)
    } else {
      loc <- str_locate(string = variable, pattern = "tt")
      if(loc[1] != 1){
        variableTT <- paste0("tt_", variable)
      }
    }

    updateIndex(index = outTT, name = variableTT)
  }

  # if no index exists, create one
  if(makeID){
    if(is.null(input)){
      # if no input table is defined, use an empty table
      outID <- tibble(!!idName := numeric(), target = character())
    } else {
      # otherwise, select the respective columns
      outID <- input %>%
        select(!!idName, target = !!target, everything())
    }

    # if the variableID name doesn't contain 'id' at the beginning, add it.
    if(!str_detect(string = variable, pattern = "id")){
      variableID <- paste0("id_", variable)
    } else {
      loc <- str_locate(string = variable, pattern = "id")
      if(loc[1] != 1){
        variableID <- paste0("id_", variable)
      }
    }

    updateIndex(index = outID, name = variableID)
  }

}