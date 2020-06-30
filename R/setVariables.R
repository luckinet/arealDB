#' Set index and translation tables
#'
#' Use a pre-compiled table to create an index and/or translation table for the
#' target variables of an areal database.
#' @param input [\code{tibble(1)}]\cr a possibly already existing table based on
#'   which the output should be constructed; see details.
#' @param variable [\code{character(1)}]\cr name of the variable and thus of the
#'   output file(s).
#' @param type [\code{character(1)}]\cr the type of table to create, either an
#'   index (\code{"id"}), a translation table (\code{"tt"}) or \code{"both"}
#'   (default).
#' @param origin [\code{character(1)}]\cr column in \code{input} that contains
#'   terms that shall be translated.
#' @param pid [\code{character(1)}]\cr column in \code{input} that contains the
#'   primary ID for the index table. If this is not given, an ID name is created
#'   as \code{paste0(str_sub(variable, 1, 3), "ID")}
#' @param target [\code{character(1)}]\cr column in \code{input} that contains
#'   the standardised terms.
#' @details This is the second function that is run in a project, as it creates
#'   index and translation tables for the target variables that shall be stored
#'   in an areal database. \itemize{ \item An index table relates an ID (given
#'   in \code{pid}) to the variable terms (given in \code{target}) and
#'   potentially to ancillary information. Such tables should be compiled before
#'   a project is started and should contain a clear set of values per variable
#'   (which will be used as standard ontology). \item A translation table
#'   relates terms in foreign languages (given in \code{origin}) to terms in the
#'   target language (given in \code{target}). If target does not exist, the
#'   terms are simply registered as "original" to be used for fuzzy matching.}
#' @return No return value, called for the side effect of writing a table to
#'   the project root directory with name \code{paste0(type, "_", variable,
#'   ".csv")}.
#' @examples
#' library(readr)
#' inPath <- system.file("test_datasets",
#'                       package = "arealDB",
#'                       mustWork = TRUE)
#'
#' # start the example database
#' makeExampleDB(until = "setPath")
#'
#' # create index from an already existing table
#' comm <- read_csv(file = paste0(inPath, "/id_commodities.csv"),
#'                  col_types = "iccc")
#' setVariables(input = comm, variable = "commodities",
#'              pid = "faoID", target = "simpleName")
#' @importFrom checkmate assertCharacter
#' @importFrom stringr str_detect str_locate str_sub
#' @export

setVariables <- function(input = NULL, variable = NULL, type = "both", pid = NULL,
                         origin = NULL, target = NULL){

  # set internal paths
  intPaths <- getOption("adb_path")

  # check validity of arguments
  assertTibble(x = input, null.ok = TRUE)
  assertCharacter(x = variable, any.missing = FALSE)
  assertChoice(x = type, choices = c("both", "tt", "id"))

  # derive some switches
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
  } else {
    makeLUS <- FALSE
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
                      ID = character(),
                      notes = character())
    } else {
      # otherwise, select the respective columns
      outTT <- input %>%
        mutate(source = NA_character_, ID = NA_character_, notes = paste0("setVariables_", Sys.Date())) %>%
        select(origin = !!origin, target = !!target, source, ID, notes)
    }

    # if origin was not provided, create the look-up section
    if(makeLUS){
      outTT <- outTT %>%
        mutate(origin = NA_character_,
               source = "original") %>%
        select(origin, target, source, ID, notes)
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

    updateTable(index = outTT, name = variableTT)
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

    updateTable(index = outID, name = variableID)
  }

}