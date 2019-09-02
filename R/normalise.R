#' Normlise data tables or geometries
#'
#' This function is a wrapper around the functions \code{\link{normGeometry}}
#' and \code{\link{normTable}}. When called, it automatically processes all
#' files in stage two that are not yet processed.
#' @param what [\code{character(1)}]\cr the data to normalise, either
#'   \code{"geometries"} or \code{"tables"}.
#' @param ... [\code{various(.)}]\cr additional arguments of \code{normGeometry}
#'   and \code{normTable}; see details.
#' @param keepOrig [\code{logical(1)}]\cr whether or not to keep not only the
#'   IDs but also the original terms for which IDs have been derived.
#' @param update [\code{logical(1)}]\cr whether or not the physical files should
#'   be updated (\code{TRUE}) or the function should merely return the new
#'   object (\code{FALSE} default).
#' @param verbose [\code{logical(1)}]\cr be verbose about what is happening
#'   (default \code{TRUE}).
#' @details The argument \code{...} captures all commands that would be provided
#'   to the other \code{norm*} functions. These could be: \itemize{ \item a
#'   subset of administrative units as given by \code{nation}, \code{continent},
#'   \code{region}, \code{subregion} or \code{un_member = TRUE/FALSE}. Valid
#'   values can be found in the object \code{\link{countries}}. \item socalled
#'   'matching lists' that indicate which variables shall be matched with which
#'   index tables and which value should be used as target ID.
#'
#'   targetID = list(variable = targetColumn)
#'
#'   The variable must be present as column in the areal data table and an index
#'   table that is named "id_VARIABLE.csv" must be available in the root
#'   directory of the project. This should have been created with
#'   \code{\link{setVariables}}.}
#' @return see \code{\link{normGeometry}} and \code{\link{normTable}}.
#' @examples
#' \dontrun{
#'
#' # select geometries for the united states
#' normalise(what = "geometries",
#'           nation = c("united states"),
#'           update = TRUE, verbose = FALSE)
#'
#' # match commodities with the fao commodity list and selsect faoID as target ID.
#' normalise(what = "tables",
#'           nation = c("united states"),
#'           faoID = list(commodities = "simpleName"),
#'           update = TRUE, verbose = FALSE)
#' }
#' @importFrom checkmate assertChoice
#' @importFrom readr read_csv
#' @export

normalise <- function(what, ..., keepOrig = TRUE, update = FALSE, verbose = TRUE){

  # what = "geometries"; subsets <- list(nation = "bolivia"); update = TRUE; verbose = FALSE

  assertChoice(x = what, choices = c("tables", "geometries"))

  recent_jobs <- list.files(path = paste0(getOption("adb_path"), "/adb_", what,"/stage2"))
  if(what == "geometries"){
    test_id <- read_csv(paste0(getOption("adb_path"), "/inv_geometries.csv"), col_types = "iiiccccDcc")
  } else {
    test_id <- read_csv(paste0(getOption("adb_path"), "/inv_tables.csv"), col_types = "iiicDcc")
  }
  id_dataseries <- read_csv(paste0(getOption(x = "adb_path"), "/inv_dataseries.csv"), col_types = "icccc")

  for(i in seq_along(recent_jobs)){

    if(recent_jobs[i] %in% test_id$source_file){

      file <- paste0(getOption("adb_path"), "/adb_", what, "/stage2/", recent_jobs[i])

      if(what == "geometries"){

        normGeometry(input = file,
                     update = update,
                     verbose = verbose,
                     ...)

      } else {

        normTable(input = file,
                  ...,
                  keepOrig = keepOrig,
                  update = update,
                  verbose = verbose)

      }
    }

  }


}
