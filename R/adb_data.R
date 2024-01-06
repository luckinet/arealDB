#' Load the data from an areal database
#'
#' @param root [\code{character(1)}]\cr path to the root directory that contains
#'   an areal database.
#' @param ... [\code{character(1)}]\cr named argument, typically of identifying
#'   variables, to filter by.
#' @param variables [\code{character(.)}]\cr columns, typically observed
#'   variables, to select.
#' @param merge [\code{logical(1)}]\cr whether or not to merge tables and
#'   geometries for the specified query. At the default value (FALSE), tables
#'   and geometries are provided as separate files.
#' @param outPath description
#' @param outType [\code{character(1)}]\cr the output file-type, see
#'   \code{\link{st_drivers}} for a list. If a file-type supports layers, they
#'   are stored in the same file, otherwise the different layers are provided
#'   separately. For an R-based workflow, \code{"rds"} could be an efficient
#'   option.
#'
#' @details
#'
#' @return if \code{outPath} and \code{outType} is given, there is no return
#'   value (instead, the resulting object is stored in \code{outPath}),
#'   otherwise ...
#' @examples
#'
#' @importFrom beepr beep
#' @export

adb_data <- function(root, ..., variables = NULL, merge = FALSE,
                     outPath = NULL, outType = NULL){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))





}