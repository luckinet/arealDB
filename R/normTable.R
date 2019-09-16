#' Normalise data tables
#'
#' Harmonise and integrate data tables into standardised format
#' @param input [\code{character(1)}]\cr path of the file to normalise.
#' @param ... [\code{list(.)}]\cr matching lists that capture the variables by
#'   which to match and the new column names containing the resulting ID; see
#'   Details.
#' @param keepOrig [\code{logical(1)}]\cr whether or not to keep not only the
#'   IDs but also the original terms for which IDs have been derived.
#' @param update [\code{logical(1)}]\cr whether or not the physical files should
#'   be updated (\code{TRUE}) or the function should merely return the new
#'   object (\code{FALSE} default).
#' @param verbose [\code{logical(1)}]\cr be verbose about what is happening
#'   (default \code{TRUE}).
#' @details Arguments in \code{...} are so-called matching lists that indicate
#'   with which target column variables shall be matched and which value should
#'   be used as target ID.
#'
#'   targetID = list(variable = targetColumn)
#'
#'   The variable must be present as column in \code{input} and a table that is
#'   named "id_VARIABLE.csv" must be available in the root directory of the
#'   project. This should have been created with \code{\link{setVariables}}.
#'
#'   To normalise data tables, this function proceeds as follows: \enumerate{
#'   \item Read in \code{input} and extract initial metadata from the file name.
#'   \item Employ the functions \code{\link{record}} and
#'   \code{\link{reorganise}} to reshape \code{input} according to the
#'   respective schema description (see \code{\link{meta_default}}). \item Match
#'   the territorial units in \code{input} via the \code{\link{matchUnits}}.
#'   \item If \code{...} has been provided with variables to match, those are
#'   matched via \code{\link{matchVars}}. \item Harmonise territorial unit
#'   names. \item If \code{update = TRUE}, store the processed data table at
#'   stage three.}
#' @family normalisers
#' @return This function integrates unprocessed data tables at stage two into
#'   the geospatial database.
#' @examples
#' \dontrun{
#'
#' normTable(input = ".../adb_tables/stage2/dataTable.csv",
#'           faoID = list(commodities = "simpleName"),
#'           update = TRUE, verbose = FALSE)
#' }
#' @importFrom checkmate assertDataFrame assertNames assertFileExists
#'   assertIntegerish assertLogical
#' @importFrom dplyr mutate select pull
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @importFrom stringr str_split
#' @importFrom tidyselect everything
#' @export

normTable <- function(input, ..., keepOrig = TRUE, update = FALSE, verbose = TRUE){

  # get objects
  inv_tables <- read_csv(paste0(getOption(x = "adb_path"), "/inv_tables.csv"), col_types = "iiicDcc")
  vars <- exprs(..., .named = TRUE)

  # check validity of arguments
  assertNames(x = colnames(inv_tables), permutation.of = c("tabID", "datID", "geoID", "source_file", "date", "orig_file", "notes"))
  assertFileExists(x = input, access = "r")
  assertLogical(x = update, len = 1)
  assertLogical(x = verbose, len = 1)
  assertList(x = vars)

  # scrutinize file-name (the fields, which are delimited by "_" carry important information)
  pathStr <- str_split(input, "/")[[1]]
  file_name <- pathStr[length(pathStr)]
  fields <- str_split(file_name, "_")[[1]]
  algorithm = paste0("meta_", fields[4], fields[6])
  if(!exists(algorithm)){
    stop(paste0("please create the meta data object '", algorithm, "' for the file '", file_name, "'.\n  --> See '?record' for details"))
  }

  # get some variables
  tabID <- ifelse(length(inv_tables$tabID) == 0, 1,
                  inv_tables$tabID[grep(pattern = file_name, x = inv_tables$source_file)])
  geoID <- ifelse(length(inv_tables$geoID) == 0, 1,
                  inv_tables$geoID[grep(pattern = file_name, x = inv_tables$source_file)])

  out <- read_csv(input, col_names = FALSE) %>%
    reorganise(schema = algorithm) %>%
    mutate(id = seq_along(year),
           tabID = tabID,
           geoID = geoID) %>%
    matchUnits(source = geoID, keepOrig = keepOrig)

  # if a matching list for other variables is defined, match those
  if(length(vars) != 0){
    out <- out %>%
      matchVars(source = tabID, vars, keepOrig = keepOrig)
  }

  # in case the user wants to update, update the data table
  if(update){

    theNations <- out %>%
      filter(!is.na(ahID)) %>%
      pull(al1_alt) %>%
      unique() #%>%
      # unifyNations()

    out <- out %>%
      select(-al1_alt)

    updateData(table = out, nations = theNations, file = pathStr)

  } else{
    out <- out %>%
      select(-al1_alt)
  }

}