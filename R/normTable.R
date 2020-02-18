#' Normalise data tables
#'
#' Harmonise and integrate data tables into standardised format
#' @param input [\code{character(1)}]\cr path of the file to normalise.
#' @param ... [\code{list(.)}]\cr matching lists that capture the variables by
#'   which to match and the new column names containing the resulting ID; see
#'   Details.
#' @param pattern [\code{character(1)}]\cr an optional regular expression. Only
#'   dataset names which match the regular expression will be returned.
#' @param update [\code{logical(1)}]\cr whether or not the physical files should
#'   be updated (\code{TRUE}) or the function should merely return the new
#'   object (\code{FALSE} default). This is helpful to check whether the
#'   metadata specification and the provided file(s) are properly specified.
#' @details Arguments in \code{...} are so-called matching lists. This argument
#'   captures three kinds of information: \enumerate{\item the 'variable' that
#'   should be matched with a matching list, \item the 'targetColumn' in that
#'   matching list that should be included in the final table in the place of
#'   'variable' and \item the 'targetID' (column name) of that new variable.}
#'
#'   targetID = list(variable = targetColumn)
#'
#'   'variable' must be present as column in \code{input} and a table that is
#'   named "id_variable.csv" (where 'variable' is replaced by the variable name)
#'   must be available in the root directory of the project. This should have
#'   been created with \code{\link{setVariables}}.
#'
#'   To normalise data tables, this function proceeds as follows: \enumerate{
#'   \item Read in \code{input} and extract initial metadata from the file name.
#'   \item Employ the function \code{rectifyr::\link{reorganise}} to reshape
#'   \code{input} according to the respective schema description (see
#'   \code{rectifyr::\link{schema_default}}). \item Match the territorial units
#'   in \code{input} via the \code{\link{matchUnits}}. \item If \code{...} has
#'   been provided with variables to match, those are matched via
#'   \code{\link{matchVars}}. \item Harmonise territorial unit names. \item If
#'   \code{update = TRUE}, store the processed data table at stage three.}
#' @family normalisers
#' @return This function harmonises and integrates so far unprocessed data
#'   tables at stage two into stage three of the geospatial database.
#' @examples
#' \dontrun{
#'
#' normTable(input = ".../adb_tables/stage2/dataTable.csv",
#'           faoID = list(commodities = "simpleName"),
#'           update = TRUE)
#' }
#' @importFrom checkmate assertNames assertFileExists assertLogical
#' @importFrom rectifyr reorganise
#' @importFrom dplyr mutate select pull
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @importFrom stringr str_split
#' @importFrom tidyselect everything
#' @importFrom utils read.csv
#' @export

normTable <- function(input = NULL, ..., pattern = NULL, update = FALSE){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))

  if(is.null(input)){
    input <- list.files(path = paste0(intPaths, "/adb_tables/stage2"), full.names = TRUE, pattern = pattern)
  } else {
    assertFileExists(x = input, access = "r")
  }

  # get objects
  inv_tables <- read_csv(paste0(intPaths, "/inv_tables.csv"), col_types = "iiiccccDccccc")
  inv_geometries <- read_csv(paste0(intPaths, "/inv_geometries.csv"), col_types = "iiiccccccDDcc")
  vars <- exprs(..., .named = TRUE)

  # check validity of arguments
  assertNames(x = colnames(inv_tables), permutation.of = c("tabID", "geoID", "datID", "source_file",
                                                           "schema", "orig_file", "orig_link", "download_date",
                                                           "next_update", "update_frequency", "metadata_link",
                                                           "metadata_path", "notes"))
  assertLogical(x = update, len = 1)
  assertList(x = vars)

  for(i in seq_along(input)){

    thisInput <- input[i]

    # scrutinize file-name (the fields, which are delimited by "_" carry important information)
    pathStr <- str_split(thisInput, "/")[[1]]
    file_name <- pathStr[length(pathStr)]
    fields <- str_split(file_name, "_")[[1]]

    thisSchema <- inv_tables$schema[inv_tables$source_file == file_name]

    if(!file_name %in% inv_tables$source_file){
      next
    }

    algorithm = readRDS(file = paste0(intPaths, "/adb_tables/meta/schemas/", thisSchema, ".rds"))
    if(!exists(x = "algorithm")){
      stop(paste0("please create the schema desciption '", algorithm, "' for the file '", file_name, "'.\n  --> See '?meta_default' for details"))
    }

    # get some variables
    tabID <- ifelse(length(inv_tables$tabID) == 0, 1,
                    inv_tables$tabID[grep(pattern = file_name, x = inv_tables$source_file)])
    geoID <- ifelse(length(inv_tables$geoID) == 0, 1,
                    inv_tables$geoID[grep(pattern = file_name, x = inv_tables$source_file)])

    message("\n--> reading new data table from '", file_name, "' ...")
    temp <- read.csv(file = thisInput, header = FALSE, as.is = TRUE) %>%
      as_tibble()
    message("--> reorganising data table with '", thisSchema, "' ...")
    temp <- temp %>%
      reorganise(schema = algorithm) %>%
      filter_at(vars(starts_with("al")), all_vars(!is.na(.)))

    # make al1 if it doesn't extist (is needed below for subsetting by nation)
    if(!"al1" %in% names(temp)){
      message("\n--> reconstructing 'al1' ...")
      if(length(fields[1]) == 0){
        stop("the data table '", file_name, "' seems to include several nations but no column for nations (al1).\n Is the schema description correct?")
      } else {
        temp$al1 <- countries$nation[countries$iso_a3 == toupper(fields[1])]
        temp <- temp %>% select(al1, everything())
      }
    }

    out <- temp %>%
      mutate(id = seq_along(year),
             tabID = tabID,
             geoID = geoID) %>%
      matchUnits(source = tabID, keepOrig = TRUE)

    # if a matching list for other variables is defined, match those
    if(length(vars) != 0){
      message()
      out <- out %>%
        matchVars(source = tabID, faoID = list(commodities = "target"), keepOrig = FALSE)
    }

    # in case the user wants to update, update the data table
    if(update){

      theNations <- out %>%
        filter(!is.na(ahID)) %>%
        pull(al1_name) %>%
        unique()

      out <- out %>%
        select(id, tabID, geoID, ahID, everything()) %>%
        mutate(year = as.numeric(year))

      updateData(table = out, nations = theNations, file = thisInput)

    } else{
      out <- out %>%
        select(-starts_with("al"))
      return(out)
    }
  }

}