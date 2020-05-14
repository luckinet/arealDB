#' Update a table
#'
#' Update any inventory, index or translation table of a geospatial
#' database.
#' @param index [\code{tibble(1)}]\cr the table to use as update.
#' @param name [\code{character(1)}]\cr name of the table that shall be updated.
#' @importFrom checkmate assertTibble assertCharacter
#' @importFrom readr write_csv
#' @importFrom dplyr union arrange row_number

updateTable <- function(index = NULL, name = NULL){

  # set internal paths
  intPaths <- getOption("adb_path")

  # check validity of arguments
  assertTibble(x = index)
  assertCharacter(x = name)

  # first archive the original index
  theTime <- paste0(strsplit(x = format(Sys.time(), format="%Y%m%d_%H%M%S"), split = "[ ]")[[1]], collapse = "_")

  # if a file already exists, join the new data to that
  if(testFileExists(x = paste0(intPaths, "/", name, ".csv"))){
    oldIndex <- read_csv(paste0(intPaths, "/", name, ".csv"), col_types = getColTypes(input = index))
    write_csv(x = oldIndex,
              path = paste0(intPaths, "/log/", name, "_", theTime, ".csv"),
              na = "", append = FALSE)

    # create vector of columns that should be checked for distinct values
    keepCols <- names(index)[!str_detect(string = names(index), pattern = "ID") &
                               !str_detect(string = names(index), pattern = "notes") &
                               !str_detect(string = names(index), pattern = "date")]

    # join the old table with 'index'
    index <- union(oldIndex, index) %>%
      group_by(.dots = keepCols) %>%
      filter(row_number() == 1) %>%
      arrange(!!as.name(colnames(index)[1]))
  }

  # store it
  write_csv(x = index,
            path = paste0(intPaths, "/", name, ".csv"),
            na = "")
}