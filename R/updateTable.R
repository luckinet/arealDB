#' Update a table
#'
#' Update any inventory, index or translation table of an areal database
#' (internal function not meant for user interaction).
#' @param index [\code{tibble(1)}]\cr the table to use as update.
#' @param name [\code{character(1)}]\cr name of the table that shall be updated.
#' @param matchCols [\code{character(.)}]\cr the columns in the old file by
#'   which to match.
#' @param backup [logical(1)]\cr whether or not to store the old table in the
#'   \code{log} directory.
#' @return No return value, called for the side-effect of storing a table in a
#'   specified location
#' @importFrom checkmate assertTibble assertCharacter
#' @importFrom readr write_csv
#' @importFrom dplyr union arrange row_number across
#' @importFrom tidyselect all_of

updateTable <- function(index = NULL, name = NULL, matchCols = NULL, backup = TRUE){

  # set internal paths
  intPaths <- getOption("adb_path")

  # check validity of arguments
  assertTibble(x = index)
  assertCharacter(x = name)

  # first archive the original index
  theTime <- paste0(strsplit(x = format(Sys.time(), format = "%Y%m%d_%H%M%S"), split = "[ ]")[[1]], collapse = "_")

  # if a file already exists, join the new data to that
  tabPath <- paste0(intPaths, "/", name, ".csv")

  if (testFileExists(x = tabPath)) {
    oldIndex <- read_csv(tabPath, col_types = getColTypes(input = index))
    # somehow the file in tabPath is locked here, in Windows, so that it can't be overwritten below', I assume...

    if (backup) {
      write_csv(x = oldIndex,
                file = paste0(intPaths, "/log/", name, "_", theTime, ".csv"),
                na = "", append = FALSE)
    }

    if (is.null(matchCols)) {
      matchCols <- names(oldIndex)
      matchCols <- matchCols[!matchCols %in% "notes"]
    } else {
      assertSubset(x = matchCols, choices = names(oldIndex))
    }

    # join the old table with 'index'
    index <- union(oldIndex, index) %>%
      group_by(across(all_of(matchCols))) %>%
      filter(row_number() == n()) %>%
      arrange(!!as.name(colnames(index)[1])) %>%
      ungroup()
  }

  # store it
  write_csv(x = index,
            file = tabPath,
            na = "")

}