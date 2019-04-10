#' Update an index
#'
#' @param index [\code{tibble(1)}]\cr the index to save.
#' @param name [\code{character(1)}]\cr name of the index that shall be updated.
#' @importFrom checkmate assertTibble assertCharacter
#' @importFrom readr write_csv
#' @importFrom dplyr union arrange
#' @export

updateIndex <- function(index = NULL, name = NULL){

  # set internal paths
  intPaths <- getOption("dmt_path")

  # check validity of arguments
  assertTibble(x = index)
  assertCharacter(x = name)

  # first archive the original index
  theTime <- paste0(strsplit(x = format(Sys.time(), format="%Y%m%d_%H%M%S"), split = "[ ]")[[1]], collapse = "_")
  oldIndex <- read_csv(paste0(intPaths, "/", name, ".csv"), col_types = getColTypes(input = index))
  write_csv(x = oldIndex,
            path = paste0(intPaths, "/log/", name, "_", theTime, ".csv"),
            na = "", append = FALSE)

  # then join the old table with 'index'
  index <- union(index, oldIndex) %>%
    arrange(!!as.name(colnames(index)[1]))

  # and store it
  write_csv(x = index,
            path = paste0(intPaths, "/", name, ".csv"),
            na = "")
}