#' Load the inventory of the currently active areal database
#'
#' @param type [\code{character(1)}]\cr the inventory sub-table to load,
#'   either \code{"dataseries"}, \code{"tables"} or \code{"geometries"}.
#' @return returns the table selected in \code{type}
#' @importFrom checkmate assertChoice
#' @export

adb_inventory <- function(type = NULL){

  assertChoice(x = type, choices = c("dataseries", "tables", "geometries"))

  temp <- readRDS(paste0(getOption(x = "adb_path"), "/meta/inventory.rds"))

  if(!is.null(type)){
    out <- temp[[type]]
  } else {
    out <- temp
  }

  return(out)
}