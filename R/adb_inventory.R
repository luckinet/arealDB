#' Load the inventory of the currently active areal database
#'
#' @param type [`character(1)`][character]\cr the inventory sub-table to load,
#'   either \code{"dataseries"}, \code{"vocabularies"}, \code{"tables"}, or
#'   \code{"geometries"}.
#' @return returns the table selected in \code{type}
#' @importFrom checkmate assertChoice
#' @export

adb_inventory <- function(type = NULL){

  assertChoice(x = type, choices = c("dataseries", "vocabularies", "tables", "geometries"), null.ok = TRUE)

  temp <- readRDS(paste0(.adb_state$path, "/inventory.rds"))

  if(!is.null(type)){
    out <- temp[[type]]
  } else {
    out <- temp
  }

  return(out)
}