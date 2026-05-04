#' Load the mapping table for a dataseries against a vocabulary
#'
#' Returns the mappings table that records how external source labels are
#' mapped to canonical IDs of the requested vocabulary.
#'
#' @param vocabulary character; name of a registered vocabulary
#'   (e.g. \code{"gazetteer"}, \code{"commodity"}).
#' @param dataseries character; name of a registered dataseries.
#' @return tibble with columns \code{source_label}, \code{source},
#'   \code{canonical_id}, \code{parent_id}, \code{note}.
#' @importFrom checkmate assertChoice
#' @export

adb_translations <- function(vocabulary = NULL, dataseries = NULL){

  intPaths <- paste0(.adb_state$path)
  temp <- readRDS(paste0(intPaths, "/inventory.rds"))
  inv_dataseries <- temp$dataseries
  inv_vocabulary <- temp$vocabularies

  assertChoice(x = vocabulary, choices = inv_vocabulary$name)
  assertChoice(x = dataseries, choices = inv_dataseries$name)

  .read_mappings(vocabulary, dataseries)
}
