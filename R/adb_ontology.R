#' Load a registered vocabulary
#'
#' @param ... optional filter expressions passed to \code{\link[dplyr]{filter}}
#'   to subset the returned terms (e.g. \code{class == "ADM0"}).
#' @param vocabulary character; name of the vocabulary to load (e.g.
#'   \code{"gazetteer"}, \code{"commodity"}). Defaults to the first ontology
#'   registered (i.e. anything other than \code{"gazetteer"}).
#' @return a tibble with columns \code{id}, \code{label}, \code{class},
#'   \code{parent_id}, and all mapped external labels from the
#'   \code{vocabularies/mappings/} files as additional columns.
#' @importFrom dplyr filter left_join bind_rows group_by summarise ungroup
#' @importFrom tidyr pivot_wider
#' @importFrom rlang quos
#' @export

adb_ontology <- function(..., vocabulary = NULL){

  intPaths <- .adb_state$path
  inv_vocabulary <- readRDS(paste0(intPaths, "/inventory.rds"))$vocabularies

  if(is.null(vocabulary)){
    nonGaz <- setdiff(inv_vocabulary$name, "gazetteer")
    if(length(nonGaz) == 0) stop("no non-gazetteer vocabulary registered.")
    vocabulary <- nonGaz[1]
  }
  assertChoice(x = vocabulary, choices = inv_vocabulary$name)

  terms <- .read_terms(vocabulary)

  dots <- quos(...)
  if(length(dots) > 0){
    terms <- filter(terms, !!!dots)
  }

  # attach all external mappings as a single pipe-collapsed column per dataseries
  all_mappings <- .read_all_mappings(vocabulary)
  if(length(all_mappings) > 0){
    combined <- bind_rows(all_mappings) %>%
      group_by(canonical_id, source) %>%
      summarise(labels = paste0(source_label, collapse = " | "), .groups = "drop") %>%
      pivot_wider(id_cols = canonical_id, names_from = source, values_from = labels)

    terms <- terms %>%
      left_join(combined, by = c("id" = "canonical_id"))
  }

  terms
}
