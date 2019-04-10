#' Determine the valid ID of commodities
#'
#' This function matches commodities with a list of known commodities and
#' returns the matched IDs.
#' @param input [\code{character(.)}]\cr terms to be translated.
#' @param ... [\code{data.frame(1)}]\cr define columns in \code{input}, where
#'   the commodities are stored; see Details.
#' @param keepOrig [\code{logical(1)}]\cr to keep the original commodities in
#'   the output (\code{TRUE}) or to remove them (\code{FALSE}, default). Useful
#'   for debugging.
#' @details Arguments in \code{...} must be named either with \code{faoName},
#'   \code{faoGroupName} or \code{scientificName}, to match with the respective
#'   column in \code{id_commodities.csv}.
#' @return The table provided in \code{input}, where the given column is
#'   replaced either by \code{faoID} or by \code{faoGroupID}, containing
#'   standardised commodity IDs.
#' @importFrom checkmate assertCharacter assertIntegerish assertNames
#'   testIntegerish
#' @importFrom dplyr filter pull bind_cols
#' @importFrom rlang exprs
#' @importFrom magrittr %>%
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export

matchCommodities <- function(input = NULL, ..., keepOrig = FALSE){

  # set internal objects
  dots <- exprs(..., .named = TRUE)

  # get tables
  id_commodities <- read_csv(paste0(getOption("dmt_path"), "/id_commodities.csv"), col_types = "iclciccc")

  # check validity of arguments
  assertList(x = dots, len = 1)
  assertNames(x = names(dots), subset.of = c("faoName", "simpleName", "faoGroupName", "scientificName"))
  assertDataFrame(x = id_commodities, ncols = 8)
  assertNames(x = colnames(id_commodities), permutation.of = c("faoID", "simpleName", "primary", "faoName", "faoGroupID", "faoGroupName", "canonicalName", "Definition"))
  assertDataFrame(x = input)
  assertLogical(x = keepOrig)

  # extract the commodities column
  inputComms <- input %>%
    select(!!!dots) %>%
    unique() %>%
    pull(1)

  # specify which variables of id_commodities should be returned
  inVar <- dots[[1]]
  inVarMatch <- names(dots)[[1]]
  outVar <- ifelse(names(dots) == "faoGroupName", "faoGroupID", "faoID")

  # translate the terms
  standardComms <- translateTerms(terms = unique(inputComms),
                                  index = "trans_commodities")

  # get the matching terms
  matched <- standardComms %>%
    left_join(id_commodities, by = c("target" = inVarMatch)) %>%
    select(!!inVar := origin, outVar)

  if(!keepOrig){
    out <- input %>%
      left_join(matched) %>%
      select(-!!inVar)
  } else{
    out <- input %>%
      left_join(matched)
  }

  return(out)

}
