#' Determine the valid ID of variables
#'
#' This function matches the values of a variable with an index and returns the
#' specified IDs.
#' @param input [\code{character(.)}]\cr terms to be translated.
#' @param ... [\code{list(1)}]\cr lists that capture the variables by which to
#'   match and the new column names containing the resulting ID; see Details.
#' @param keepOrig [\code{logical(1)}]\cr to keep the original commodities in
#'   the output (\code{TRUE}) or to remove them (\code{FALSE}, default). Useful
#'   for debugging.
#' @details Arguments in \code{...} are named lists, where the name of the list
#'   represents at the same time the variable to match and the the index in
#'   which to match (should have been created with \code{\link{makeIndex}}). The
#'   value represents the column in the index, with which to match the variable.
#'   The argument name represents the column in the index, which should be
#'   returned and which contains the new ID.
#' @examples
#' \dontrun{
#'
#' # match the column 'commodities' in 'someInput' with the column 'simpleName'
#' # in the index 'commodities' and chose the column 'faoID' in the index.
#' matchVars(input = someInput, faoID = list(commodities = "simpleName"))
#'
#' # alternatively, the column 'faoGroupID' could be used via
#' matchVars(input = someInput, faoGroupID = list(commodities = "simpleName"))
#' }
#' @return The table provided in \code{input}, where the given variable is
#'   replaced by the column that is specified by the argument name.
#' @importFrom checkmate assertCharacter assertIntegerish assertNames
#'   testIntegerish
#' @importFrom dplyr filter pull bind_cols
#' @importFrom rlang exprs
#' @importFrom magrittr %>%
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export

matchVars <- function(input = NULL, ..., keepOrig = FALSE){

  # set internal objects
  intPaths <- paste0(getOption(x = "dmt_path"))
  vars <- exprs(..., .named = TRUE)

  # check validity of arguments
  assertTibble(x = input)
  assertDataFrame(x = input)
  assertLogical(x = keepOrig)

  for(i in seq_along(vars)){

    targetVar <- names(vars)[i]
    toMatch <- eval(vars[[i]])
    varName <- names(toMatch)
    targetName <- toMatch[[1]]

    # get tables
    id_temp <- suppressMessages(read_csv(paste0(getOption("dmt_path"), "/id_", varName, ".csv")))

    # extract the respective column
    inputTerms <- input %>%
      select(varName) %>%
      unique() %>%
      pull(1)

    # translate the terms
    theTerms <- translateTerms(terms = unique(inputTerms),
                               index = paste0("tt_", varName))

    # get the matching terms
    matched <- theTerms %>%
      left_join(id_temp, by = c("target" = targetName)) %>%
      select(!!varName := origin, targetVar)

    if(!keepOrig){
      out <- input %>%
        left_join(matched) %>%
        select(-!!varName)
    } else{
      out <- input %>%
        left_join(matched)
    }
  }

  return(out)
}