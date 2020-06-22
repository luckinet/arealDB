#' Determine the valid ID of variables
#'
#' This function matches the values of a variable with an index and returns the
#' specified IDs.
#' @param input [\code{character(.)}]\cr terms to be translated.
#' @param source [\code{integerish(1)}]\cr the census ID (\code{cenID}) from
#'   which the terms have been taken.
#' @param ... [\code{list(1)}]\cr lists that capture the variables by which to
#'   match and the new column names containing the resulting ID; see Details.
#' @details Arguments in \code{...} are named lists that indicate with which
#'   target column variables shall be matched and which value should be used as
#'   target ID.
#'
#'   targetID = list(variable = targetColumn)
#'
#'   The variable must be present as column in \code{input} and a table that is
#'   named "id_VARIABLE.csv" must be available in the root directory of the
#'   project. This should have been created with \code{\link{setVariables}}.
#' @return The table provided in \code{input}, where the given variable is
#'   replaced by the column that is specified by the argument name.
#' @importFrom checkmate assertCharacter assertIntegerish assertNames
#'   testIntegerish
#' @importFrom dplyr filter pull bind_cols
#' @importFrom rlang exprs
#' @importFrom magrittr %>%
#' @importFrom utils txtProgressBar setTxtProgressBar

matchVars <- function(input = NULL, source = NULL, ...){

  # set internal objects
  intPaths <- paste0(getOption(x = "adb_path"))
  vars <- exprs(..., .named = TRUE)

  # check validity of arguments
  assertTibble(x = input)
  assertIntegerish(x = source)
  assertList(x = vars)

  message("--> matching variables of ...")
  for(i in seq_along(vars)){

    targetVar <- names(vars)[i]
    toMatch <- eval(vars[[i]])
    varName <- names(toMatch)
    targetName <- toMatch[[1]]

    message("    ... '", targetVar, "'")

    # get tables
    id_temp <- suppressMessages(read_csv(paste0(getOption("adb_path"), "/id_", varName, ".csv")))

    # extract the respective column
    inputTerms <- input %>%
      select(all_of(varName)) %>%
      unique() %>%
      pull(1)

    # translate the terms
    theTerms <- translateTerms(terms = unique(inputTerms),
                               source = list("tabID" = source),
                               index = paste0("tt_", varName))

    # get the matching terms
    matched <- theTerms %>%
      left_join(id_temp, by = c("target" = targetName)) %>%
      select(!!varName := origin, all_of(targetVar))

    out <- suppressMessages(
      input %>%
        left_join(matched))

  }

  return(out)
}