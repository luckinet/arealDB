#' Determine the valid year
#'
#' This function matches years known years and returns \code{input} containing
#' the matched \code{years}.
#' @param input [\code{data.frame(1)}]\cr table in which to match administrative
#'   units.
#' @param priority [\code{character(1)}]\cr to which year should a tow-year date
#'   be assigned, either the \code{"sowing"} or the \code{"harvest"} year.
#' @param keepOrig [\code{logical(1)}]\cr to keep the original units in the
#'   output (\code{TRUE}) or to remove them (\code{FALSE}, default). Useful for
#'   debugging.
#' @details On the southern hemisphere a growth period can span two years
#'   because the plants are sawn in autumn and the harvest is brought in in
#'   spring. This often results in a two-year notation, such as 1992-1993. This
#'   function thus mostly determines the correct year to which the census should
#'   be assigned.
#' @return The table provided in \code{input}, where the given years are
#'   replaced by the column \code{years}, which contains the year to which the
#'   census statistics should be assigned.
#' @importFrom checkmate assertDataFrame assertChoice assertLogical
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @export

matchYears <- function(input = NULL, priority = "harvest", keepOrig = FALSE){

  # check validity of arguments
  assertDataFrame(x = input)
  assertChoice(x = priority, choices = c("harvest", "sowing"))
  assertLogical(x = keepOrig)

  out <- input

  out <- out %>%
    mutate(year = as.integer(years))


  if(!keepOrig){
    out <- out %>%
      select(-years)
  }

  return(out)

}