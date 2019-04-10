#' Get the column types of a tibble
#'
#' @param input [\code{tibble(1)}]\cr tibble from which to get column types.
#' @importFrom tibble tibble
#' @importFrom dplyr summarise_all left_join pull
#' @importFrom tidyr gather
#' @importFrom stringr str_c
#' @export

getColTypes <- function(input = NULL){

  types <- tibble(col_type = c("character", "integer", "numeric", "double", "logical", "Date"),
                  code = c("c", "i", "n", "d", "l", "D"))

  out <- input %>%
    summarise_all(class) %>%
    gather(col_name, col_type) %>%
    left_join(y = types, by = "col_type") %>%
    pull("code") %>%
    str_c(collapse = "")

  return(out)

}