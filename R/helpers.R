#' Get the column types of a tibble
#'
#' @param input [\code{tibble(1)}]\cr tibble from which to get column types.
#' @importFrom checkmate assertDataFrame
#' @importFrom tibble tibble
#' @importFrom dplyr summarise_all left_join pull
#' @importFrom tidyr gather
#' @importFrom stringr str_c

getColTypes <- function(input = NULL){

  assertDataFrame(x = input)

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

#' Test whether a file is a compressed file
#'
#' @param x [\code{character(1)}]\cr the file name.
#' @details This function looks at the file-extension and if it is one of
#'   \code{.gz}, \code{.bz2}, \code{.tar} \code{.zip}, \code{.tgz}, \code{.gzip}
#'   or \code{.7z}, it returns the value \code{TRUE}.
#' @importFrom checkmate assertCharacter

testCompressed <- function(x){

  assertCharacter(x = x, any.missing = FALSE, len = 1)

  return(grepl("^.*(.gz|.bz2|.tar|.zip|.tgz|.gzip|.7z)[[:space:]]*$", x))
}