#' Get the column types of a tibble
#'
#' (internal function not for user interaction)
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
#' (internal function not for user interaction)
#' @param x [\code{character(1)}]\cr the file name.
#' @details This function looks at the file-extension and if it is one of
#'   \code{.gz}, \code{.bz2}, \code{.tar} \code{.zip}, \code{.tgz}, \code{.gzip}
#'   or \code{.7z}, it returns the value \code{TRUE}.
#' @importFrom checkmate assertCharacter

testCompressed <- function(x){

  assertCharacter(x = x, any.missing = FALSE, len = 1)

  return(grepl("^.*(.gz|.bz2|.tar|.zip|.tgz|.gzip|.7z)[[:space:]]*$", x))
}

#' Open file when it's not open
#'
#' @param file a file to store with a delay.
#' @param fun the function to use.
#' @param ... arguments to the \emph{write} functions.
#' @details This function takes the \code{file} and only opens it, if it is not
#' currently already open. In the case it's already open, it prompts the user to
#' confirm when the file is closed (and processed by the other process) and then
#' opens it again.
#' @importFrom checkmate assertChoice

delayedRead <- function(file, fun = NULL, ...){

  assertChoice(x = fun, choices = c("readRDS", "read_csv"))

  # readRDS()
  # read_csv()


  # https://stackoverflow.com/questions/20038732/how-to-check-a-file-is-opened-or-closed-in-r



}