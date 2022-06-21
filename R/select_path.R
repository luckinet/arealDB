#' Select a system specific path
#'
#' This function calls \code{\link{Sys.info}} and selects from the provided
#' arguments that path that corresponds to the recent nodename.
#' @param ... combination of a nodename and the path that should be valid here.
#' @param default [`character(1)`][character]\cr a fallback path that should be
#'   used in case the matching nodename is not mounted.
#' @examples
#' \dontrun{
#' myPath <- select_path(local = "/path/on/local/machine/",
#'                       hpc = "/path/on/remote/machine/",
#'                       default = "/fallback/path/if/nothing/else/found/")
#' }
#' @importFrom rlang exprs eval_tidy
#' @importFrom checkmate assertDirectoryExists
#' @export

select_path <- function(..., default = NULL){

  sys <- Sys.info()

  paths <- exprs(...)
  out <- eval_tidy(paths[sys[["nodename"]]][[1]])

  if(length(out) == 0 & !is.null(default)){
    assertDirectoryExists(x = default, access = "rw")
    out <- default
  }

  return(out)
}
