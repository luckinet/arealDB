#' Set the root path
#'
#' Gather and load all look-up-tables for translating and matching terms.
#' @param root [\code{character(1)}]\cr path tothe root directory that contains
#'   all tables and sub-directories.
#' @examples
#' setPath(root = "/home/se87kuhe/Nextcloud/LUCKINet/data/")
#' @importFrom checkmate assertDirectory
#' @export

setPath <- function(root = NULL){

  # shitty windows workaround, because a directory may not have a trailing slash
  # for the function "file.exists()" used in assertDirectory()
  lastChar <- substr(x = root, start = nchar(root), stop = nchar(root))
  if(lastChar == "/"){
    root <- substr(root, start = 1, stop = nchar(root)-1)
  }
  assertDirectory(root, access = "rwx")
  options(dmt_path = root)

}
