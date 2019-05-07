#' Set the root path
#'
#' Gather and load all look-up-tables for translating and matching terms.
#' @param root [\code{character(1)}]\cr path tothe root directory that contains
#'   all tables and sub-directories.
#' @examples
#' setPath(root = "/home/se87kuhe/Nextcloud/LUCKINet/myProject/")
#' @importFrom checkmate testDirectory
#' @export

setPath <- function(root = NULL){

  # shitty windows workaround, because a directory may not have a trailing slash
  # for the function "file.exists()" used in assertDirectory()
  lastChar <- substr(x = root, start = nchar(root), stop = nchar(root))
  if(lastChar == "/"){
    root <- substr(root, start = 1, stop = nchar(root)-1)
  }

  # test whether the required directories exist and create them if they don't exist
  if(!testDirectory(x = root, access = "rw")){
    dir.create(file.path(root))
    message("I have created a new project directory.\n  -> please run 'setIndex()' to create the default and optional indices and translation tables.")
  }

  if(!testDirectory(x = file.path(root, "cT_census"), access = "rw")){
    dir.create(file.path(root, "cT_census"))
  }
  if(!testDirectory(x = file.path(root, "cT_geometries"), access = "rw")){
    dir.create(file.path(root, "cT_geometries"))
  }

  if(!testDirectory(x = file.path(root, "cT_census", "incoming"), access = "rw")){
    dir.create(file.path(root, "cT_census", "incoming"))
  }
  if(!testDirectory(x = file.path(root, "cT_census", "meta"), access = "rw")){
    dir.create(file.path(root, "cT_census", "meta"))
  }
  if(!testDirectory(x = file.path(root, "cT_census", "original_datasets"), access = "rw")){
    dir.create(file.path(root, "cT_census", "original_datasets"))
  }
  if(!testDirectory(x = file.path(root, "cT_census", "stage1"), access = "rw")){
    dir.create(file.path(root, "cT_census", "stage1"))
  }
  if(!testDirectory(x = file.path(root, "cT_census", "stage2"), access = "rw")){
    dir.create(file.path(root, "cT_census", "stage2"))
  }
  if(!testDirectory(x = file.path(root, "cT_geometries", "incoming"), access = "rw")){
    dir.create(file.path(root, "cT_geometries", "incoming"))
  }
  if(!testDirectory(x = file.path(root, "cT_geometries", "meta"), access = "rw")){
    dir.create(file.path(root, "cT_geometries", "meta"))
  }
  if(!testDirectory(x = file.path(root, "cT_geometries", "original_datasets"), access = "rw")){
    dir.create(file.path(root, "cT_geometries", "original_datasets"))
  }
  if(!testDirectory(x = file.path(root, "cT_geometries", "stage1"), access = "rw")){
    dir.create(file.path(root, "cT_geometries", "stage1"))
  }
  if(!testDirectory(x = file.path(root, "cT_geometries", "stage2"), access = "rw")){
    dir.create(file.path(root, "cT_geometries", "stage2"))
  }

  options(dmt_path = root)
}
