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
    message("I have created a new project directory.\n  -> please run 'setIndex()' to create optional indices and translation tables.")
  }

  if(!testDirectory(x = file.path(root, "cT_census"), access = "rw")){
    dir.create(file.path(root, "cT_census"))
  }
  if(!testDirectory(x = file.path(root, "cT_geometries"), access = "rw")){
    dir.create(file.path(root, "cT_geometries"))
  }
  if(!testDirectory(x = file.path(root, "log"), access = "rw")){
    dir.create(file.path(root, "log"))
  }

  if(!testDirectory(x = file.path(root, "cT_census", "incoming"), access = "rw")){
    dir.create(file.path(root, "cT_census", "incoming"))
  }
  if(!testDirectory(x = file.path(root, "cT_census", "meta"), access = "rw")){
    dir.create(file.path(root, "cT_census", "meta"))
  }
  if(!testDirectory(x = file.path(root, "cT_census", "stage1"), access = "rw")){
    dir.create(file.path(root, "cT_census", "stage1"))
  }
  if(!testDirectory(x = file.path(root, "cT_census", "stage2"), access = "rw")){
    dir.create(file.path(root, "cT_census", "stage2"))
  }
  if(!testDirectory(x = file.path(root, "cT_census", "stage2", "processed"), access = "rw")){
    dir.create(file.path(root, "cT_census", "stage2", "processed"))
  }
  if(!testDirectory(x = file.path(root, "cT_census", "stage3"), access = "rw")){
    dir.create(file.path(root, "cT_census", "stage3"))
  }
  if(!testDirectory(x = file.path(root, "cT_geometries", "incoming"), access = "rw")){
    dir.create(file.path(root, "cT_geometries", "incoming"))
  }
  if(!testDirectory(x = file.path(root, "cT_geometries", "meta"), access = "rw")){
    dir.create(file.path(root, "cT_geometries", "meta"))
  }
  if(!testDirectory(x = file.path(root, "cT_geometries", "stage1"), access = "rw")){
    dir.create(file.path(root, "cT_geometries", "stage1"))
  }
  if(!testDirectory(x = file.path(root, "cT_geometries", "stage2"), access = "rw")){
    dir.create(file.path(root, "cT_geometries", "stage2"))
  }
  if(!testDirectory(x = file.path(root, "cT_geometries", "stage2", "processed"), access = "rw")){
    dir.create(file.path(root, "cT_geometries", "stage2", "processed"))
  }
  if(!testDirectory(x = file.path(root, "cT_geometries", "stage3"), access = "rw")){
    dir.create(file.path(root, "cT_geometries", "stage3"))
  }

  # create the empty inventory tables
  if(!testFileExists(x = file.path(root, "inv_dataseries.csv"))){
    dataseries <- tibble(datID = integer(),
                         name = character(),
                         long_name = character(),
                         website = character(),
                         notes = character())
    write_csv(x = dataseries,
              path = paste0(root, "/inv_dataseries.csv"),
              na = "")
  }
  if(!testFileExists(x = file.path(root, "inv_census.csv"))){
    census <- tibble(cenID = integer(),
                     geoID = integer(),
                     datID = integer(),
                     source_file = character(),
                     date = date(),
                     orig_file = character(),
                     notes = character())
    write_csv(x = census,
              path = paste0(root, "/inv_census.csv"),
              na = "")
  }
  if(!testFileExists(x = file.path(root, "inv_geometries.csv"))){
    geometries <- tibble(geoID = integer(),
                         datID = integer(),
                         level = integer(),
                         source_file = character(),
                         layer = character(),
                         nation_column = character(),
                         unit_column = character(),
                         date = date(),
                         orig_file = character(),
                         notes = character())
    write_csv(x = geometries,
              path = paste0(root, "/inv_geometries.csv"),
              na = "")
  }

  # and also the translation table for nations
  if(!testFileExists(x = file.path(root, "tt_nations.csv"))){
    tt_nations <- tibble(origin = NA_character_,
                         target = countries$nation,
                         source = "original",
                         date = Sys.Date(),
                         cenID = NA_character_)
    write_csv(x = tt_nations,
              path = paste0(root, "/tt_nations.csv"),
              na = "")
  }

  options(cT_path = root)
}
