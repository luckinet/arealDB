#' Set the root path
#'
#' Initiate a geospatial database or register a database that exists at the root
#' path.
#' @param root [\code{character(1)}]\cr path to the root directory that contains
#'   or shall contain an areal database.
#' @details This is the first function that is run in a project, as it initiates
#'   the areal database by creating the default sub-directories and initial
#'   inventory tables. When a database has already been set up, this funciton is
#'   used to register that path in the options of the current R session.
#' @return Enters the root path as 'adb_path' into the options.
#' @importFrom checkmate testDirectory testFileExists
#' @importFrom readr write_csv
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
    message("I have created a new project directory.\n  -> please run 'setVariables()' to create optional indices and translation tables.")
  }

  if(!testDirectory(x = file.path(root, "adb_tables"), access = "rw")){
    dir.create(file.path(root, "adb_tables"))
  }
  if(!testDirectory(x = file.path(root, "adb_geometries"), access = "rw")){
    dir.create(file.path(root, "adb_geometries"))
  }
  if(!testDirectory(x = file.path(root, "log"), access = "rw")){
    dir.create(file.path(root, "log"))
  }

  if(!testDirectory(x = file.path(root, "adb_tables", "incoming"), access = "rw")){
    dir.create(file.path(root, "adb_tables", "incoming"))
  }
  if(!testDirectory(x = file.path(root, "adb_tables", "meta"), access = "rw")){
    dir.create(file.path(root, "adb_tables", "meta"))
  }
  if(!testDirectory(x = file.path(root, "adb_tables", "stage1"), access = "rw")){
    dir.create(file.path(root, "adb_tables", "stage1"))
  }
  if(!testDirectory(x = file.path(root, "adb_tables", "stage2"), access = "rw")){
    dir.create(file.path(root, "adb_tables", "stage2"))
  }
  if(!testDirectory(x = file.path(root, "adb_tables", "stage2", "processed"), access = "rw")){
    dir.create(file.path(root, "adb_tables", "stage2", "processed"))
  }
  if(!testDirectory(x = file.path(root, "adb_tables", "stage3"), access = "rw")){
    dir.create(file.path(root, "adb_tables", "stage3"))
  }
  if(!testDirectory(x = file.path(root, "adb_geometries", "incoming"), access = "rw")){
    dir.create(file.path(root, "adb_geometries", "incoming"))
  }
  if(!testDirectory(x = file.path(root, "adb_geometries", "meta"), access = "rw")){
    dir.create(file.path(root, "adb_geometries", "meta"))
  }
  if(!testDirectory(x = file.path(root, "adb_geometries", "stage1"), access = "rw")){
    dir.create(file.path(root, "adb_geometries", "stage1"))
  }
  if(!testDirectory(x = file.path(root, "adb_geometries", "stage2"), access = "rw")){
    dir.create(file.path(root, "adb_geometries", "stage2"))
  }
  if(!testDirectory(x = file.path(root, "adb_geometries", "stage2", "processed"), access = "rw")){
    dir.create(file.path(root, "adb_geometries", "stage2", "processed"))
  }
  if(!testDirectory(x = file.path(root, "adb_geometries", "stage3"), access = "rw")){
    dir.create(file.path(root, "adb_geometries", "stage3"))
  }

  # create the empty inventory tables, if they don't exist yet
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
  if(!testFileExists(x = file.path(root, "inv_tables.csv"))){
    census <- tibble(tabID = integer(),
                     geoID = integer(),
                     datID = integer(),
                     source_file = character(),
                     date = date(),
                     orig_file = character(),
                     notes = character())
    write_csv(x = census,
              path = paste0(root, "/inv_tables.csv"),
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

  # and also the translation table for nations and territories
  if(!testFileExists(x = file.path(root, "tt_nations.csv"))){
    tt_nations <- tibble(origin = NA_character_,
                         target = countries$nation,
                         source = "original",
                         date = Sys.Date(),
                         tabID = NA_character_)
    write_csv(x = tt_nations,
              path = paste0(root, "/tt_nations.csv"),
              na = "")
  }
  if(!testFileExists(x = file.path(root, "tt_territories.csv"))){
    tt_territories <- tibble(origin = NA_character_,
                             target = NA_character_,
                             source = "original",
                             date = Sys.Date(),
                             tabID = NA_character_)
    write_csv(x = tt_territories,
              path = paste0(root, "/tt_territories.csv"),
              na = "")
  }

  options(adb_path = root)
}
