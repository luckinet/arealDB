#' Set the root path
#'
#' Initiate a geospatial database or register a database that exists at the root
#' path.
#' @param root [\code{character(1)}]\cr path to the root directory that contains
#'   or shall contain an areal database.
#' @param gazetteer [\code{character(1)}]\cr path to the gazetteer that holds
#'   the (hierarchical) information of territorial units used in this database.
#' @details This is the first function that is run in a project, as it initiates
#'   the areal database by creating the default sub-directories and initial
#'   inventory tables. When a database has already been set up, this function is
#'   used to register that path in the options of the current R session.
#' @return  No return value, called for the side effect of creating the
#'   directory structure of the new areal database and tables that contain the
#'   database metadata.
#' @examples
#' start_arealDB(root = paste0(tempdir(), "/newDB"),
#'               gazetteer = system.file("test_datasets/territories.rds",
#'                                       package = "arealDB"))
#'
#' getOption("adb_path"); getOption("gazetteer_path")
#' @importFrom checkmate testDirectory testFileExists
#' @importFrom readr write_csv
#' @export

start_arealDB <- function(root = NULL, gazetteer = NULL){

  assertCharacter(x = root, len = 1)
  if(!getOption("adb_testing")){
    assertFileExists(x = gazetteer, access = "rw", extension = "rds")
  }

  # shitty windows workaround, because a directory may not have a trailing slash
  # for the function "file.exists()" used in assertDirectory()
  lastChar <- substr(x = root, start = nchar(root), stop = nchar(root))
  if(lastChar == "/"){
    root <- substr(root, start = 1, stop = nchar(root)-1)
  }

  # test whether the required directories exist and create them if they don't exist
  if(!testDirectory(x = root, access = "rw")){
    dir.create(file.path(root))
    message("I have created a new project directory.")
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
  if(!testDirectory(x = file.path(root, "incoming"), access = "rw")){
    dir.create(file.path(root, "incoming"))
  }
  if(!testDirectory(x = file.path(root, "meta"), access = "rw")){
    dir.create(file.path(root, "meta"))
  }
  if(!testDirectory(x = file.path(root, "meta", "schemas"), access = "rw")){
    dir.create(file.path(root, "meta", "schemas"))
  }
  if(!testDirectory(x = file.path(root, "meta", "concepts"), access = "rw")){
    dir.create(file.path(root, "meta", "concepts"))
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
                         description = character(),
                         homepage = character(),
                         licence_link = character(),
                         licence_path = character(),
                         notes = character())
    write_csv(x = dataseries,
              file = paste0(root, "/inv_dataseries.csv"),
              na = "")
  }

  if(!testFileExists(x = file.path(root, "inv_tables.csv"))){
    census <- tibble(tabID = integer(),
                     geoID = integer(),
                     datID = integer(),
                     source_file = character(),
                     schema = character(),
                     orig_file = character(),
                     orig_link = character(),
                     download_date = date(),
                     next_update = date(),
                     update_frequency = character(),
                     metadata_link = character(),
                     metadata_path = character(),
                     notes = character())
    write_csv(x = census,
              file = paste0(root, "/inv_tables.csv"),
              na = "")
  }

  if(!testFileExists(x = file.path(root, "inv_geometries.csv"))){
    geometries <- tibble(geoID = integer(),
                         datID = integer(),
                         level = integer(),
                         source_file = character(),
                         layer = character(),
                         hierarchy = character(),
                         orig_file = character(),
                         orig_link = character(),
                         download_date = date(),
                         next_update = date(),
                         update_frequency = character(),
                         notes = character())
    write_csv(x = geometries,
              file = paste0(root, "/inv_geometries.csv"),
              na = "")
  }

  oldOptions <- options()
  on.exit(options(oldOptions))

  options(adb_path = root)
  options(gazetteer_path = gazetteer)
}


#' @describeIn start_arealDB deprecated way of starting an areal database
#' @export

setPath <- function(root = NULL) {
  .Deprecated("start_arealDB")

  setPath(root = root)
}