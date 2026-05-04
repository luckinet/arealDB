#' Initiate an areal database
#'
#' Initiate a geospatial database or register a database that exists at the root
#' path.
#' @param root [`character(1)`][character]\cr path to the root directory that
#'   contains or shall contain an areal database.
#' @param version [`character(1)`][character]\cr version identifier for this
#'   areal database.
#' @param author [`character(1)`][character]\cr authors that contributed to
#'   building this areal database. Should be a list with items \code{"cre"}
#'   (creator), \code{"aut"} (authors) and \code{"ctb"} (contributors).
#' @param licence [`character(1)`][character]\cr licence (link) for this areal
#'   database.
#' @param level [`character(1)`][character]\cr the label of the class in the
#'   gazetteer at which this database operates (e.g. \code{"ADM0"} for a
#'   national database). Stage 3 outputs are split by units of this class.
#' @details This is the first function that is run in a project, as it initiates
#'   the areal database by creating the default sub-directories and initial
#'   inventory tables. Vocabularies (gazetteer, ontologies) are added later via
#'   \code{\link{regVocabulary}} and \code{\link{normVocabulary}}.
#' @return  No return value, called for the side effect of creating the
#'   directory structure of the new areal database and tables that contain the
#'   database metadata.
#' @examples
#' adb_init(root = paste0(tempdir(), "/newDB"),
#'          version = "1.0.0", licence = "CC-BY-0.4",
#'          author = list(cre = "Jane Doe", aut = "John Doe", ctb = "Jamie Roe"),
#'          level = "ADM0")
#' @importFrom checkmate testDirectory testFileExists assertCharacter
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom utils citation
#' @export

adb_init <- function(root, version, author, licence, level){

  # in testing mode?
  testing <- .adb_state$testing

  assertCharacter(x = root, len = 1)
  assertCharacter(x = level, len = 1, any.missing = FALSE)
  if(str_detect(string = version, pattern = "_")){
    stop("please chose a version name that does not contain the symbol '_'")
  }

  # shitty windows workaround, because a directory may not have a trailing slash
  # for the function "file.exists()" used in assertDirectory()
  lastChar <- substr(x = root, start = nchar(root), stop = nchar(root))
  if(lastChar == "/"){
    root <- substr(root, start = 1, stop = nchar(root)-1)
  }

  # test whether the required directories exist and create them if they don't exist
  if(!testDirectory(x = root, access = "rw")){
    message("creating root directory ", root)
    dir.create(file.path(root))
  }

  for(sub in c("stage1", "stage2", "stage3", "schemas", "mappings")){
    if(!testDirectory(x = file.path(root, "vocabularies", sub), access = "rw")){
      message("creating ", paste0(".../vocabularies/", sub))
      dir.create(file.path(root, "vocabularies", sub), recursive = TRUE)
    }
  }

  for(sub in c("stage1", "stage2", "stage3", "schemas")){
    if(!testDirectory(x = file.path(root, "tables", sub), access = "rw")){
      message("creating ", paste0(".../tables/", sub))
      dir.create(file.path(root, "tables", sub), recursive = TRUE)
    }
  }

  for(sub in c("stage1", "stage2", "stage3")){
    if(!testDirectory(x = file.path(root, "geometries", sub), access = "rw")){
      message("creating ", paste0(".../geometries/", sub))
      dir.create(file.path(root, "geometries", sub), recursive = TRUE)
    }
  }

  # create the empty inventory tables, if they don't exist yet
  if(!testFileExists(x = file.path(root, "inventory.rds"))){
    dataseries <- tibble(datID = integer(),
                         name = character(),
                         description = character(),
                         homepage = character(),
                         version = character(),
                         licence_link = character(),
                         notes = character())

    vocabularies <- tibble(vocID = integer(),
                           datID = integer(),
                           name = character(),
                           description = character(),
                           version = character(),
                           licence_link = character(),
                           stage2_name = character(),
                           schema = character(),
                           stage1_name = character(),
                           stage1_url = character(),
                           download_date = as.Date(NA),
                           status = character(),
                           notes = character())

    tables <- tibble(tabID = integer(),
                     geoID = integer(),
                     datID = integer(),
                     geography = character(),
                     level = character(),
                     start_period = numeric(),
                     end_period = numeric(),
                     stage2_name = character(),
                     schema = character(),
                     stage1_name = character(),
                     stage1_url = character(),
                     download_date = as.Date(NA),
                     update_frequency = character(),
                     metadata_url = character(),
                     metadata_path = character(),
                     status = character(),
                     notes = character())

    geometries <- tibble(geoID = integer(),
                         datID = integer(),
                         stage2_name = character(),
                         layer = character(),
                         label = character(),
                         ancillary = character(),
                         stage1_name = character(),
                         stage1_url = character(),
                         download_date = as.Date(NA),
                         update_frequency = character(),
                         status = character(),
                         notes = character())

    inventory <- list(dataseries = dataseries, vocabularies = vocabularies,
                      tables = tables, geometries = geometries)

    message("creating ", paste0(".../inventory.rds"))
    saveRDS(object = inventory, file = paste0(root, "/inventory.rds"))
  }

  if(!testFileExists(x = paste0(root, "db_info.RData"))){
    db_info <- list(version = version,
                    author = author,
                    licence = licence,
                    level = level)

    message("creating ", paste0(".../db_info.RData"))
    save(db_info, file = paste0(root, "/db_info.RData"))
  }

  .adb_state$path <- root
}