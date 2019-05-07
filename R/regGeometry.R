#' Register a new geometry entry
#'
#' This function registers a new geometry of administrative units into the
#' LUCKINet geospatial database.
#' @param nation [\code{character(1)}]\cr wither the nation name or the column
#'   that contains nation names.
#' @param level [\code{integerish(1)}]\cr the administrative level at which the
#'   boundaries are recorded.
#' @param subset [\code{character(1)}]\cr optional argument to specify which
#'   subset the file contains. This could be a subset of administrative units or
#'   of commodities.
#' @param gSeries [\code{character(1)}]\cr the geometry series name.
#' @param layer [\code{character}]\cr the name of the layer from which the
#'   geometry should be created.
#' @param nameCol [\code{character(.)}]\cr the columns in which the names of
#'   administrative units are to be found, delimited by \code{"|"}; see
#'   Examples.
#' @param archive [\code{character(1)}]\cr the original file from which the
#'   boundaries emerge.
#' @param notes [\code{character(1)}]\cr optional notes that are assigned to all
#'   features of this geometry.
#' @param update [\code{logical(1)}]\cr whether or not the physical files should
#'   be updated (\code{TRUE}). or the function should merely return the new
#'   object (\code{FALSE} default).
#' @details When processing geometries/shapfiles to which census data are
#'   assigned, carry out the following steps: \enumerate{ \item Determine the
#'   \code{nation}, the \code{subset} of administrative units (if applicable),
#'   the geometry series and the \code{administrative level} and provide them as
#'   arguments to this function. \item Run the function. \item Export the
#'   shapefile via QGIS with the following properties: \itemize{ \item Format:
#'   GeoPackage \item File name: What is provided as message by this function
#'   \item CRS: EPSG:4326 - WGS 84 \item make sure that 'all fields are
#'   exported'} \item Confirm that you have saved the file.}
#' @examples
#' \dontrun{
#' setPath(root = "/home/se87kuhe/Nextcloud/LUCKINet/data/")
#'
#' # dry-run to be able to check whether everything is as intended.
#' argGeom <- regGeometry(nation = "Argentina", gSeries = "maia",
#'                        level = 3, nameCol = "NAM",
#'                        archive = "DEPARTAMENTOS.zip")
#'
#' # with several countries in one geometry (specify nation column)
#' latAmGeoms <- regGeometry(nation = "COUNTRY_NA", gSeries = "lamer",
#'                           level = 2, nameCol = "NAME1_",
#'                           archive = "lamer_ad1.shp")
#'
#' # eventually, carry out the registration
#' regGeometry(nation = "Argentina", gSeries = "maia", level = 3,
#'             nameCol = "NAM", archive = "DEPARTAMENTOS.zip",
#'             update = TRUE)
#' }
#' @importFrom checkmate assertCharacter assertIntegerish assertFileExists
#'   testChoice assertLogical
#' @importFrom readr read_csv
#' @importFrom dplyr filter distinct
#' @importFrom sf st_layers
#' @importFrom tibble tibble
#' @export

regGeometry <- function(nation = NULL, subset = NULL, gSeries = NULL, level = 3,
                        layer = NULL, nameCol = NULL, archive = NULL, notes = NULL,
                        update = FALSE){

  # set internal paths
  intPaths <- paste0(getOption(x = "dmt_path"))

  # get tables
  inv_geometries <- read_csv(paste0(intPaths, "/inv_geometries.csv"), col_types = "iciccccDcc")

  # check validity of arguments
  assertNames(x = colnames(inv_geometries), permutation.of = c("geoID", "name", "level", "source_file", "layer", "nation_column", "unit_column", "date", "orig_file", "notes"))
  assertCharacter(x = nation, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = subset, any.missing = FALSE, null.ok = TRUE)
  if(!is.null(subset)){
    if(grepl(pattern = "_", x = subset)){
      stop("please give a subset that does not contain any '_' characters.")
    }
  }
  assertCharacter(x = gSeries, ignore.case = TRUE, any.missing = FALSE, len = 1)
  if(!is.null(gSeries)){
    if(grepl(pattern = "_", x = gSeries)){
      stop("please give a geometry series name that does not contain any '_' characters.")
    }
  }
  assertIntegerish(x = level, any.missing = FALSE, len = 1)
  assertCharacter(x = layer, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = archive, any.missing = FALSE, null.ok = TRUE)
  if(is.null(archive)){
    archive <- readline("... How is the original file named? ")
  }
  assertFileExists(x = paste0(intPaths, "/cT_geometries/original_datasets/", archive))
  assertCharacter(x = notes, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertLogical(x = update, len = 1)

  # determine nation value
  if(!testChoice(x = tolower(nation), choices = countries$nation)){
    theNation <- NULL
    if(is.null(nation)){
      stop("please provide the name of a column that contains valid nations.")
    }
  } else{
    nations <- tolower(nation)
    assertChoice(x = nations, choices = countries$nation)
    theNation <- countries %>%
      filter(nation == nations) %>%
      distinct(iso_a3) %>%
      tolower()
    nation <- ""
  }

  if(is.null(notes)){
    notes = NA_character_
  }

  # put together file name and get confirmation that file should exist now
  fileName <- paste0(theNation, "_", level, "_", subset, "_", gSeries, ".gpkg")
  message(fileName)
  done <- readline(paste0("... press any key when the file '", fileName, "' is stored: "))
  filePath <- paste0(intPaths, "/cT_geometries/stage1/", fileName)

  # make sure that the file is really there
  assertFileExists(x = filePath, access = "r", extension = "gpkg")

  # also test whether the archive file is available ...
  filesTrace <- str_split(archive, "\\|")[[1]]
  assertFileExists(x = paste0(intPaths, "/cT_geometries/original_datasets/", filesTrace[1]), "r")

  # ... and if it is compressed, whether also the file therein is given that contains the data
  if(testCompressed(x = filesTrace[1]) & length(filesTrace) < 2){
    theArchiveFile <- readline(paste0("please give the name of the file in ", filesTrace[1]," that contains the geometries: "))
  }
  archive <- paste0(archive, "|", theArchiveFile)

  # determine which layers exist and ask the user which to chose, if none is
  # given
  layers <- st_layers(dsn = filePath)
  if(length(layers$name) != 1){
    if(is.null(layer)){
      layer <- readline("... Please type in the name of the layer to chose. ")
    }
    assertChoice(x = layer, choices = layers$name)
  } else{
    layer <- layers$name
  }

  # to check that what has been given in 'nation' is in fact a column in the geometry, load it
  if(is.null(theNation)){
    theGeometry <- read_sf(dsn = filePath,
                           layer = layer,
                           stringsAsFactors = FALSE)
    assertChoice(x = nation, choices = colnames(theGeometry))
  }

  # construct new documentation
  newGID <- ifelse(length(inv_geometries$geoID)==0, 1, as.integer(max(inv_geometries$geoID)+1))
  doc <- tibble(geoID = newGID,
                name = gSeries,
                level = level,
                source_file = fileName,
                layer = layer,
                nation_column = nation,
                unit_column = nameCol,
                date = Sys.Date(),
                orig_file = archive,
                notes = notes)

  if(update){
    # in case the user wants to update, attach the new information to the table
    # inv_geometries.csv
    updateIndex(index = doc, name = "inv_geometries")
  }

  return(doc)
}
