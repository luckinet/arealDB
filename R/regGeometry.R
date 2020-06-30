#' Register a new geometry entry
#'
#' This function registers a new geometry of territorial units into the
#' geospatial database.
#' @param nation [\code{character(1)}]\cr either the nation name or the column
#'   of the file's attribute table that contains nations.
#' @param subset [\code{character(1)}]\cr optional argument to specify which
#'   subset the file contains. This could be a subset of territorial units (e.g.
#'   only one municipality) or of a target variable.
#' @param gSeries [\code{character(1)}]\cr the name of the geometry dataseries
#'   (see \code{\link{regDataseries}}).
#' @param level [\code{integerish(1)}]\cr the administrative level at which the
#'   geometry is recorded.
#' @param layer [\code{character}]\cr the name of the file's layer from which
#'   the geometry should be created (if applicable).
#' @param nameCol [\code{character(.)}]\cr the columns in which the names of
#'   administrative units are to be found, delimited by \code{"|"}; see
#'   Examples.
#' @param archive [\code{character(1)}]\cr the original file (perhaps a *.zip)
#'   from which the geometry emerges.
#' @param archiveLink [\code{character(1)}]\cr download-link of the archive.
#' @param nextUpdate [\code{character(1)}]\cr value describing the next
#'   anticipated update of this dataset (in YYYY-MM-DD format).
#' @param updateFrequency [\code{character(1)}]\cr value describing the
#'   frequency with which the dataset is updated, according to the ISO 19115
#'   Codelist, MD_MaintenanceFrequencyCode. Possible values are: 'continual',
#'   'daily', 'weekly', 'fortnightly', 'quarterly', 'biannually', 'annually',
#'   'asNeeded', 'irregular', 'notPlanned', 'unknown', 'periodic',
#'   'semimonthly', 'biennially'.
#' @param notes [\code{character(1)}]\cr optional notes that are assigned to all
#'   features of this geometry.
#' @param update [\code{logical(1)}]\cr whether or not the file
#'   'inv_geometries.csv' should be updated.
#' @param overwrite [\code{logical(1)}]\cr whether or not the geometry to
#'   register shall overwrite a potentially already existing older version.
#' @details When processing geometries to which areal data shall be linked,
#'   carry out the following steps: \enumerate{ \item Determine the
#'   \code{nation}, a \code{subset} (if applicable), the dataseries of the
#'   geometry and the administrative \code{level}, and provide them as arguments
#'   to this function. \item Run the function. \item Export the shapefile with
#'   the following properties: \itemize{ \item Format: GeoPackage \item File
#'   name: What is provided as message by this function \item CRS: EPSG:4326 -
#'   WGS 84 \item make sure that 'all fields are exported'} \item Confirm that
#'   you have saved the file.}
#' @return Returns a tibble of the entry that is appended to
#'   'inv_geometries.csv' in case \code{update = TRUE}.
#' @examples
#' # build the example database
#' makeExampleDB(until = "regDataseries")
#'
#' # The GADM dataset comes as *.zip archive
#' regGeometry(nation = "NAME_0",
#'             gSeries = "gadm",
#'             level = 1,
#'             layer = "example_geom1",
#'             nameCol = "NAME_0",
#'             archive = "example_geom.7z|example_geom1.gpkg",
#'             archiveLink = "https://gadm.org/",
#'             nextUpdate = "2019-10-01",
#'             updateFrequency = "quarterly",
#'             update = TRUE)
#'
#' # The second administrative level in GADM contains names in the columns
#' # NAME_0 and NAME_1
#' regGeometry(nation = "NAME_0",
#'             gSeries = "gadm",
#'             level = 2,
#'             layer = "example_geom2",
#'             nameCol = "NAME_0|NAME_1",
#'             archive = "example_geom.7z|example_geom2.gpkg",
#'             archiveLink = "https://gadm.org/",
#'             nextUpdate = "2019-10-01",
#'             updateFrequency = "quarterly",
#'             update = TRUE)
#' @importFrom checkmate assertNames assertCharacter assertIntegerish
#'   assertFileExists testChoice assertLogical
#' @importFrom readr read_csv
#' @importFrom dplyr filter distinct
#' @importFrom stringr str_split
#' @importFrom sf st_layers read_sf
#' @importFrom tibble tibble
#' @export

regGeometry <- function(nation = NULL, subset = NULL, gSeries = NULL, level = NULL,
                        layer = NULL, nameCol = NULL, archive = NULL, archiveLink = NULL,
                        nextUpdate = NULL, updateFrequency = NULL, notes = NULL,
                        update = FALSE, overwrite = FALSE){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))

  # get tables
  inv_dataseries <- read_csv(paste0(intPaths, "/inv_dataseries.csv"), col_types = "icccccc")
  inv_geometries <- read_csv(paste0(intPaths, "/inv_geometries.csv"), col_types = "iiiccccccDccc")

   if(dim(inv_dataseries)[1] == 0){
    stop("'inv_dataseries.csv' does not contain any entries!")
  }

  # in testing mode?
  testing <- getOption(x = "adb_testing")

  # check validity of arguments
  assertNames(x = colnames(inv_geometries),
              permutation.of = c("geoID", "datID", "level", "source_file", "layer",
                                 "nation_column", "unit_column", "orig_file", "orig_link", "download_date",
                                 "next_update", "update_frequency", "notes"))
  assertCharacter(x = nation, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = subset, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = gSeries, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertIntegerish(x = level, any.missing = FALSE, len = 1, lower = 1, null.ok = TRUE)
  assertCharacter(x = layer, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = archive, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = archiveLink, any.missing = FALSE, null.ok = TRUE)
  # maybe assertDate
  assertCharacter(x = nextUpdate, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = updateFrequency, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = notes, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertLogical(x = update, len = 1)

  # ask for missing and required arguments
  if(is.null(nation)){
    message("please type in either a nation or the name of the column that contains nation names: ")
    if(!testing){
      nation <- readline()
    } else {
      nation <- countries$nation[11]
    }
    if(is.na(nation)){
      nation = NA_character_
    }
  }

  if(!is.null(subset)){
    if(grepl(pattern = "_", x = subset)){
      stop("please give a subset that does not contain any '_' characters.")
    }
  }

  if(is.null(nameCol)){
    message("please type in the name of the column that contains unit names: ")
    if(!testing){
      nameCol <- readline()
    } else {
      nameCol <- "units"
    }
  }

  if(is.null(gSeries)){
    message("please type in to which series the geometry belongs: ")
    if(!testing){
      gSeries <- readline()
    } else {
      gSeries <- "gadm"
    }

    if(grepl(pattern = "_", x = gSeries)){
      stop("! please give a geometry series name that does not contain any '_' characters !")
    }

    if(!testing){
      if(!any(inv_dataseries$name %in% gSeries)){
        stop(paste0("! please first create the new geometry series '", gSeries,"' via 'regDataseries()' !"))
      }
    } else {
      dataSeries <- NA_integer_
    }
  } else{
    dataSeries <- inv_dataseries$datID[inv_dataseries$name %in% gSeries]
    if(length(dataSeries) < 1){
      stop(paste0("! please first create the new geometry series '", gSeries,"' via 'regDataseries()' !"))
    }
  }

  if(is.null(level)){
    message("please type in the administrative level of the units: ")
    if(!testing){
      level <- readline()
    } else {
      level <- 1
    }
    if(is.na(level)){
      level = NA_integer_
    }
  }

  if(is.null(archive)){
    message("please type in the archives' file name: ")
    if(!testing){
      archive <- readline()
    } else {
      archive <- "example_geom.7z"
    }
    if(is.na(archive)){
      archive = NA_character_
    }
  }

  # determine nation value
  if(!testChoice(x = tolower(nation), choices = countries$nation)){
    theNation <- NULL
  } else{
    nations <- tolower(nation)
    assertChoice(x = nations, choices = countries$nation)
    theNation <- countries %>%
      as_tibble() %>%
      filter(nation == nations) %>%
      distinct(iso_a3) %>%
      tolower()
    nation <- ""
  }

  # put together file name and get confirmation that file should exist now
  fileName <- paste0(theNation, "_", level, "_", subset, "_", gSeries, ".gpkg")
  filePath <- paste0(intPaths, "/adb_geometries/stage2/", fileName)
  filesTrace <- str_split(archive, "\\|")[[1]]

  if(any(inv_geometries$source_file %in% fileName) & !overwrite){
    return(paste0("'", fileName, "' has already been registered."))
  }

  if(is.null(archiveLink)){
    message("please type in the weblink from which the archive was downloaded: ")
    if(!testing){
      archiveLink <- readline()
    } else {
      archiveLink <- "https://gadm.org/downloads/example_geom.7z.html"
    }
    if(is.na(archiveLink)){
      archiveLink = NA_character_
    }
  }

  if(is.null(updateFrequency)){
    message(paste("please type in the frequency in which the table gets updated ..."))
    if(!testing){
      updateFrequency <- readline(" -> select one of: continual, daily, weekly, fortnightly, quarterly, biannually, annually, asNeeded, irregular, notPlanned, unknown, periodic, semimonthly, biennially: ")
      while(!is.element(updateFrequency,
                        c("continual", "daily","weekly", "fortnightly", "quarterly", "biannually", "annually", "asNeeded", "irregular", "notPlanned", "unknown", "periodic", "semimonthly", "biennially"))){
        # test missing
        message(paste(" -> input one of: continual, daily, weekly, fortnightly, quarterly, biannually, annually, asNeeded, irregular, notPlanned, unknown, periodic, semimonthly, biennially"))
        updateFrequency <- readline()
      }
    } else {
      updateFrequency <- "quarterly"
    }
    if(is.na(updateFrequency)){
      updateFrequency = as.Date(NA)
    }
  }

  if(is.null(nextUpdate)){
    if(updateFrequency %in% c("asNeeded", "notPlanned", "unknown")){
      nextUpdate <- as.Date(NA)
    } else {
      message("please type in when the geometry gets its next update (YYYY-MM-DD): ")
      if(!testing){
        nextUpdate <- as.Date(readline(), "%Y-%m-%d")
      } else {
        nextUpdate <- as.Date("2019-10-01", "%Y-%m-%d")
      }
    }
    if(is.na(nextUpdate)){
      nextUpdate = as.Date(NA)
    }
  }

  if(is.null(notes)){
    notes = NA_character_
  }

  # test whether the archive file is available
  if(!testFileExists(x = paste0(intPaths, "/adb_geometries/stage1/", filesTrace[1]), "r")){
    message(paste0("... please store the archive '", filesTrace[[1]], "' in './adb_geometries/stage1'"))
    if(!testing){
      done <- readline(" -> press any key when done: ")
    }

    # make sure that the file is really there
    assertFileExists(x = paste0(intPaths, "/adb_geometries/stage1/", filesTrace[1]), access = "r")

    # ... and if it is compressed, whether also the file therein is given that contains the data
    if(testCompressed(x = filesTrace[1]) & length(filesTrace) < 2){
      message(paste0("please give the name of the file in ", filesTrace[1]," that contains the geometries: "))
      if(!testing){
        theArchiveFile <- readline()
      } else {
        theArchiveFile <- "1__gadm.gpkg"
      }
      archive <- paste0(archive, "|", theArchiveFile)
    }
  }

  # test whether the geometry file is available and proper
  if(update){
    if(!testFileExists(x = filePath, access = "r", extension = "gpkg")){
      message(paste0("... please store the geometry as '", fileName, "' in './adb_geometries/stage2'"))
      if(!testing){
        done <- readline(" -> press any key when done: ")
      }
      # make sure that the file is really there
      assertFileExists(x = filePath, access = "r", extension = "gpkg")
    }

    # to check that what has been given in 'nation' and 'nameCol' is in fact a
    # column in the geometry, load it
    if(is.null(theNation)){
      theGeometry <- read_sf(dsn = filePath,
                             stringsAsFactors = FALSE)
      assertChoice(x = nation, choices = colnames(theGeometry))
    }

    # determine which layers exist and ask the user which to chose, if none is
    # given
    layers <- st_layers(dsn = filePath)
    if(length(layers$name) != 1){
      if(is.null(layer)){
        message(paste0("... Please chose only one of the layers ", paste0(layers$name, collapse = ", "), ": "))
        if(!testing){
          layer <- readline()
        } else {
          layer <- "example_geom"
        }
      }
      assertChoice(x = layer, choices = layers$name)
    } else{
      layer <- layers$name
    }

    # construct new documentation
    newGID <- ifelse(length(inv_geometries$geoID)==0, 1, as.integer(max(inv_geometries$geoID)+1))
    doc <- tibble(geoID = newGID,
                  datID = dataSeries,
                  level = level,
                  source_file = fileName,
                  layer = layer,
                  nation_column = nation,
                  unit_column = nameCol,
                  orig_file = archive,
                  orig_link = archiveLink,
                  download_date = Sys.Date(),
                  next_update = nextUpdate,
                  update_frequency = updateFrequency,
                  notes = notes)
    if(!any(inv_geometries$source_file %in% fileName)){
      # in case the user wants to update, attach the new information to the table
      # inv_geometries.csv
      updateTable(index = doc, name = "inv_geometries")
    }
    return(doc)
  } else {
    message(paste0("... the filename is '", fileName, "'."))
  }
}
