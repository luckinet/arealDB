#' Register a new geometry entry
#'
#' This function registers a new geometry of territorial units into the
#' geospatial database.
#' @param ... [\code{character(1)}]\cr optional named argument selecting the
#'   main territory into which this geometry is nested. The name of this must be
#'   a class of the gazetteer and the value must be one of the territory names
#'   of that class, e.g. \emph{nation = "Estonia"}.
#' @param subset [\code{character(1)}]\cr optional argument to specify which
#'   subset the file contains. This could be a subset of territorial units (e.g.
#'   only one municipality) or of a target variable.
#' @param gSeries [\code{character(1)}]\cr the name of the geometry dataseries
#'   (see \code{\link{regDataseries}}).
#' @param label [\code{list(.)}]\cr list of as many columns as there are in
#'   common in the ontology and this geometry. Must be of the form
#'   \code{list(class = columnName)}, with 'class' as the class of the ontology
#'   corresponding to the respective column name in the geometry.
#' @param layer [\code{character}]\cr the name of the file's layer from which
#'   the geometry should be created (if applicable).
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
#'   carry out the following steps: \enumerate{ \item Determine the main
#'   territory (such as a nation, or any other polygon), a \code{subset} (if
#'   applicable), the dataseries of the geometry and the ontology \code{label},
#'   and provide them as arguments to this function. \item Run the function.
#'   \item Export the shapefile with the following properties: \itemize{ \item
#'   Format: GeoPackage \item File name: What is provided as message by this
#'   function \item CRS: EPSG:4326 - WGS 84 \item make sure that 'all fields are
#'   exported'} \item Confirm that you have saved the file.}
#' @return Returns a tibble of the entry that is appended to
#'   'inv_geometries.csv' in case \code{update = TRUE}.
#' @family register functions
#' @examples
#' if(dev.interactive()){
#'   # build the example database
#'   makeExampleDB(until = "regDataseries", path = tempdir())
#'
#'   # The GADM dataset comes as *.7z archive
#'   regGeometry(gSeries = "gadm",
#'               label = list(al1 = "NAME_0"),
#'               layer = "example_geom1",
#'               archive = "example_geom.7z|example_geom1.gpkg",
#'               archiveLink = "https://gadm.org/",
#'               nextUpdate = "2019-10-01",
#'               updateFrequency = "quarterly",
#'               update = TRUE)
#'
#'   # The second administrative level in GADM contains names in the columns
#'   # NAME_0 and NAME_1
#'   regGeometry(gSeries = "gadm",
#'               label = list(al1 = "NAME_0", al2 = "NAME_1"),
#'               layer = "example_geom2",
#'               archive = "example_geom.7z|example_geom2.gpkg",
#'               archiveLink = "https://gadm.org/",
#'               nextUpdate = "2019-10-01",
#'               updateFrequency = "quarterly",
#'               update = TRUE)
#' }
#' @importFrom checkmate assertNames assertCharacter assertIntegerish
#'   assertFileExists testChoice assertLogical testSubset
#' @importFrom ontologics load_ontology
#' @importFrom readr read_csv
#' @importFrom dplyr filter distinct
#' @importFrom stringr str_split
#' @importFrom sf st_layers read_sf
#' @importFrom tibble tibble
#' @export

regGeometry <- function(..., subset = NULL, gSeries = NULL, label = NULL,
                        layer = NULL, archive = NULL, archiveLink = NULL,
                        nextUpdate = NULL, updateFrequency = NULL, notes = NULL,
                        update = FALSE, overwrite = FALSE){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))
  gazPath <- paste0(getOption(x = "gazetteer_path"))
  topClass <- paste0(getOption(x = "gazetteer_top"))

  # get tables
  inv_dataseries <- read_csv(paste0(intPaths, "/inv_dataseries.csv"), col_types = "icccccc")
  inv_geometries <- read_csv(paste0(intPaths, "/inv_geometries.csv"), col_types = "iicccccDDcc")

  if(dim(inv_dataseries)[1] == 0){
    stop("'inv_dataseries.csv' does not contain any entries!")
  }

  # in testing mode?
  testing <- getOption(x = "adb_testing")

  # check validity of arguments
  assertList(x = label, any.missing = FALSE)
  assertCharacter(x = subset, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = gSeries, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = layer, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = archive, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = archiveLink, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = nextUpdate, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = updateFrequency, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = notes, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertLogical(x = update, len = 1)
  assertLogical(x = overwrite, len = 1)
  assertNames(x = colnames(inv_dataseries),
              permutation.of = c("datID", "name", "description", "homepage",
                                 "licence_link", "licence_path", "notes"))
  assertNames(x = colnames(inv_geometries),
              permutation.of = c("geoID", "datID", "source_file", "layer",
                                 "label", "orig_file", "orig_link", "download_date",
                                 "next_update", "update_frequency", "notes"))

  broadest <- exprs(..., .named = TRUE)
  if(length(broadest) > 0){
    mainPoly <- broadest[[1]]
  } else {
    mainPoly <- ""
  }

  gaz <- load_ontology(gazPath)
  gazClasses <- get_class(ontology = gazPath)
  # include here a test that checks whether broaders is actually part of the gazetteer
  # if(!testSubset(x = names(broadest), choices = unique(gaz$attributes$class))){
  #   stop("please specify a main category that is part of the classes in the gazetteer (",
  #        paste0(unique(gaz$attributes$class)[1:4], collapse = ", "), ")")
  # }

  if(!is.null(subset)){
    if(grepl(pattern = "_", x = subset)){
      stop("please give a subset that does not contain any '_' characters.")
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

  # if(is.null(label)){
  #   message("please type in the ontology label and the column name of the units (e.g. al1=NAME_0: ")
  #   if(!testing){
  #     theLabel <- readline()
  #   } else {
  #     theLabel <- "al1"
  #   }
  #   if(is.na(theLabel)){
  #     theLabel = NA_character_
  #   }
  # } else {
    assertSubset(x = names(label), choices = gazClasses$label)
    theLabel <- tail(names(label), 1)
    labelString <- paste0(paste0(names(label), "=", label), collapse = "|")
  # }

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

  # put together file name and get confirmation that file should exist now
  fileName <- paste0(mainPoly, "_", theLabel, "_", subset, "_", gSeries, ".gpkg")
  filePath <- paste0(intPaths, "/adb_geometries/stage2/", fileName)
  filesTrace <- str_split(archive, "\\|")[[1]]

  newGID <- ifelse(length(inv_geometries$geoID)==0, 1, as.integer(max(inv_geometries$geoID)+1))
  if(any(inv_geometries$source_file %in% fileName)){
    if(overwrite){
      newGID <- inv_geometries$geoID[which(inv_geometries$source_file %in% fileName)]
    } else {
      temp <- inv_geometries[which(inv_geometries$source_file %in% fileName), ]
      message(paste0("! the geometry '", fileName, "' has already been registered !"))
      return(temp)
    }
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
  if(!testFileExists(x = paste0(intPaths, "/adb_geometries/stage1/", filesTrace[1]))){
    message(paste0("... please store the archive '", filesTrace[[1]], "' in './adb_geometries/stage1'"))
    if(!testing){
      done <- readline(" -> press any key when done: ")
    }

    # make sure that the file is really there
    assertFileExists(x = paste0(intPaths, "/adb_geometries/stage1/", filesTrace[1]))

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
    if(!testFileExists(x = filePath, extension = "gpkg")){
      message(paste0("... please store the geometry as '", fileName, "' in './adb_geometries/stage2'"))
      if(!testing){
        done <- readline(" -> press any key when done: ")
      }
      # make sure that the file is really there
      assertFileExists(x = filePath, extension = "gpkg")
    }

    # to check that what has been given in 'nation' and 'nameCol' is in fact a
    # column in the geometry, load it
    if(is.null(mainPoly)){
      theGeometry <- read_sf(dsn = filePath, stringsAsFactors = FALSE)
    }

    nameCols <- unlist(label, use.names = FALSE)

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
    doc <- tibble(geoID = newGID,
                  datID = dataSeries,
                  source_file = fileName,
                  layer = layer,
                  label = labelString,
                  orig_file = archive,
                  orig_link = archiveLink,
                  download_date = Sys.Date(),
                  next_update = nextUpdate,
                  update_frequency = updateFrequency,
                  notes = notes)
    if(!any(inv_geometries$source_file %in% fileName) | overwrite){
      # in case the user wants to update, attach the new information to the table
      # inv_geometries.csv
      updateTable(index = doc, name = "inv_geometries", matchCols = c("source_file", "label"))
    }
    return(doc)
  } else {
    message(paste0("... the filename is '", fileName, "'."))
  }
}