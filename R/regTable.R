#' Register a new areal data table
#'
#' This function registers a new areal data table into the geospatial database.
#' @param nation [\code{character(1)}]\cr the nation for which the areal data
#'   are valid.
#' @param subset [\code{character(1)}]\cr optional argument to specify which
#'   subset the file contains. This could be a subset of territorial units (e.g.
#'   only one municipality) or of a target variable.
#' @param dSeries [\code{character(1)}]\cr the dataseries of the areal data.
#' @param gSeries [\code{character(1)}]\cr optinally, the dataseries of the
#'   geometries, if the geometry dataseries deviates from the dataseries of the
#'   areal data.
#' @param level [\code{integerish(1)}]\cr the administrative level at which the
#'   boundaries are recorded.
#' @param variable [\code{character(.)}]\cr the variables included in the file.
#' @param algo [\code{character}]\cr a serial number that indicates permutations
#'   of the areal data dataseries.
#' @param begin [\code{integerish(1)}]\cr the date from which on the boundaries
#'   are valid.
#' @param end [\code{integerish(1)}]\cr the date until which the boundaries are
#'   valid.
#' @param archive [\code{character(1)}]\cr the original file from which the
#'   boundaries emerge.
#' @param notes [\code{character(1)}]\cr optional notes.
#' @param update [\code{logical(1)}]\cr whether or not the file 'inv_tables.csv'
#'   should be updated.
#' @details When processing areal data tables, carry out the following steps:
#'   \enumerate{ \item Determine the \code{nation}, administrative \code{level},
#'   a \code{subset} (if applicable), the \code{dataseries} of the areal data
#'   and of the geometry and an \code{algo}rithm (if applicable), and provide
#'   them as arguments to this function. \item Provide a \code{begin} and
#'   \code{end} date for the areal data. \item Run the function. \item (Re)Save
#'   the table with the following properties: \itemize{\item Format: csv \item
#'   Encoding: UTF-8 \item File name: What is provided as message by this
#'   function \item make sure that the file is not modified or reshaped. This
#'   will happen during data normalisation via the schema description, which
#'   expects the original table.} \item Confirm that you have saved the file.}
#'
#'   Every areal data dataseries (\code{dSeries}) may come as a slight
#'   permutation of a particular table arrangement. The function
#'   \code{\link{normalise}} expects internally a schema description (a list
#'   that describes the position of the data components) for each data table,
#'   which is saved as \code{paste0("meta_", dSeries, algo)}. A template
#'   thereof, and documentation on how to set them up, comes as the object
#'   \code{\link{meta_default}} with \code{arealDB}.
#' @return Returns the entry that is appended to 'inv_tables.csv' in case
#'   \code{update = TRUE}.
#' @examples
#' \dontrun{
#'
#' regTable(nation = "United States of America",
#'          subset = "soy",
#'          dSeries = "usda", gSeries = "gadm",
#'          level = 3,
#'          variable = c("harvested_area"),
#'          algo = 1,
#'          begin = 1990, end = 2017,
#'          archive = "soybean_us_county_1990_2017.csv",
#'          update = TRUE)
#' }
#' @importFrom readr read_csv
#' @importFrom checkmate assertDataFrame assertNames assertCharacter
#'   assertIntegerish assertSubset assertLogical testChoice assertChoice
#'   assertFileExists
#' @importFrom dplyr filter distinct
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @export

regTable <- function(nation = NULL, subset = NULL, dSeries = NULL, gSeries = NULL,
                     level = NULL, variable = NULL, algo = NULL, begin = NULL,
                     end = NULL, archive = NULL, notes = NULL, update = FALSE){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))

  # get tables
  inv_tables <- read_csv(paste0(intPaths, "/inv_tables.csv"), col_types = "iiicDcc")
  inv_dataseries <- read_csv(paste0(intPaths, "/inv_dataseries.csv"), col_types = "icccc")
  inv_geometries <- read_csv(paste0(intPaths, "/inv_geometries.csv"), col_types = "iciccccDcc")

  # create a new data series, if dSeries is not part of the currently known data series names
  if(!any(inv_dataseries$name %in% dSeries)){
    stop(paste0("please first create the new data table dataseries '", dSeries, "' via 'regDataseries()'"))

  } else{
    dataSeries <- inv_dataseries$datID[inv_dataseries$name %in% dSeries]
  }
  # create a new geometry series, if gSeries is not part of the currently known geometry series names
  if(!any(inv_dataseries$name %in% gSeries)){
    stop(paste0("please first create the new geometry dataseries '", gSeries,"' via 'regDataseries()'"))
  } else {
    geomSeries <- inv_dataseries$datID[inv_dataseries$name %in% gSeries]
  }

  # check validity of arguments
  assertDataFrame(x = inv_tables, ncols = 7)
  assertNames(x = colnames(inv_tables), permutation.of = c("tabID", "datID", "geoID", "source_file", "date", "orig_file", "notes"))
  assertNames(x = colnames(inv_dataseries), permutation.of = c("datID", "name", "long_name", "website", "notes"))
  assertNames(x = colnames(inv_geometries), permutation.of = c("geoID", "datID", "level", "source_file", "layer", "nation_column", "unit_column", "date", "orig_file", "notes"))
  assertCharacter(x = nation, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertIntegerish(x = level, any.missing = FALSE, len = 1)
  assertCharacter(x = subset, any.missing = FALSE, null.ok = TRUE)
  if(!is.null(subset)){
    if(grepl(pattern = "_", x = subset)){
      stop("please give a subset that does not contain any '_' characters.")
    }
  }
  assertCharacter(x = dSeries, ignore.case = TRUE, any.missing = FALSE, len = 1)
  if(!is.null(dSeries)){
    if(grepl(pattern = "_", x = dSeries)){
      stop("please give a data series name that does not contain any '_' characters.")
    }
  }
  assertCharacter(x = gSeries, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  if(is.null(gSeries)){
    gSeries <- dSeries
  } else{
    if(grepl(pattern = "_", x = gSeries)){
      stop("please give a geometry series name that does not contain any '_' characters.")
    }
  }
  assertCharacter(x = variable, ignore.case = TRUE, any.missing = FALSE)
  assertSubset(x = variable, choices = c("planted_area", "harvested_area", "production", "yield", "headcount", "animal_units"))
  assertIntegerish(x = algo, any.missing = FALSE, len = 1)
  assertIntegerish(x = begin, any.missing = FALSE, len = 1, lower = 1900)
  assertIntegerish(x = end, any.missing = FALSE, len = 1, upper = as.integer(format(Sys.Date(), "%Y")))
  assertCharacter(x = archive, any.missing = FALSE)
  assertCharacter(x = notes, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertLogical(x = update, len = 1)

  # determine nation value
  if(!testChoice(x = tolower(nation), choices = countries$nation)){
    theNation <- NULL
  } else{
    nations <- tolower(nation)
    assertChoice(x = nations, choices = countries$nation)
    theNation <- countries %>%
      filter(nation == nations) %>%
      distinct(iso_a3) %>%
      tolower()
  }

  if(is.null(notes)){
    notes = NA_character_
  }

  tempDim <- NULL
  if(any(grep("planted_area", variable, ignore.case = TRUE))){
    tempDim <- paste0(tempDim, "B")
  }
  if(any(grep("harvested_area", variable, ignore.case = TRUE))){
    tempDim <- paste0(tempDim, "N")
  }
  if(any(grep("production", variable, ignore.case = TRUE))){
    tempDim <- paste0(tempDim, "P")
  }
  if(any(grep("yield", variable, ignore.case = TRUE))){
    tempDim <- paste0(tempDim, "Y")
  }
  if(any(grep("headcount", variable, ignore.case = TRUE))){
    tempDim <- paste0(tempDim, "H")
  }
  if(any(grep("animal_units", variable, ignore.case = TRUE))){
    tempDim <- paste0(tempDim, "U")
  }

  # put together file name and get confirmation that file should exist now
  fileName <- paste0(theNation, "_", level, "_", subset, "_", dSeries, "_", tempDim, "_", algo, "_", begin, "_", end, ".csv")
  if(any(inv_tables$source_file %in% fileName)){
    return(paste0("'", fileName, "' has already been registered."))
  }

  # test whether the archive file is available
  filesTrace <- str_split(archive, "\\|")[[1]]
  if(!testFileExists(x = paste0(intPaths, "/adb_tables/stage1/", filesTrace[1]), "r")){
    done <- readline(paste0("... please store the archive '", filesTrace[[1]], "' in './adb_tables/stage1'\n  -> press any key when done: "))

    # make sure that the file is really there
    assertFileExists(x = paste0(intPaths, "/adb_tables/stage1/", filesTrace[1]), "r")

    # ... and if it is compressed, whether also the file therein is given that contains the data
    if(testCompressed(x = filesTrace[1]) & length(filesTrace) < 2){
      theArchiveFile <- readline(paste0("please give the name of the file in ", filesTrace[1]," that contains the table: "))
      archive <- paste0(archive, "|", theArchiveFile)
    }
  }

  # test that the file is available
  filePath <- paste0(intPaths, "/adb_tables/stage2/", fileName)
  if(!testFileExists(x = filePath, "r", extension = "csv")){
    done <- readline(paste0("... please store the table as '", fileName, "' in './adb_tables/stage2'\n  -> press any key when done: "))

    # make sure that the file is really there
    assertFileExists(x = filePath, "r", extension = "csv")
  }

  geomSeries <- inv_geometries[inv_geometries$datID %in% geomSeries,] %>%
    filter(level == !!level) %>%
    pull("geoID")
  if(length(geomSeries) == 0){
    cat(paste0("\nI did not find a geometry series '", gSeries, "' at the requested level, registering one now ...\n"))

    geomSeries <- regGeometry(nation = theNation,
                              gSeries = gSeries,
                              level = level,
                              update = update)
    geomSeries <- geomSeries$geoID
  }

  # put together new census database entry
  newCID <- ifelse(length(inv_tables$tabID)==0, 1, as.integer(max(inv_tables$tabID)+1))
  doc <- tibble(tabID = newCID,
                geoID = geomSeries,
                datID = dataSeries,
                source_file = fileName,
                date = Sys.Date(),
                orig_file = archive,
                notes = notes)

  if(update){
    if(!any(inv_tables$source_file %in% fileName)){
      # in case the user wants to update, attach the new information to the table inv_sourceData.csv
      updateTable(index = doc, name = "inv_tables")
    }
  }
  return(doc)

}
