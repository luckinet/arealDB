#' Register a new census data entry
#'
#' This function creates the standardized name for a new census file and enters
#' the required information into all look-up tables.
#' @param nation [\code{character(1)}]\cr the nation for which the boundaries
#'   are defined.
#' @param level [\code{integerish(1)}]\cr the administrative level at which the
#'   boundaries are recorded.
#' @param subset [\code{character(1)}]\cr optional argument to specify which
#'   subset the file contains. This could be a subset of administrative units or
#'   of commodities.
#' @param dSeries [\code{character(1)}]\cr the dataseries of the census data.
#' @param gSeries [\code{character(1)}]\cr optinally, if the geometry series
#'   deviates from the data series with which the census data should be
#'   registered.
#' @param variable [\code{character(.)}]\cr the variables included in
#'   the file.
#' @param algo [\code{character}]\cr the dataseries specific algorithm
#'   associated to the specific format of this file.
#' @param begin [\code{integerish(1)}]\cr the first date from which on the
#'   boundaries are valid.
#' @param end [\code{integerish(1)}]\cr the last date from which on the
#'   boundaries are valid.
#' @param archive [\code{character(1)}]\cr the original file from which the
#'   boundaries emerge.
#' @param notes [\code{character(1)}]\cr optional notes.
#' @param update [\code{logical(1)}]\cr whether or not the file
#'   'inv_sourceData.csv' should be updated.
#' @details When processing census data, carry out the following steps:
#'   \enumerate{ \item Determine the \code{nation}, \code{administrative level},
#'   the \code{subset} of the nation and the \code{dataseries} of the shapefiles
#'   and provide them as arguments to this function. \item Provide a
#'   \code{begin} and \code{end} date for the census data. \item Run the
#'   function. \item (Re)Save the table with the following properties:
#'   \itemize{\item Format: csv \item Encoding: UTF-8 \item File name: What is
#'   provided as message by this function \item make sure that no columns or
#'   rows are removed. This will happen later via a \code{rect*} function that
#'   expects the original table.} \item Confirm that you have saved the file.}
#' @examples
#' \dontrun{
#'
#' setPath(root = "/home/se87kuhe/Nextcloud/LUCKINet/data/")
#'
#' # dry run to be able to check whether everything is as intended.
#' regCensus(nation = "Argentina", level = 3, dSeries = "maia",
#'           variable = c("planted_area", "harvested_area", "production", "yield"),
#'           algo = 1, begin = 1969, end = 2017,
#'           archive = "AgroIndustria_Estimaciones_complete_2017_11_12.xlsx")
#'
#' # with several countries in one table (nation is NULL)
#' regCensus(nation = NULL, level = 1, dSeries = "faostat", gSeries = "gadm",
#'           variable = c("planted_area","production","yield"),
#'           algo = 1, begin = 1961, end = 2017,
#'           archive = "Production_Crops_E_All_Data_(Normalized).csv")
#'
#' # eventually, carry out the registration
#' regCensus(nation = "Argentina", level = 3, dSeries = "maia",
#'           variable = c("planted_area", "harvested_area", "production", "yield"),
#'           algo = 1, begin = 1969, end = 2017,
#'           archive = "AgroIndustria_Estimaciones_complete_2017_11_12.xlsx",
#'           update = TRUE)
#' }
#' # when the data series and the geometry series are different, specify
#' # both of them
#' @importFrom checkmate assertCharacter assertSubset assertChoice
#'   assertIntegerish
#' @importFrom dplyr filter distinct
#' @importFrom readr read_csv
#' @importFrom stringr str_split
#' @export

regCensus <- function(nation = NULL, subset = NULL, dSeries = NULL, gSeries = NULL,
                      level = NULL, variable = NULL, algo = NULL, begin = NULL,
                      end = NULL, archive = NULL, notes = NULL, update = FALSE){

  # set internal paths
  intPaths <- paste0(getOption(x = "cT_path"))

  # get tables
  inv_census <- read_csv(paste0(intPaths, "/inv_census.csv"), col_types = "iiicDcc")
  inv_dataseries <- read_csv(paste0(intPaths, "/inv_dataseries.csv"), col_types = "icccc")
  inv_geometries <- read_csv(paste0(intPaths, "/inv_geometries.csv"), col_types = "iciccccDcc")

  # check validity of arguments
  assertDataFrame(x = inv_census, ncols = 7)
  assertNames(x = colnames(inv_census), permutation.of = c("cenID", "datID", "geoID", "source_file", "date", "orig_file", "notes"))
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
  done <- readline(paste0("... please store the table as '", fileName, "' in './cT_census/stage2'\n  -> press any key when done: "))

  # make sure that the file is really there
  assertFileExists(x = paste0(intPaths, "/cT_census/stage2/", fileName), "r", extension = "csv")

  # also test whether the archive file is available ...
  filesTrace <- str_split(archive, "\\|")[[1]]
  assertFileExists(x = paste0(intPaths, "/cT_census/stage1/", filesTrace[1]), "r")

  # ... and if it is compressed, whether also the file therein is given that contains the data
  if(testCompressed(x = filesTrace[1]) & length(filesTrace) < 2){
    theArchiveFile <- readline(paste0("please give the name of the file in ", filesTrace[1]," that contains the table: "))
    archive <- paste0(archive, "|", theArchiveFile)
  }

  # create a new data series, if dSeries is not part of the currently known data series names
  if(!any(inv_dataseries$name %in% dSeries)){
    dataSeries <- regDataseries(name = dSeries,
                                update = update)
    dataSeries <- dataSeries$datID
  } else{
    dataSeries <- inv_dataseries$datID[inv_dataseries$name %in% dSeries]
  }
  # create a new geometry series, if gSeries is not part of the currently known geometry series names
  if(!any(inv_dataseries$name %in% gSeries)){
    geomSeries <- regDataseries(name = gSeries,
                                update = update)
    geomSeries <- geomSeries$datID
  } else {
    geomSeries <- inv_dataseries$datID[inv_dataseries$name %in% gSeries]
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
  newCID <- ifelse(length(inv_census$cenID)==0, 1, as.integer(max(inv_census$cenID)+1))
  doc <- tibble(cenID = newCID,
                geoID = geomSeries,
                datID = dataSeries,
                source_file = fileName,
                date = Sys.Date(),
                orig_file = archive,
                notes = notes)

  if(update){
    # in case the user wants to update, attach the new information to the table inv_sourceData.csv
    updateIndex(index = doc, name = "inv_census")
  }

  # updateCensus()
  return(doc)

}
