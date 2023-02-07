#' Register a new areal data table
#'
#' This function registers a new areal data table into the geospatial database.
#' @param ... [\code{character(1)}]\cr name and value of the topmost unit under
#'   which the table shall be registered. The name of this must be a class of
#'   the gazetteer and the value must be one of the territory names of that
#'   class, e.g. \emph{nation = "Estonia"}.
#' @param subset [\code{character(1)}]\cr optional argument to specify which
#'   subset the file contains. This could be a subset of territorial units (e.g.
#'   only one municipality) or of a target variable.
#' @param dSeries [\code{character(1)}]\cr the dataseries of the areal data (see
#'   \code{\link{regDataseries}}).
#' @param gSeries [\code{character(1)}]\cr optionally, the dataseries of the
#'   geometries, if the geometry dataseries deviates from the dataseries of the
#'   areal data (see \code{\link{regDataseries}}).
#' @param label [\code{integerish(1)}]\cr the label in the onology this geometry
#'   should correspond to.
#' @param begin [\code{integerish(1)}]\cr the date from which on the data are
#'   valid.
#' @param end [\code{integerish(1)}]\cr the date until which the data are valid.
#' @param schema [\code{list(1)}]\cr the schema description of the table to read
#'   in (must have been placed in the global environment before calling it
#'   here).
#' @param archive [\code{character(1)}]\cr the original file from which the
#'   boundaries emerge.
#' @param archiveLink [\code{character(1)}]\cr download-link of the archive.
#' @param nextUpdate [\code{character(1)}]\cr when does the geometry dataset get
#'   updated the next time (format restricted to: YYYY-MM-DD).
#' @param updateFrequency [\code{character(1)}]\cr value describing the
#'   frequency with which the dataset is updated, according to the ISO 19115
#'   Codelist, MD_MaintenanceFrequencyCode. Possible values are: 'continual',
#'   'daily', 'weekly', 'fortnightly', 'quarterly', 'biannually', 'annually',
#'   'asNeeded', 'irregular', 'notPlanned', 'unknown', 'periodic',
#'   'semimonthly', 'biennially'.
#' @param metadataLink [\code{character(1)}]\cr if there is already metadata
#'   existing: link to the meta dataset.
#' @param metadataPath [\code{character(1)}]\cr if an existing meta dataset was
#'   downloaded along the data: the path where it is stored locally.
#' @param notes [\code{character(1)}]\cr optional notes.
#' @param update [\code{logical(1)}]\cr whether or not the file 'inv_tables.csv'
#'   should be updated.
#' @param overwrite [\code{logical(1)}]\cr whether or not the geometry to
#'   register shall overwrite a potentially already existing older version.
#' @details When processing areal data tables, carry out the following steps:
#'   \enumerate{ \item Determine the main territory (such as a nation, or any
#'   other polygon), a \code{subset} (if applicable), the ontology
#'   \code{label} and the dataseries of the areal data and of the geometry, and
#'   provide them as arguments to this function. \item Provide a \code{begin}
#'   and \code{end} date for the areal data. \item Run the function. \item
#'   (Re)Save the table with the following properties: \itemize{\item Format:
#'   csv \item Encoding: UTF-8 \item File name: What is provided as message by
#'   this function \item make sure that the file is not modified or reshaped.
#'   This will happen during data normalisation via the schema description,
#'   which expects the original table.} \item Confirm that you have saved the
#'   file.}
#'
#'   Every areal data dataseries (\code{dSeries}) may come as a slight
#'   permutation of a particular table arrangement. The function
#'   \code{\link{normTable}} expects internally a schema description (a list
#'   that describes the position of the data components) for each data table,
#'   which is saved as \code{paste0("meta_", dSeries, TAB_NUMBER)}. See package
#'   \code{tabshiftr}.
#' @return Returns a tibble of the entry that is appended to 'inv_tables.csv' in
#'   case \code{update = TRUE}.
#' @family register functions
#' @examples
#' if(dev.interactive()){
#'   # build the example database
#'   makeExampleDB(until = "regGeometry", path = tempdir())
#'
#'   # the schema description for this table
#'   library(tabshiftr)
#'
#'   schema_madeUp <-
#'     setIDVar(name = "al1", columns = 1) %>%
#'     setIDVar(name = "year", columns = 2) %>%
#'     setIDVar(name = "commodities", columns = 3) %>%
#'     setObsVar(name = "harvested",
#'               factor = 1, columns = 4) %>%
#'     setObsVar(name = "production",
#'               factor = 1, columns = 5)
#'
#'   regTable(nation = "Estonia",
#'            subset = "barleyMaize",
#'            dSeries = "madeUp",
#'            gSeries = "gadm",
#'            level = 1,
#'            begin = 1990,
#'            end = 2017,
#'            schema = schema_madeUp,
#'            archive = "example_table.7z|example_table1.csv",
#'            archiveLink = "...",
#'            nextUpdate = "2019-10-01",
#'            updateFrequency = "quarterly",
#'            metadataLink = "...",
#'            metadataPath = "my/local/path",
#'            update = TRUE)
#' }
#' @importFrom readr read_csv write_rds guess_encoding
#' @importFrom rlang ensym exprs eval_tidy
#' @importFrom purrr map_chr
#' @importFrom checkmate assertDataFrame assertNames assertCharacter
#'   assertIntegerish assertSubset assertLogical testChoice assertChoice
#'   assertFileExists assertClass assertTRUE testDataFrame testNames
#' @importFrom dplyr filter distinct
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @export

regTable <- function(..., subset = NULL, dSeries = NULL, gSeries = NULL,
                     label = NULL, begin = NULL, end = NULL, schema = NULL,
                     archive = NULL, archiveLink = NULL, nextUpdate = NULL,
                     updateFrequency = NULL, metadataLink = NULL, metadataPath = NULL,
                     notes = NULL, update = FALSE, overwrite = FALSE){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))

  # get tables
  inv_tables <- read_csv(paste0(intPaths, "/inv_tables.csv"), col_types = "iiiccccDccccc")
  inv_dataseries <- read_csv(paste0(intPaths, "/inv_dataseries.csv"), col_types = "icccccc")
  inv_geometries <- read_csv(paste0(intPaths, "/inv_geometries.csv"), col_types = "iicccccDDcc")

  if(dim(inv_dataseries)[1] == 0){
    stop("'inv_dataseries.csv' does not contain any entries!")
  } else if(dim(inv_geometries)[1] == 0){
    stop("'inv_geometries.csv' does not contain any entries!")
  }

  # make new tabID
  newTID <- ifelse(length(inv_tables$tabID)==0, 1, as.integer(max(inv_tables$tabID)+1))

  # in testing mode?
  testing <- getOption(x = "adb_testing")

  # check validity of arguments
  assertNames(x = colnames(inv_tables),
              permutation.of = c("tabID", "datID", "geoID", "source_file", "schema",
                                 "orig_file", "orig_link", "download_date", "next_update",
                                 "update_frequency", "metadata_link", "metadata_path", "notes"))
  assertNames(x = colnames(inv_dataseries),
              permutation.of = c("datID", "name", "description", "homepage",
                                 "licence_link", "licence_path", "notes"))
  assertNames(x = colnames(inv_geometries),
              permutation.of = c("geoID", "datID", "source_file", "layer",
                                 "label", "orig_file", "orig_link", "download_date",
                                 "next_update", "update_frequency", "notes"))
  assertCharacter(x = subset, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = dSeries, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = gSeries, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = label, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertIntegerish(x = begin, any.missing = FALSE, len = 1, lower = 1900, null.ok = TRUE)
  assertIntegerish(x = end, any.missing = FALSE, len = 1, upper = as.integer(format(Sys.Date(), "%Y")), null.ok = TRUE)
  assertClass(x = schema, classes = "schema", null.ok = TRUE)
  assertCharacter(x = archive, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = archiveLink, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = nextUpdate, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = updateFrequency, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = metadataLink, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = metadataPath, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = notes, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertLogical(x = update, len = 1)
  assertLogical(x = overwrite, len = 1)

  broadest <- exprs(..., .named = TRUE)

  if(length(broadest) > 0){
    mainPoly <- eval_tidy(broadest[[1]])
  } else {
    mainPoly <- ""
  }

  schemaName <- as.character(substitute(schema))

  # ask for missing and required arguments
  if(!is.null(subset)){
    if(grepl(pattern = "_", x = subset)){
      stop("please give a subset that does not contain any '_' characters.")
    }
  } else {
    subset <- ""
  }

  if(is.null(dSeries)){
    message("please type in to which data series this table belongs: ")
    if(!testing){
      dSeries <- readline()
    } else {
      dSeries <- "madeUp"
    }

    if(grepl(pattern = "_", x = dSeries)){
      stop("please give a data series name that does not contain any '_' characters.")
    }

    if(!testing){
      if(!any(inv_dataseries$name %in% dSeries)){
        stop(paste0("please first create the new dataseries '", dSeries,"' via 'regDataseries()'"))
      }
    } else {
      dataSeries <- NA_integer_
    }

  } else{
    if(!any(inv_dataseries$name %in% dSeries)){
      stop(paste0("please first create the new data table dataseries '", dSeries, "' via 'regDataseries()'"))
    }
    dataSeries <- inv_dataseries$datID[inv_dataseries$name %in% dSeries]
  }

  if(is.null(gSeries)){
    message("please type in to which geometry series this table belongs: ")
    if(!testing){
      gSeries <- readline()
    } else {
      gSeries <- "gadm"
    }

    if(grepl(pattern = "_", x = gSeries)){
      stop("please give a geometry series name that does not contain any '_' characters.")
    }

    if(!testing){
      if(!any(inv_dataseries$name %in% gSeries)){
        stop(paste0("! please first create the new geometry series '", gSeries,"' via 'regDataseries()' !"))
      }
    } else {
      geomSeries <- NA_integer_
    }

  } else{
    tempDatID <- inv_dataseries$datID[inv_dataseries$name %in% gSeries]
    tempLabels <- map_chr(.x = inv_geometries$label,
                      .f = function(x){
                        str_split(tail(str_split(x, "\\|")[[1]], 1), "=")[[1]][1]
                      })
    geomSeries <- inv_geometries$geoID[inv_geometries$datID %in% tempDatID & tempLabels == label]
    if(length(geomSeries) < 1){
      stop(paste0("! please first register geometries of the series '", gSeries,"' via 'regGeometries()' !"))
    }
  }


  if(is.null(label)){
    message("please type in the ontology label of the units: ")
    if(!testing){
      label <- readline()
    } else {
      label <- 1
    }
    if(is.na(label)){
      label = NA_character_
    }
  }

  if(is.null(begin)){
    message("please type in the first year in the table: ")
    if(!testing){
      begin <- readline()
    } else {
      begin <- 1990
    }
    if(is.na(begin)){
      begin = NA_integer_
    }
  }

  if(is.null(end)){
    message("please type in the last year in the table: ")
    if(!testing){
      end <- readline()
    } else {
      end <- 2017
    }
    if(is.na(end)){
      end =  NA_integer_
    }
  }

  if(is.null(schema)){
    message("please provide the schema description for this table: ")
    if(!testing){
      schema <- readline()
    } else {
      schema <- readRDS(file = paste0(intPaths, "/meta/schemas/example_schema.rds"))
    }
    if(length(schema) < 1){
      schema = NA_character_
    }
  }


  if(is.null(archive)){
    message("please type in the archives' file name: ")
    if(!testing){
      archive <- readline()
    } else {
      archive <- "example_table.7z"
    }
    if(is.na(archive)){
      archive = NA_character_
    }
  }

  # put together file name and get confirmation that file should exist now
  fileName <- paste0(mainPoly, "_", label, "_", subset, "_", begin, "_", end, "_", dSeries, ".csv")
  filePath <- paste0(intPaths, "/adb_tables/stage2/", fileName)
  fileArchive <- str_split(archive, "\\|")[[1]]

  if(any(inv_tables$source_file %in% fileName)){
    if(overwrite){
      theSchemaName <- inv_tables$schema[inv_tables$source_file == fileName]
      newTID <- inv_tables$tabID[which(inv_tables$source_file %in% fileName)]
    } else {
      return(paste0("'", fileName, "' has already been registered."))
    }
  } else {
    theSchemaName <- paste0("schema_", newTID)
  }

  # make a schema description
  write_rds(x = schema, file = paste0(intPaths, "/meta/schemas/", theSchemaName, ".rds"))

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
    message(paste("please type in the frequency in which the table gets updated \n -> select one of: continual, daily, weekly, fortnightly, quarterly, biannually, annually, asNeeded, irregular, notPlanned, unknown, periodic, semimonthly, biennially: "))
    if(!testing){
      updateFrequency <- readline()
      while(!is.element(updateFrequency,
                        c("continual", "daily","weekly", "fortnightly",
                          "quarterly", "biannually", "annually", "asNeeded",
                          "irregular", "notPlanned", "unknown", "periodic",
                          "semimonthly", "biennially"))){
        # test missing
        message(paste(" -> input one of: continual, daily, weekly, fortnightly, quarterly, biannually, annually, asNeeded, irregular, notPlanned, unknown, periodic, semimonthly, biennially \n
                      please repeat: "))
        updateFrequency <- readline()
      }
    } else {
      updateFrequency <- "quarterly"
    }
    if(is.na(updateFrequency)){
      # this might fail, there is no NA_Date_
      # also, it should be impossible to land here
      updateFrequency = as.Date(NA)
    }
  }

  if(is.null(nextUpdate)){
    if(updateFrequency %in% c("asNeeded", "notPlanned", "unknown")){
      nextUpdate <- as.Date(NA)
    } else {
      message("please type in when the table gets its next update (YYYY-MM-DD): ")
      if(!testing){
        nextUpdate <- as.Date(readline(), "%Y-%m-%d")
      } else {
        nextUpdate <- as.Date("2019-10-01", "%Y-%m-%d")
      }
      if(is.na(nextUpdate)){
        # this might fail, there is no NA_Date_
        nextUpdate = as.Date(NA)
      }
    }
  }

  if(is.null(metadataLink)){
    message(paste("if there is already metadata available:\n -> type in the weblink to the metadataset: "))
    if(!testing){
      metadataLink <- readline()
    } else {
      metadataLink <- "https://ec.europa.eu/eurostat/de/table1/metadata"
    }
    if(is.na(metadataLink)){
      metadataLink = NA_character_
    }
  }

  if(is.null(metadataPath)){
    message(paste("if there was an existing metadataset downloaded:\n -> type in the local path to the metadataset: "))
    if(!testing){
      metadataPath <- readline()
    } else {
      metadataPath <- "C:/Users/arue/Projects/GeoKur/Luckinet/census/table1_meta.txt"
    }
    if(is.na(metadataLink)){
      metadataPath = NA_character_
    }
  }

  if(is.null(notes)){
    notes = NA_character_
  }

  if(update){

    # test whether the archive file is available
    if(!testFileExists(x = paste0(intPaths, "/adb_tables/stage1/", fileArchive[1]))){
      message(paste0("... please store the archive '", fileArchive[[1]], "' in './adb_tables/stage1'"))
      if(!testing){
        done <- readline(" -> press any key when done: ")
      }

      # make sure that the file is really there
      assertFileExists(x = paste0(intPaths, "/adb_tables/stage1/", fileArchive[1]))

      # ... and if it is compressed, whether also the file therein is given that contains the data
      if(testCompressed(x = fileArchive[1]) & length(fileArchive) < 2){
        message(paste0("please give the name of the file in ", fileArchive[1]," that contains the table: "))
        if(!testing){
          theArchiveFile <- readline()
        } else {
          theArchiveFile <- "example_table.csv"
        }
        archive <- paste0(archive, "|", theArchiveFile)
      }
    }

    # test that the file is available
    if(!testFileExists(x = filePath, extension = "csv")){
      processedPath <- paste0(intPaths, "/adb_tables/stage2/processed/", fileName)
      if(testFileExists(x = processedPath, extension = "csv")){
        temp <- inv_tables[which(inv_tables$source_file %in% fileName), ]
        message(paste0("! the table '", fileName, "' has already been normalised !"))
        return(temp)
      }

      message(paste0("... please store the table as '", fileName, "' with utf-8 encoding in './adb_tables/stage2'"))
      if(!testing){
        done <- readline(" -> press any key when done: ")
      }
      # make sure that the file is really there
      assertFileExists(x = filePath, extension = "csv")
    }

    # put together new census database entry
    doc <- tibble(tabID = newTID,
                  geoID = geomSeries,
                  datID = dataSeries,
                  source_file = fileName,
                  schema = theSchemaName,
                  orig_file = archive,
                  orig_link = archiveLink,
                  download_date = Sys.Date(),
                  next_update = nextUpdate,
                  update_frequency = updateFrequency,
                  metadata_link = metadataLink,
                  metadata_path = metadataPath,
                  notes = notes)

    if(!any(inv_tables$source_file %in% fileName) | overwrite){
      # in case the user wants to update, attach the new information to the table inv_sourceData.csv
      updateTable(index = doc, name = "inv_tables", matchCols = c("source_file"))
    }

    return(doc)
  } else {

    stage1Exists <- testFileExists(x = paste0(intPaths, "/adb_tables/stage1/", fileArchive[1]), "r")
    stage2Exists <- testFileExists(x = filePath, "r", extension = "csv")
    if(stage2Exists){
      # thisTable <- as_tibble(read.csv(file = filePath, header = FALSE, as.is = TRUE, na.strings = schema@format$na, encoding = "UTF-8"))
      thisTable <- read_csv(file = filePath, col_names = FALSE, col_types = cols(.default = "c"))
      temp <- tryCatch(expr = reorganise(input = thisTable, schema = schema),
                       error = function(e){
                         return("There was an error message")
                       },
                       warning = function(w){
                         return("There was a warning message")
                       })
      isTable <- testDataFrame(x = temp)
      correctNames <- testNames(x = names(temp), must.include = names(schema@variables))
      if(isTable & correctNames){
        schema_ok <- "schema ok"
      } else {
        schema_ok <- temp

      }
    } else {
      schema_ok <- "not checked"
    }

    diag <- tibble(stage1_name = fileArchive[1],
                   stage2_name = fileName,
                   schema_name = schemaName,
                   stage1_ok = stage1Exists,
                   stage2_ok = stage2Exists,
                   schema = schema_ok)

    updateTable(index = diag, name = "diag_tables", matchCols = c("stage2_name"), backup = FALSE)

    return(diag)
  }


}
