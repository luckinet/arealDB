#' Normalise data tables
#'
#' Harmonise and integrate data tables into standardised format
#' @param input [\code{character(1)}]\cr path of the file to normalise. If this
#'   is left empty, all files at stage two as subset by \code{pattern} are
#'   chosen.
#' @param pattern [\code{character(1)}]\cr an optional regular expression. Only
#'   dataset names which match the regular expression will be processed.
#' @param update [\code{logical(1)}]\cr whether or not the physical files should
#'   be updated (\code{TRUE}) or the function should merely return the new
#'   object (\code{FALSE}, default). This is helpful to check whether the
#'   metadata specification and the provided file(s) (translation and ID tables)
#'   are properly specified.
#' @param beep [\code{integerish(1)}]\cr Number specifying what sound to be
#'   played to signal the user that a point of interaction is reached by the
#'   program, see \code{\link[beepr]{beep}}.
#' @param outType [\code{logical(1)}]\cr the output file-type, currently
#'   implemented options are either \emph{*.csv} (more exchangeable for a
#'   workflow based on several programs) or \emph{*.rds} (smaller and less
#'   error-prone data-format but can only be read by R efficiently).
#' @param ontoMatch [\code{character(.)}]\cr name of the column(s) that shall be
#'   matched with an ontology (defined in \code{\link{start_arealDB}}).
#' @param verbose [\code{logical(1)}]\cr be verbose about translating terms
#'   (default \code{FALSE}). Furthermore, you can use
#'   \code{\link{suppressMessages}} to make this function completely silent.
#' @details To normalise data tables, this function proceeds as follows:
#'   \enumerate{ \item Read in \code{input} and extract initial metadata from
#'   the file name. \item Employ the function
#'   \code{tabshiftr::\link{reorganise}} to reshape \code{input} according to
#'   the respective schema description. \item The territorial names are matched
#'   with the gazetteer to harmonise new territorial names (at this step, the
#'   function might ask the user to edit the file 'matching.csv' to align new
#'   names with already harmonised names). \item Harmonise territorial unit
#'   names. \item If \code{update = TRUE}, store the processed data table at
#'   stage three.}
#' @family normalise functions
#' @return This function harmonises and integrates so far unprocessed data
#'   tables at stage two into stage three of the areal database. It produces for
#'   each main polygon (e.g. nation) in the registered data tables a file that
#'   includes all thematic areal data.
#' @examples
#' if(dev.interactive()){
#'   # build the example database
#'   makeExampleDB(until = "normGeometry", path = tempdir())
#'
#'   # normalise all available data tables ...
#'   normTable(update = TRUE)
#'
#'   # ... and check the result
#'   output <- readRDS(paste0(tempdir(), "/adb_tables/stage3/Estonia.rds"))
#' }
#' @importFrom checkmate assertNames assertFileExists assertLogical
#' @importFrom ontologics load_ontology
#' @importFrom rlang exprs :=
#' @importFrom tabshiftr reorganise
#' @importFrom dplyr mutate select pull full_join bind_rows
#' @importFrom magrittr %>%
#' @importFrom readr read_csv cols
#' @importFrom stringr str_split str_detect
#' @importFrom tidyselect everything
#' @importFrom utils read.csv
#' @export

normTable <- function(input = NULL, pattern = NULL, ontoMatch = NULL,
                      outType = "rds", beep = NULL, update = FALSE,
                      verbose = FALSE){

  # set internal paths
  intPaths <- getOption(x = "adb_path")
  gazPath <- getOption(x = "gazetteer_path")

  # get territorial context
  topClass <- paste0(getOption(x = "gazetteer_top"))
  topUnits <- get_concept(class = topClass, ontology = gazPath) %>%
    arrange(label)

  if(is.null(input)){
    input <- list.files(path = paste0(intPaths, "/adb_tables/stage2"), full.names = TRUE, pattern = pattern)
  } else {
    assertFileExists(x = input, access = "r")
  }

  # set internal objects
  moveFile <- TRUE

  # get tables
  inv_tables <- read_csv(paste0(intPaths, "/inv_tables.csv"), col_types = "iiiccccDccccc")
  inv_dataseries <- read_csv(paste0(intPaths, "/inv_dataseries.csv"), col_types = "icccccc")
  inv_geometries <- read_csv(paste0(intPaths, "/inv_geometries.csv"), col_types = "iicccccDDcc")

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
  assertLogical(x = update, len = 1)
  assertCharacter(x = ontoMatch, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  assertNames(x = outType, subset.of = c("csv", "rds"))

  ret <- NULL
  for(i in seq_along(input)){

    thisInput <- input[i]

    # scrutinize file-name (the fields, which are delimited by "_" carry important information)
    pathStr <- str_split(thisInput, "/")[[1]]
    file_name <- pathStr[length(pathStr)]
    fields <- str_split(file_name, "_")[[1]]

    if(!file_name %in% inv_tables$source_file){
      message("\n--- ", i, " / ", length(input), " skipping ", rep("-", times = getOption("width")-(nchar(i)+nchar(length(input))+21+nchar(file_name))), " ", file_name, " ---")
      next
    } else {
      message("\n--- ", i, " / ", length(input), " ", rep("-", times = getOption("width")-(nchar(i)+nchar(length(input))+13+nchar(file_name))), " ", file_name, " ---")
    }

    # get some variables
    lut <- inv_tables[grep(pattern = paste0("^", file_name, "$"), x = inv_tables$source_file),]
    if(file_name %in% lut$source_file){
      geoID <- lut$geoID
      tabID <- lut$tabID
      datID <- lut$datID
      thisSchema <- lut$schema

      gSeries <- inv_dataseries$name[inv_dataseries$datID == geoID]
      dSeries <- inv_dataseries$name[inv_dataseries$datID == datID]
    } else {
      stop(paste0("  ! the file '", file_name, "' has not been registered yet."))
    }

    algorithm = readRDS(file = paste0(intPaths, "/meta/schemas/", thisSchema, ".rds"))
    if(!exists(x = "algorithm")){
      stop(paste0("please create the schema desciption '", algorithm, "' for the file '", file_name, "'.\n  --> See '?meta_default' for details"))
    }

    # reorganise data
    message("\n--> reading new data table ...")
    thisTable <- read.csv(file = thisInput, header = FALSE, strip.white = TRUE, as.is = TRUE, na.strings = algorithm@format$na, encoding = "UTF-8") %>%
      as_tibble()

    message("    reorganising table with '", thisSchema, "' ...")
    thisTable <- thisTable %>%
      reorganise(schema = algorithm)

    message("    harmonizing territory names ...")
    targetCols <- get_class(ontology = gazPath) %>%
      pull(label)
    targetCols <- targetCols[targetCols %in% colnames(thisTable)]

    if(targetCols[1] != topClass){
      theUnits <- str_split(string = lut$source_file, pattern = "_")[[1]][1]
      assertSubset(x = theUnits, choices = topUnits$label)
    } else {
      theUnits <- NULL
    }

    if(!topClass %in% targetCols){
      thisTable <- thisTable %>%
        add_column(tibble(!!topClass := theUnits), .before = targetCols[1])
      targetCols <- c(topClass, targetCols)
    }

    thatTable <- matchOntology(table = thisTable,
                               columns = targetCols,
                               dataseries = dSeries,
                               ontology = gazPath,
                               beep = beep) %>%
      unite(col = "gazMatch", match, external, sep = "--", na.rm = TRUE) %>%
      rename(gazID = id) %>%
      select(-has_broader, -class, -description) %>%
      mutate(gazID = str_replace_all(string = gazID, pattern = "[.]", replacement = "-"))

    if(!is.null(ontoMatch)){
      message("    harmonizing thematic concepts ...")
      assertNames(x = ontoMatch, subset.of = names(thatTable))
      ontoPath <- getOption(x = "ontology_path")[[ontoMatch]]
      thatTable <- matchOntology(table = thatTable,
                                 columns = ontoMatch,
                                 dataseries = dSeries,
                                 ontology = ontoPath,
                                 beep = beep) %>%
        rename(ontoID = id, ontoName = all_of(ontoMatch)) %>%
        unite(col = "ontoMatch", match, external, sep = "--", na.rm = TRUE) %>%
        select(ontoName, ontoID, ontoMatch, everything()) %>%
        select(-has_broader, -class, -description) %>%
        filter(!is.na(ontoName))
    }

    thatTable <- thatTable %>%
      mutate(gazID = str_replace_all(string = gazID, pattern = "-", replacement = "."),
             tabID = tabID,
             geoID = geoID)

    # produce output
    if(update){

      if(is.null(theUnits)){
        theUnits <- unique(eval(expr = parse(text = targetCols[1]), envir = thatTable)) %>%
          na.omit() %>%
          as.character()
      }

      for(j in seq_along(theUnits)){

        if(length(theUnits) != 1){
          tempOut <- thatTable %>%
            filter(.data[[targetCols[1]]] == theUnits[j])
        } else {
          tempOut <- thatTable
        }
        tempOut <- tempOut %>%
          unite(col = "gazName", all_of(targetCols), sep = ".") %>%
          select(tabID, geoID, gazID, gazName, gazMatch, everything()) %>%
          distinct()

        # append output to previous file
        avail <- list.files(path = paste0(intPaths, "/adb_tables/stage3/"), pattern = paste0("^", theUnits[j], ".", outType))

        if(length(avail) == 1){

          if(outType == "csv"){
            prevData <- read_csv(file = paste0(intPaths, "/adb_tables/stage3/", theUnits[j], ".csv"),
                                 col_types = cols(tabID = "i", geoID = "i", gazID = "c", gazName = "c", gazMatch = "c", year = "c"))
          } else {
            prevData <- readRDS(file = paste0(intPaths, "/adb_tables/stage3/", theUnits[j], ".rds"))
          }

          out <- tempOut %>%
            bind_rows(prevData, .) %>%
            distinct()

          # here distinct rows only?

        } else if(length(avail) > 1){
          stop("the nation '", theUnits[j], "' exists several times in the output folder '/adb_tablse/stage3/'.")
        } else {
          out <- tempOut
        }

        # write file to 'stage3' and move to folder 'processed'
        if(outType == "csv"){
          write_csv(x = out,
                    file = paste0(intPaths, "/adb_tables/stage3/", theUnits[j], ".csv"),
                    na = "")
        } else if(outType == "rds"){
          saveRDS(object = out, file = paste0(intPaths, "/adb_tables/stage3/", theUnits[j], ".rds"))
        }
        ret <- bind_rows(ret, out)
      }

      if(moveFile){
        file.copy(from = thisInput, to = paste0(intPaths, "/adb_tables/stage2/processed"))
        file.remove(thisInput)
      }
    } else {
      ret <- thatTable
    }

    gc()

  }

  invisible(ret)

}
