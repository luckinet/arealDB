#' Normalise data tables
#'
#' Harmonise and integrate data tables into standardised format
#' @param input [\code{character(1)}]\cr path of the file to normalise. If this
#'   is left empty, all files at stage two as subset by \code{pattern} are
#'   chosen.
#' @param pattern [\code{character(1)}]\cr an optional regular expression. Only
#'   dataset names which match the regular expression will be returned.
#' @param update [\code{logical(1)}]\cr whether or not the physical files should
#'   be updated (\code{TRUE}) or the function should merely return the new
#'   object (\code{FALSE}, default). This is helpful to check whether the
#'   metadata specification and the provided file(s) (translation and ID tables)
#'   are properly specified.
#' @param outType [\code{logical(1)}]\cr the output file-type, currently
#'   implemented options are either \emph{*.csv} (more exchangeable for a
#'   workflow based on several programs) or \emph{*.rds} (smaller and less
#'   error-prone data-format but can only be read by R efficiently).
#' @param verbose [\code{logical(1)}]\cr be verbose about translating terms
#'   (default \code{FALSE}). Furthermore, you can use
#'   \code{\link{suppressMessages}} to make this function completely silent.
#' @details To normalise data tables, this function proceeds as follows:
#'   \enumerate{ \item Read in \code{input} and extract initial metadata from
#'   the file name. \item Employ the function
#'   \code{tabshiftr::\link{reorganise}} to reshape \code{input} according to
#'   the respective schema description. \item Match the territorial units in
#'   \code{input} via . \item Harmonise territorial unit
#'   names. \item If \code{update = TRUE}, store the processed data table at
#'   stage three.}
#' @family normalisers
#' @return This function harmonises and integrates so far unprocessed data
#'   tables at stage two into stage three of the areal database. It produces for
#'   each nation in the registered data tables a comma-separated values file
#'   that includes all thematic areal data.
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
#' @importFrom stringr str_split
#' @importFrom tidyselect everything
#' @importFrom utils read.csv
#' @export

normTable <- function(input = NULL, outType = "rds", pattern = NULL,
                      update = FALSE, verbose = FALSE){

  # input <- NULL; outType = "rds"; pattern = NULL; update = TRUE; verbose = FALSE

  # set internal paths
  intPaths <- getOption(x = "adb_path")
  gazPath <- paste0(getOption(x = "gazetteer_path"))

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
  inv_geometries <- read_csv(paste0(intPaths, "/inv_geometries.csv"), col_types = "iiicccccDDcc")
  gazetteer <- load_ontology(path = gazPath) %>%
    rowwise() %>%
    mutate(level = str_split(code, "[.]", simplify = TRUE) %>% length())

  # check validity of arguments
  assertNames(x = colnames(inv_tables),
              permutation.of = c("tabID", "datID", "geoID", "source_file", "schema",
                                 "orig_file", "orig_link", "download_date", "next_update",
                                 "update_frequency", "metadata_link", "metadata_path", "notes"))
  assertNames(x = colnames(inv_dataseries),
              permutation.of = c("datID", "name", "description", "homepage",
                                 "licence_link", "licence_path", "notes"))
  assertNames(x = colnames(inv_geometries),
              permutation.of = c("geoID", "datID", "level", "source_file", "layer",
                                 "hierarchy", "orig_file", "orig_link", "download_date",
                                 "next_update", "update_frequency", "notes"))
  assertLogical(x = update, len = 1)
  assertNames(x = outType, subset.of = c("csv", "rds"))

  ret <- NULL
  for(i in seq_along(input)){

    thisInput <- input[i]

    # scrutinize file-name (the fields, which are delimited by "_" carry important information)
    pathStr <- str_split(thisInput, "/")[[1]]
    file_name <- pathStr[length(pathStr)]
    fields <- str_split(file_name, "_")[[1]]

    if(!file_name %in% inv_tables$source_file){
      message("\n--- ", i, " / ", length(input), " skipping ", rep("-", times = getOption("width")-(nchar(i)+nchar(length(input))+4+nchar(file_name))), " ", file_name, " ---")
      next
    } else {
      message("\n--- ", i, " / ", length(input), " ", rep("-", times = getOption("width")-(nchar(i)+nchar(length(input))+13+nchar(file_name))), " ", file_name, " ---")
    }

    # get some variables
    lut <- inv_tables[grep(pattern = paste0("^", file_name, "$"), x = inv_tables$source_file),]
    if(file_name %in% lut$source_file){
      geoID <- lut$geoID
      tabID <- lut$tabID
      thisSchema <- lut$schema

      dSeries <- inv_dataseries$name[inv_dataseries$datID == lut$datID]

      oldNames <- str_split(string = inv_geometries$hierarchy[inv_geometries$geoID == geoID],
                            pattern = "\\|")[[1]]
      unitCols <- unique(gazetteer$class[gazetteer$level %in% 1:length(oldNames)])
    } else{
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
    temp <- thisTable %>%
      reorganise(schema = algorithm)

    message("    harmonising territory names ...")
    temp <- match_gazetteer(table = temp, columns = unitCols, dataseries = dSeries, from_meta = FALSE)
    # re-load gazetteer (to contain also updates)
    gazetteer <- load_ontology(path = gazPath) %>%
      rowwise() %>%
      mutate(level = str_split(code, "[.]", simplify = TRUE) %>% length())

    temp <- temp %>%
      mutate(tabID = tabID,
             geoID = geoID)

    # produce output
    if(update){

      topUnits <- temp %>%
        pull(unitCols[1]) %>%
        unique()

      for(j in seq_along(topUnits)){

        tempOut <- temp %>%
          filter(.data[[unitCols[1]]] == topUnits[j]) %>%
          left_join(gazetteer %>% select(code, !!unitCols[length(unitCols)] := label_en)) %>%
          unite(col = "ahName", all_of(unitCols), sep = ".") %>%
          mutate(id = seq_along(ahName)) %>%
          select(id, ahName, ahID = code, tabID, geoID, everything()) %>%
          distinct()

        # append output to previous file
        avail <- list.files(path = paste0(intPaths, "/adb_tables/stage3/"), pattern = paste0("^", topUnits[j], "."))

        if(length(avail) == 1){

          if(outType == "csv"){
            prevData <- read_csv(file = paste0(intPaths, "/adb_tables/stage3/", topUnits[j], ".csv"),
                                 col_types = cols(id = "i", tabID = "i", geoID = "i", ahID = "c", year = "c", .default = "d"))
          } else {
            prevData <- readRDS(file = paste0(intPaths, "/adb_tables/stage3/", topUnits[j], ".rds"))
          }


          out <- tempOut %>%
            bind_rows(prevData, .) %>%
            mutate(id = seq_along(ahName))
        } else if(length(avail) > 1){
          # stop("the nation '", topUnits[j], "' exists several times in the output folder '/adb_tablse/stage3/'.")
        } else {
          out <- tempOut
        }

        # write file to 'stage3' and move to folder 'processed'
        if(outType == "csv"){
          write_csv(x = out,
                    file = paste0(intPaths, "/adb_tables/stage3/", topUnits[j], ".csv"),
                    na = "")
        } else if(outType == "rds"){
          saveRDS(object = out, file = paste0(intPaths, "/adb_tables/stage3/", topUnits[j], ".rds"))
        }
      }

      if(moveFile){
        file.copy(from = thisInput, to = paste0(intPaths, "/adb_tables/stage2/processed"))
        file.remove(thisInput)
      }


    }

    ret <- bind_rows(ret, out)

    gc()

  }

  invisible(ret)

}
