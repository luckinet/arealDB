#' Register census tables
#'
#' Harmonise and integrate census tables in a standardised format
#' @param input [\code{character(1)}]\cr path of the file to register.
#' @param algorithm [\code{character(1)}]\cr the algorithm to use for the
#'   specific format of the dataseries provider.
#' @param ... [\code{}]\cr ...
#' @param keepOrig [\code{logical(1)}]\cr whether or not to keep not only the
#'   IDs btu also the original terms for which IDs have been derived.
#' @param update [\code{logical(1)}]\cr whether or not the physical files should
#'   be updated (\code{TRUE}) or the function should merely return the new
#'   object (\code{FALSE} default).
#' @importFrom checkmate assertDataFrame assertNames assertFileExists
#'   assertIntegerish assertLogical
#' @importFrom dplyr mutate select pull
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @importFrom stringr str_split
#' @importFrom tidyselect everything
#' @export

normCensus <- function(input, ..., keepOrig = TRUE, update = FALSE, verbose = TRUE){

  # get objects
  inv_census <- read_csv(paste0(getOption(x = "cT_path"), "/inv_census.csv"), col_types = "iiicDcc")
  vars <- exprs(..., .named = TRUE)

  # check validity of arguments
  assertNames(x = colnames(inv_census), permutation.of = c("cenID", "datID", "geoID", "source_file", "date", "orig_file", "notes"))
  assertFileExists(x = input, access = "r")
  assertLogical(x = update, len = 1)
  assertLogical(x = verbose, len = 1)
  assertList(x = vars)

  # scrutinize file-name (the fields, which are delimited by "_" carry important information)
  pathStr <- str_split(input, "/")[[1]]
  file_name <- pathStr[length(pathStr)]
  fields <- str_split(file_name, "_")[[1]]
  algorithm = paste0("meta_", fields[4], fields[6])
  if(!exists(algorithm)){
    stop(paste0("please create the meta data object '", algorithm, "' for the file '", file_name, "'.\n  --> See '?record' for details"))
  }

  # get some variables
  cenID <- ifelse(length(inv_census$cenID) == 0, 1,
                  inv_census$cenID[grep(pattern = file_name, x = inv_census$source_file)])
  geoID <- ifelse(length(inv_census$geoID) == 0, 1,
                  inv_census$geoID[grep(pattern = file_name, x = inv_census$source_file)])
  country <- countries$nation[countries$iso_a3 == toupper(fields[1])]

  out <- read_csv(input, col_names = FALSE) %>%
    record(what = "algorithm", name = algorithm) %>%
    reorganise() %>%
    mutate(id = seq_along(years),
           cenID = cenID,
           geoID = geoID) %>%
    matchUnits(source = geoID, keepOrig = keepOrig)

  if(length(vars) != 0){
    out <- out %>%
      matchVars(source = cenID, vars, keepOrig = keepOrig)
  }

  # in case the user wants to update, update the census file
  if(update){

    theNations <- out %>%
      filter(!is.na(ahID)) %>%
      pull(al1_alt) %>%
      unique() %>%
      unifyNations()

    out <- out %>%
      select(-al1_alt)

    updateCensus(census = out, nations = theNations, file = toClean)

  } else{
    out <- out %>%
      select(-al1_alt)
  }

}