#' Register census tables
#'
#' Register census tables in a standardised format
#' @param input [\code{character(1)}]\cr path of the file to register.
#' @param algorithm [\code{character(1)}]\cr the algorithm to use for the
#'   specific format of the dataseries provider.
#' @param makeIDs [\code{logical(1)}]\cr whether or not to derive IDs instead of
#'   chraracter strings describing adminstrative units and commodities.
#' @param update [\code{logical(1)}]\cr whether or not the physical files should
#'   be updated (\code{TRUE}) or the function should merely return the new
#'   object (\code{FALSE} default).
#' @importFrom checkmate assertDataFrame assertNames assertFileExists
#'   assertIntegerish assertLogical
#' @importFrom dplyr mutate select pull
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @importFrom rectr register rectangularise
#' @importFrom stringr str_split
#' @export

normCensus <- function(input, algorithm = NULL, makeIDs = TRUE, update = FALSE){

  # get objects
  id_census <- read_csv(paste0(getOption(x = "dmt_path"), "/id_census.csv"), col_types = "iiicDcc")
  theAlgo <- get(algorithm)

  # check validity of arguments
  assertNames(x = colnames(id_census), permutation.of = c("cenID", "datID", "geoID", "source_file", "date", "orig_file", "notes"))
  assertFileExists(x = input, access = "r")
  assertCharacter(x = algorithm, null.ok = TRUE)
  assertLogical(x = makeIDs, len = 1)
  assertLogical(x = update, len = 1)

  # scrutinize file-name (the fields, which are delimited by "_" carry important information)
  pathStr <- str_split(input, "/")[[1]]
  file_name <- pathStr[length(pathStr)]
  fields <- str_split(file_name, "_")[[1]]

  # get some variables
  cenID <- ifelse(length(id_census$cenID) == 0, 1,
                  id_census$cenID[grep(pattern = file_name, x = id_census$source_file)])
  geoID <- ifelse(length(id_census$geoID) == 0, 1,
                  id_census$geoID[grep(pattern = file_name, x = id_census$source_file)])
  country <- countries$nation[countries$iso_a3 == toupper(fields[1])]

  out <- read_csv(input) %>%
    register(what = "algorithm", name = algorithm) %>%
    rectangularise() %>%
    mutate(id = seq_along(years),
           cenID = cenID,
           geoID = geoID)

  if(makeIDs){
    # match administrative units and commodites with the respective tables to
    # get their IDs; match years to bring it into the correct format
    out <- out %>%
      matchAdminUnits(keepOrig = TRUE) %>%
      matchCommodities(simpleName = commodities) %>%
      matchYears() %>%
      select(id, cenID, geoID, ahID, faoID, year, `harvested area`, production, yield)
  }

  if(update){

    theNations <- out %>%
      filter(!is.na(ahID)) %>%
      pull(al1_alt) %>%
      unique() %>%
      unifyNations()

    out <- out %>%
      select(-al1_alt)

    # in case the user wants to update, update the census file
    updateCensus(census = out, nations = theNations, file = toClean)

  } else{
    out <- out %>%
      select(-al1_alt)
  }

}