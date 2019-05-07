#' Register census tables
#'
#' Harmonise and integrate census tables in a standardised format
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
#' @importFrom tidyselect everything
#' @export

normCensus <- function(input, algorithm = NULL, ..., keepOrig = TRUE,
                       update = FALSE){

  # get objects
  inv_census <- read_csv(paste0(getOption(x = "dmt_path"), "/inv_census.csv"), col_types = "iiicDcc")
  theAlgo <- get(algorithm)
  vars <- exprs(..., .named = TRUE)

  return(vars)

  # check validity of arguments
  assertNames(x = colnames(inv_census), permutation.of = c("cenID", "datID", "geoID", "source_file", "date", "orig_file", "notes"))
  assertFileExists(x = input, access = "r")
  assertCharacter(x = algorithm, null.ok = TRUE)
  assertList(x = makeIDs, types = "character")
  assertLogical(x = update, len = 1)

  # scrutinize file-name (the fields, which are delimited by "_" carry important information)
  pathStr <- str_split(input, "/")[[1]]
  file_name <- pathStr[length(pathStr)]
  fields <- str_split(file_name, "_")[[1]]

  # get some variables
  cenID <- ifelse(length(inv_census$cenID) == 0, 1,
                  inv_census$cenID[grep(pattern = file_name, x = inv_census$source_file)])
  geoID <- ifelse(length(inv_census$geoID) == 0, 1,
                  inv_census$geoID[grep(pattern = file_name, x = inv_census$source_file)])
  country <- countries$nation[countries$iso_a3 == toupper(fields[1])]

  out <- read_csv(input) %>%
    register(what = "algorithm", name = algorithm) %>%
    rectangularise() %>%
    mutate(id = seq_along(years),
           cenID = cenID,
           geoID = geoID) %>%
    matchUnits(keepOrig = keepOrig)

  if(length(vars) != 0){
    out <- out %>%
      matchVars(vars, keepOrig = keepOrig)
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