#' Register a new dataseries
#'
#' This function registers a new dataseries of both, geometries or areal data
#' into the geospatial database
#' @param name [\code{character(1)}]\cr the dataseries abbreviation.
#' @param description [\code{character(1)}]\cr the "long name" or "brief
#'   description" of the dataseries.
#' @param website [\code{character(1)}]\cr a website where the dataseries or
#'   additional information can be found.
#' @param notes [\code{character(1)}]\cr optional notes.
#' @param update [\code{logical(1)}]\cr whether or not the file
#'   'inv_dataseries.csv' should be updated.
#' @return Returns the entry that is appended to 'inv_dataseries.csv' in case
#'   \code{update = TRUE}.
#' @examples
#' \dontrun{
#'
#' regDataseries(name = "gadm",
#'               description = "Database of Global Administrative Areas",
#'               website = "https://gadm.org/index.html",
#'               update = TRUE)
#'
#' regDataseries(name = "usda",
#'               description = "US Dept. of Agriculture",
#'               website = "https://www.nass.usda.gov/Quick_Stats/Lite/index.php",
#'               update = TRUE)
#' }
#' @importFrom readr read_csv
#' @importFrom checkmate assertDataFrame assertNames assertCharacter
#'   assertLogical
#' @importFrom tibble tibble
#' @export

regDataseries <- function(name = NULL, description = NULL, website = NULL,
                          notes = NULL, update = FALSE){

  # get tables
  inv_dataseries <- read_csv(paste0(getOption(x = "adb_path"), "/inv_dataseries.csv"), col_types = "icccc")

  # in testing mode?
  testing <- getOption(x = "adb_testing")

  # check validity of arguments
  assertDataFrame(x = inv_dataseries)
  assertNames(x = colnames(inv_dataseries), permutation.of = c("datID", "name", "long_name", "website", "notes"))
  assertCharacter(x = name, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = description, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = website, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = notes, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertLogical(x = update, len = 1)

  # ask for missing and required arguments
  if(is.null(name)){
    message("please type in the dataseries abbreviation: ")
    if(!testing){
      theName <- readline()
    } else {
      theName <- NA
    }
    if(is.na(theName)){
      theName = NA_character_
    }
  } else{
    theName <- name
  }

  if(is.null(description)){
    message("please type in the long name or description of the series: ")
    if(!testing){
      theDescr <- readline()
    } else {
      theDescr <- NA
    }
    if(is.na(theDescr)){
      theDescr = NA_character_
    }
  } else{
    theDescr <- description
  }

  if(is.null(website)){
    message("please type in the dataseries website: ")
    if(!testing){
      theWebsite <- readline()
    } else {
      theWebsite <- NA
    }
    if(is.na(theWebsite)){
      theWebsite = NA_character_
    }
  } else{
    theWebsite <- website
  }

  if(is.null(notes)){
    notes = NA_character_
  }

  # construct new documentation
  newDID <- ifelse(length(inv_dataseries$datID)==0, 1, as.integer(max(inv_dataseries$datID)+1))
  temp <- tibble(datID = as.integer(newDID),
                 name = theName,
                 long_name = theDescr,
                 website = theWebsite,
                 notes = notes)
  if(update){
    # in case the user wants to update, attach the new information to the table inv_dataseries.csv
    updateTable(index = temp, name = "inv_dataseries")
  }

  return(temp)
}