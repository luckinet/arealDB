#' Register a new dataseries
#'
#' This function registers a new dataseries of both, geometries or areal data
#' into the geospatial database.
#' @param name [\code{character(1)}]\cr the dataseries abbreviation.
#' @param description [\code{character(1)}]\cr the "long name" or "brief
#'   description" of the dataseries.
#' @param homepage [\code{character(1)}]\cr the homepage of the data provider
#'   where the dataseries or additional information can be found.
#' @param licence_link [\code{character(1)}]\cr link to the licence or the
#'   webpage from which the licence was copied.
#' @param licence_path [\code{character(1)}]\cr path to the local file in which
#'   the licence text is stored.
#' @param notes [\code{character(1)}]\cr optional notes.
#' @param update [\code{logical(1)}]\cr whether or not the file
#'   'inv_dataseries.csv' should be updated.
#' @param overwrite [\code{logical(1)}]\cr whether or not the dataseries to
#'   register shall overwrite a potentially already existing older version.
#' @return Returns a tibble of the new entry that is appended to
#'   'inv_dataseries.csv' in case \code{update = TRUE}.
#' @examples
#' # start the example database
#' makeExampleDB(until = "setVariables")
#'
#' regDataseries(name = "gadm",
#'               description = "Database of Global Administrative Areas",
#'               homepage = "https://gadm.org/index.html",
#'               licence_link = "https://gadm.org/license.html",
#'               update = TRUE)
#' @importFrom readr read_csv
#' @importFrom checkmate assertDataFrame assertNames assertCharacter
#'   assertLogical
#' @importFrom tibble tibble
#' @export

regDataseries <- function(name = NULL, description = NULL, homepage = NULL,
                          licence_link = NULL, licence_path = NULL, notes = NULL,
                          update = FALSE, overwrite = FALSE){

  # get tables
  inv_dataseries <- read_csv(paste0(getOption(x = "adb_path"), "/inv_dataseries.csv"), col_types = "icccccc")

  # in testing mode?
  testing <- getOption(x = "adb_testing")

  # check validity of arguments
  assertDataFrame(x = inv_dataseries)
  assertNames(x = colnames(inv_dataseries), permutation.of = c("datID", "name", "description", "homepage", "licence_link", "licence_path", "notes"))
  assertCharacter(x = name, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = description, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = homepage, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = licence_link, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = licence_path, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = notes, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertLogical(x = update, len = 1)
  assertLogical(x = overwrite, len = 1)

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
    if(name %in% inv_dataseries$name & !overwrite){
      message("! the dataseries '", name, "' has already been registered !")
      temp <- inv_dataseries[which(inv_dataseries$name %in% name), ]
      return(temp)
    }
    theName <- name
  }

  if(is.null(description)){
    message("please type in the long name or description of the series: ")
    if(!testing){
      theDescription <- readline()
    } else {
      theDescription <- NA
    }
    if(is.na(theDescription)){
      theDescription = NA_character_
    }
  } else{
    theDescription <- description
  }

  if(is.null(homepage)){
    message("please type in the dataseries homepage: ")
    if(!testing){
      theHomepage <- readline()
    } else {
      theHomepage <- NA
    }
    if(is.na(theHomepage)){
      theHomepage = NA_character_
    }
  } else{
    theHomepage <- homepage
  }

  if(is.null(licence_link)){
    message("please type in the weblink to the dataseries licence: ")
    if(!testing){
      theLicence_link <- readline()
    } else {
      theLicence_link <- NA
    }
    if(is.na(theLicence_link)){
      theLicence_link = NA_character_
    }
  } else{
    theLicence_link <- licence_link
  }

  if(is.null(licence_path)){
    message("please type in the path to the local folder where the licence is stored: ")
    if(!testing){
      theLicence_path <- readline()
    } else {
      theLicence_path <- NA
    }
    if(is.na(theLicence_path)){
      theLicence_path = NA_character_
    }
  } else{
    theLicence_path <- licence_path
  }

  if(is.null(notes)){
    notes = NA_character_
  }

  # construct new documentation
  newDID <- ifelse(length(inv_dataseries$datID)==0, 1, as.integer(max(inv_dataseries$datID)+1))
  if(overwrite){
    if(theName %in% inv_dataseries$name){
      newDID <- inv_dataseries$datID[which(inv_dataseries$name %in% theName)]
    }
  }
  temp <- tibble(datID = as.integer(newDID),
                 name = theName,
                 description = theDescription,
                 homepage = theHomepage,
                 licence_link = theLicence_link,
                 licence_path = theLicence_path,
                 notes = notes)
  if(update){
    # in case the user wants to update, attach the new information to the table inv_dataseries.csv
    updateTable(index = temp, name = "inv_dataseries", matchCols = c("name"))
  }

  return(temp)
}