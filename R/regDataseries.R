#' Register a new geometry entry
#'
#' @param name [\code{character(1)}]\cr the dataseries abbreviation.
#' @param description [\code{character(1)}]\cr the "long name" or "brief
#'   description" of the dataseries.
#' @param address [\code{character(1)}]\cr an optional address of the dataseries
#'   provider.
#' @param website [\code{character(1)}]\cr the website, where the dataseries was
#'   found or where additional information can be found.
#' @param notes [\code{character(1)}]\cr optional notes.
#' @param update [\code{logical(1)}]\cr whether or not the file
#'   'id_dataseries.csv' should be updated.
#' @examples
#' \dontrun{
#'
#' setPath(root = "/home/se87kuhe/Nextcloud/LUCKINet/data/")
#'
#' regDataseries(name = "maia",
#'               description = "Ministerio de Agroindustria",
#'               address = "Buenos Aires, Argentina",
#'               website = "http://datosestimaciones.magyp.gob.ar/",
#'               update = FALSE)
#' }
#' @importFrom checkmate assertCharacter
#' @importFrom tibble tibble
#' @export

regDataseries <- function(name = NULL, description = NULL, address = NULL,
                          website = NULL, notes = NULL, update = FALSE){

  # get tables
  id_dataseries <- read_csv(paste0(getOption(x = "dmt_path"), "/id_dataseries.csv"), col_types = "iccccc")

  # check validity of arguments
  assertDataFrame(x = id_dataseries, ncols = 6)
  assertNames(x = colnames(id_dataseries), permutation.of = c("datID", "name", "long_name", "address", "website", "notes"))
  assertCharacter(x = name, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = description, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = address, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = website, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = notes, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertLogical(x = update, len = 1)

  # ask for missing and required arguments
  if(is.null(name)){
    theName <- readline("please give the dataseries abbreviation: ")
    if(theName == "NA"){
      theName = NA_character_
    }
  } else{
    theName <- name
  }

  if(is.null(description)){
    theDescr <- readline("please give the long name or description of the series: ")
    if(theDescr == "NA"){
      theDescr = NA_character_
    }
  } else{
    theDescr <- description
  }

  if(is.null(website)){
    theWebsite <- readline("please give the dataseries website: ")
    if(theWebsite == "NA"){
      theWebsite = NA_character_
    }
  } else{
    theWebsite <- website
  }

  if(is.null(address)){
    address = NA_character_
  }

  if(is.null(notes)){
    notes = NA_character_
  }

  # construct new documentation
  newDID <- ifelse(length(id_dataseries$datID)==0, 1, as.integer(max(id_dataseries$datID)+1))
  temp <- tibble(datID = newDID,
                 name = theName,
                 long_name = theDescr,
                 address = address,
                 website = theWebsite,
                 notes = notes)
  if(update){
    # in case the user wants to update, attach the new information to the table id_dataseries.csv
    updateIndex(index = temp, name = "id_dataseries")
  }

  return(temp)

}