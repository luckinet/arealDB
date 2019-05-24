#' Normlise census tables or geometries
#'
#' @param what [\code{character(1)}]\cr the data to normalise, either
#'   \code{"census"} or \code{"geometries"}.
#' @param ... [\code{}]\cr for \code{what = "geometries"} chose a subset of
#'   administrative units by calling \code{nation}, \code{un_member},
#'   \code{continent}, \code{region} or \code{subregion}.\cr for \code{what =
#'   "census"} chose
#' @param update [\code{logical(1)}]\cr whether or not the physical files should
#'   be updated (\code{TRUE}) or the function should merely return the new
#'   object (\code{FALSE} default).
#' @param verbose [\code{logical(1)}]\cr be verbose about what is happening
#'   (default \code{TRUE}).
#' @importFrom stringr str_split
#' @importFrom checkmate assertChoice testDirectoryExists testFileExists
#' @importFrom readr read_csv
#' @export

normalise <- function(what, ..., keepOrig = TRUE, update = FALSE, verbose = TRUE){

  assertChoice(x = what, choices = c("census", "geometries"))

  recent_jobs <- list.files(path = paste0(getOption("cT_path"), "/cT_", what,"/stage2"))
  if(what == "geometries"){
    test_id <- read_csv(paste0(getOption("cT_path"), "/inv_geometries.csv"), col_types = "iiiccccDcc")
  } else {
    test_id <- read_csv(paste0(getOption("cT_path"), "/inv_census.csv"), col_types = "iiicDcc")
  }
  id_dataseries <- read_csv(paste0(getOption(x = "cT_path"), "/inv_dataseries.csv"), col_types = "icccc")

  for(i in seq_along(recent_jobs)){

    if(recent_jobs[i] %in% test_id$source_file){

      file <- paste0(getOption("cT_path"), "/cT_", what,"/stage2/", recent_jobs[i])

      if(what == "geometries"){

        normGeometry(input = file,
                     update = TRUE,
                     verbose = verbose,
                     ...)

      } else {

        normCensus(input = file,
                   # faoID = list(commodities = "simpleName"),
                   ...,
                   keepOrig = keepOrig,
                   update = TRUE,
                   verbose = verbose)

      }
    }




  }


}
