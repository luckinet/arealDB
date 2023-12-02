#' Query data from the database
#'
#' @param ... description
#' @param variable description
#' @param inventory [character(.)][character] one of \code{"dataseries"},
#'   \code{"geometries"} or \code{"tables"}.
#' @param spatial description
#' @details Additional details...
#' @return description
#' @family family name
#' @examples
#' # example code
#' @importFrom checkmate
#' @export

adb_query <- function(..., variable = NULL, inventory = NULL, spatial = FALSE){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))

  # get tables
  inv_tables <- read_csv(paste0(intPaths, "/inv_tables.csv"), col_types = "iiiccccccccDccccc")
  inv_dataseries <- read_csv(paste0(intPaths, "/inv_dataseries.csv"), col_types = "icccccc")
  inv_geometries <- read_csv(paste0(intPaths, "/inv_geometries.csv"), col_types = "iicccccDDcc")

  # in testing mode?
  testing <- getOption(x = "adb_testing")

  # check validity of arguments
  assertSubset(x = inventory, choices = c("dataseries", "geometries", "tables"))
  assertLogical(x = spatial, any.missing = FALSE, len = 1)


  args <- exprs(..., .named = TRUE)


  if(!is.null(inventory)){

    if(inventory == "dataseries"){
      out <- inv_dataseries
    } else if(inventory == "geometries"){
      out <- inv_geometries
    } else if(inventory == "tables"){
      out <- inv_tables
    }

  } else {

    assertCharacter(x = variable, len = 1, null.ok = FALSE)

    table_paths <- paste0(intPaths, "/adb_tables/stage3/")
    geometry_paths <- paste0(intPaths, "/adb_geometries/stage3/")

    stage3_tables <- list.files(path = table_paths)
    stage3_geoms <- list.files(path = geometry_paths)

    out <- map(.x = seq_along(stage3_tables),
               .f = function(ix){

                 tempTab <- read_rds(file = paste0(table_paths, stage3_tables[ix])) #%>%
                   # separate(col = gazMatch, into = c("gazMatch", "gazExternal"), sep = "--") %>%
                   # separate(col = ontoMatch, into = c("ontoMatch", "ontoExternal"), sep = "--")
                 tempGeom <- st_read(dsn = paste0(geometry_paths, stage3_geoms[ix]))
                 # tempGeom <- read_rds(file = paste0(geometry_paths, stage3_geoms[ix]))


               })


  }


  return(out)

}