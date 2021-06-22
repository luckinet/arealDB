#' Build an example database
#'
#' This function helps setting up an example database up until a certain point.
#' @param until [\code{character(1)}]\cr The database building step in terms of
#'   the function names until which the example database shall be built, one of
#'   \code{"setPath"}, \code{"setVariables"}, \code{"regDataseries"},
#'   \code{"regGeometry"}, \code{"regTable"}, \code{"normGeometry"} or
#'   \code{"normTable"}.
#' @param path [\code{character(1)}]\cr The database gets created by default in
#'   tempdir(), but if you want it in a particular location, specify that in
#'   this argument.
#' @param verbose [\code{logical(1)}]\cr be verbose about building the example
#'   database (default \code{FALSE}).
#' @return No return value, called for the side effect of creating an example
#'   database at the specified \code{path}.
#' @examples
#' # to merely register a set of files
#' makeExampleDB(until = "regTable")
#'
#' # to build the full example database
#' makeExampleDB()
#'
#' @importFrom checkmate assertChoice assertDirectoryExists
#' @importFrom readr read_csv
#' @importFrom tabshiftr setFormat setIDVar setObsVar
#' @export

makeExampleDB <- function(until = NULL, path = NULL, verbose = FALSE){

  inPath <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)
  steps <- c("setPath", "setVariables", "regDataseries", "regGeometry", "regTable", "normGeometry", "normTable")
  if(is.null(until)){
    until <- "normTable"
  }
  assertChoice(x = until, choices = steps)

  theSteps <- steps[1:which(steps %in% until)]

  if(is.null(path)){
    path <- paste0(tempdir(), "/newDB")
  }

  if(file.exists(path)){
    unlink(path, recursive = TRUE)
  }

  # enable testing, this inserts values to readLine() calls that would otherwise
  # not be answered by the test
  oldOptions <- options()
  on.exit(options(oldOptions))
  options(adb_testing = TRUE)

  if(any(theSteps %in% "setPath")){

    setPath(root = path)
    assertDirectoryExists(x = path, access = "rw")
  }

  file.copy(from = paste0(inPath, "/example_geom.7z"),
            to = paste0(path, "/adb_geometries/stage1/example_geom.7z"))
  file.copy(from = paste0(inPath, "/example_geom1.gpkg"),
            to = paste0(path, "/adb_geometries/stage2/_1__gadm.gpkg"))
  file.copy(from = paste0(inPath, "/example_geom2.gpkg"),
            to = paste0(path, "/adb_geometries/stage2/_2__gadm.gpkg"))
  file.copy(from = paste0(inPath, "/example_geom3.gpkg"),
            to = paste0(path, "/adb_geometries/stage2/_3__gadm.gpkg"))
  file.copy(from = paste0(inPath, "/example_geom4.gpkg"),
            to = paste0(path, "/adb_geometries/stage2/_3__madeUp.gpkg"))

  file.copy(from = paste0(inPath, "/example_table.7z"),
            to = paste0(path, "/adb_tables/stage1/example_table.7z"))
  file.copy(from = paste0(inPath, "/example_table1.csv"),
            to = paste0(path, "/adb_tables/stage2/est_1_barleyMaize_1990_2017_madeUp.csv"))
  file.copy(from = paste0(inPath, "/example_table2.csv"),
            to = paste0(path, "/adb_tables/stage2/est_2_barleyMaize_1990_2017_madeUp.csv"))

  file.copy(from = paste0(inPath, "/example_schema.rds"),
            to = paste0(path, "/adb_tables/meta/schemas/example_schema.rds"))


  if(any(theSteps %in% "setVariables")){
    territories <- read_csv(file = paste0(inPath, "/id_units.csv"), col_types = "iccc")
    setVariables(input = territories, variable = "territories",
                 pid = "anID", origin = "origin", target = "names")

    comm <- read_csv(file = paste0(inPath, "/id_commodities.csv"), col_types = "iccc")
    setVariables(input = comm, variable = "commodities",
                 pid = "faoID", target = "simpleName")
  }

  if(any(theSteps %in% "regDataseries")){
    regDataseries(name = "gadm",
                  description = "Database of Global Administrative Areas",
                  homepage = "https://gadm.org/index.html",
                  licence_link = "https://gadm.org/license.html",
                  update = TRUE)

    regDataseries(name = "madeUp",
                  description = "Made-Up Concepts",
                  homepage = "https://en.wikipedia.org/wiki/String_theory",
                  licence_link = "https://creativecommons.org/share-your-work/public-domain/cc0/",
                  update = TRUE)
  }

  if(any(theSteps %in% "regGeometry")){

    regGeometry(nation = "NAME_0",
                gSeries = "gadm",
                level = 1,
                layer = "example_geom1",
                nameCol = "NAME_0",
                archive = "example_geom.7z|example_geom1.gpkg",
                archiveLink = "https://gadm.org/",
                nextUpdate = "2019-10-01",
                updateFrequency = "quarterly",
                update = TRUE)

    regGeometry(nation = "NAME_0",
                gSeries = "gadm",
                level = 2,
                layer = "example_geom2",
                nameCol = "NAME_0|NAME_1",
                archive = "example_geom.7z|example_geom2.gpkg",
                archiveLink = "https://gadm.org/",
                nextUpdate = "2019-10-01",
                updateFrequency = "quarterly",
                update = TRUE)

    regGeometry(nation = "NAME_0",
                gSeries = "gadm",
                level = 3,
                layer = "example_geom3",
                nameCol = "NAME_0|NAME_1|NAME_2",
                archive = "example_geom.7z|example_geom3.gpkg",
                archiveLink = "https://gadm.org/",
                nextUpdate = "2019-10-01",
                updateFrequency = "quarterly",
                update = TRUE)

    regGeometry(nation = "NAME_0",
                gSeries = "madeUp",
                level = 3,
                layer = "example_geom4",
                nameCol = "NAME_0|NAME_1|NAME_2",
                archive = "example_geom.7z|example_geom4.gpkg",
                archiveLink = "https://gadm.org/",
                nextUpdate = "2019-10-01",
                updateFrequency = "quarterly",
                update = TRUE)

  }

  if(any(theSteps %in% "regTable")){

    meta_madeUp_1 <- tabshiftr::schema_default %>%
      setIDVar(name = "al1", columns = 1) %>%
      setIDVar(name = "year", columns = 2) %>%
      setIDVar(name = "commodities", columns = 3) %>%
      setObsVar(name = "harvested", unit = "ha", columns = 4) %>%
      setObsVar(name = "production", unit = "t", columns = 5)

    meta_madeUp_2 <- tabshiftr::schema_default %>%
      setFormat(decimal = ".", na_values = c("", "NA")) %>%
      setIDVar(name = "al1", columns = 1) %>%
      setIDVar(name = "al2", columns = 2) %>%
      setIDVar(name = "year", columns = 3) %>%
      setIDVar(name = "commodities", columns = 4) %>%
      setObsVar(name = "harvested", unit = "ha", columns = 5) %>%
      setObsVar(name = "production", unit = "t", columns = 6)

    regTable(nation = "Estonia",
             subset = "barleyMaize",
             dSeries = "madeUp",
             gSeries = "gadm",
             level = 1,
             begin = 1990,
             end = 2017,
             schema = meta_madeUp_1,
             archive = "example_table.7z|example_table1.csv",
             archiveLink = "https://en.wikipedia.org/wiki/Data_lineage",
             nextUpdate = "2019-10-01",
             updateFrequency = "quarterly",
             metadataLink = "https://en.wikipedia.org/wiki/Metadata",
             metadataPath = "my/local/path",
             update = TRUE)

    regTable(nation = "Estonia",
             subset = "barleyMaize",
             dSeries = "madeUp",
             gSeries = "gadm",
             level = 2,
             begin = 1990,
             end = 2017,
             schema = meta_madeUp_2,
             archive = "example_table.7z|example_table2.csv",
             archiveLink = "https://en.wikipedia.org/wiki/Data_lineage",
             nextUpdate = "2019-10-01",
             updateFrequency = "quarterly",
             metadataLink = "https://en.wikipedia.org/wiki/Metadata",
             metadataPath = "my/local/path",
             update = TRUE)

  }

  if(any(theSteps %in% "normGeometry")){
    normGeometry(nation = "Estonia",
                 update = TRUE, verbose = verbose)
  }

  if(any(theSteps %in% "normTable")){
    normTable(faoID = list(commodities = "target"),
              update = TRUE, verbose = verbose)
  }

}