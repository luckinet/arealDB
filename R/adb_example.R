#' Build an example areal database
#'
#' This function helps setting up an example database up until a certain step.
#' @param path [`character(1)`][character]\cr The database gets created by
#'   default in tempdir(), but if you want it in a particular location, specify
#'   that in this argument.
#' @param until [`character(1)`][character]\cr The database building step in
#'   terms of the function names until which the example database shall be
#'   built, one of \code{"adb_init"}, \code{"regDataseries"},
#'   \code{"regVocabulary"}, \code{"regGeometry"}, \code{"regTable"},
#'   \code{"normVocabulary"}, \code{"normGeometry"} or \code{"normTable"}.
#' @param verbose [`logical(1)`][logical]\cr be verbose about building the
#'   example database (default \code{FALSE}).
#' @details Setting up a database with an R-based tool can appear to be
#'   cumbersome and too complex and thus intimidating. By creating an example
#'   database, this functions allows interested users to learn step by step how
#'   to build a database of areal data. Moreover, all functions in this package
#'   contain verbose information and ask for information that would be missing
#'   or lead to an inconsistent database, before a failure renders hours of work
#'   useless.
#' @return No return value, called for the side effect of creating an example
#'   database at the specified \code{path}.
#' @examples
#' if(dev.interactive()){
#' # to build the full example database
#' adb_example(path = paste0(tempdir(), "/newDB"))
#'
#' # to build the example database until a certain step
#' adb_example(path = paste0(tempdir(), "/newDB"), until = "regDataseries")
#'
#' }
#' @importFrom checkmate assertChoice testDirectoryExists
#' @importFrom readr read_csv cols
#' @importFrom arrow write_parquet
#' @importFrom dplyr left_join mutate select
#' @importFrom utils read.csv
#' @importFrom tabshiftr setFormat setIDVar setObsVar
#' @export

adb_example <- function(path = NULL, until = NULL, verbose = FALSE){

  # library(arealDB); library(checkmate); library(tabshiftr); library(tibble); library(arrow); library(stringr); library(dplyr); library(readr); path = paste0(tempdir(), "/newDB"); until = "regGeometry"; verbose = FALSE;

  # set internal paths
  inPath <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)
  steps <- c("adb_init", "regDataseries", "regVocabulary", "normVocabulary", "regGeometry", "regTable", "normGeometry", "normTable")
  if (is.null(until)) {
    until <- "normTable"
  }
  assertChoice(x = until, choices = steps)

  if(testDirectoryExists(path)){
    unlink(path, recursive = TRUE)
  }

  theSteps <- steps[1:which(steps %in% until)]

  # enable testing, this inserts values to readLine() calls that would otherwise
  # not be answered by the test
  oldTesting <- .adb_state$testing
  on.exit(.adb_state$testing <- oldTesting)
  .adb_state$testing <- TRUE

  # 1. initialize database ----
  dir.create(file.path(path), showWarnings = FALSE)
  if (any(theSteps %in% "adb_init")) {
    adb_init(root = path,
             version = "0.0.1", licence = "https://creativecommons.org/licenses/by-sa/4.0/",
             author = "Jane Doe",
             level = c("ADM0", "ADM1", "ADM2"))
  }

  # 2. copy files ----
  ## vocabularies ----
  ### stage 1
  dir.create(file.path(path, "/vocabularies/stage1/unsd/"), recursive = TRUE)
  file.copy(from = paste0(inPath, "/gazetteer/terms.csv"),
            to = paste0(path, "/vocabularies/stage1/unsd/terms.csv"))

  dir.create(file.path(path, "/vocabularies/stage1/icc/"), recursive = TRUE)
  file.copy(from = paste0(inPath, "/ontology/base_ontology.csv"),
            to = paste0(path, "/vocabularies/stage1/icc/base_ontology.csv"))

  ### stage 2
  file.copy(from = paste0(inPath, "/gazetteer/terms.csv"),
            to = paste0(path, "/vocabularies/stage2/gazetteer__unsd.csv"))
  file.copy(from = paste0(inPath, "/ontology/base_ontology.csv"),
            to = paste0(path, "/vocabularies/stage2/commodity__icc.csv"))

  ### mappings
  file.copy(from = paste0(inPath, "/gazetteer/mappings_gadm.csv"),
            to = paste0(path, "/vocabularies/mappings/gazetteer_gadm.csv"))
  file.copy(from = paste0(inPath, "/gazetteer/mappings_madeUp.csv"),
            to = paste0(path, "/vocabularies/mappings/gazetteer_madeUp.csv"))


  ## tables ----
  ### stage 1
  dir.create(file.path(path, "/tables/stage1/madeUp/"))
  file.copy(from = paste0(inPath, "/example_table.7z"),
            to = paste0(path, "/tables/stage1/madeUp/example_table.7z"))

  ### stage 2
  file.copy(from = paste0(inPath, "/example_table1.csv"),
            to = paste0(path, "/tables/stage2/_ADM0_barleyMaize_1990_2017_madeUp.csv"))
  file.copy(from = paste0(inPath, "/example_table2.csv"),
            to = paste0(path, "/tables/stage2/aNation_ADM1_barleyMaize_1990_2017_madeUp.csv"))

  ### schema
  file.copy(from = paste0(inPath, "/example_schema.rds"),
            to = paste0(path, "/tables/schemas/example_schema.rds"))


  ## geometries ----
  ### stage 1
  file.copy(from = paste0(inPath, "/example_geom.7z"),
            to = paste0(path, "/geometries/stage1/example_geom.7z"))

  ### stage 2
  file.copy(from = paste0(inPath, "/example_geom1.gpkg"),
            to = paste0(path, "/geometries/stage2/_ADM0__gadm.gpkg"))
  file.copy(from = paste0(inPath, "/example_geom2.gpkg"),
            to = paste0(path, "/geometries/stage2/_ADM1__gadm.gpkg"))
  file.copy(from = paste0(inPath, "/example_geom3.gpkg"),
            to = paste0(path, "/geometries/stage2/_ADM2__gadm.gpkg"))
  file.copy(from = paste0(inPath, "/example_geom4.gpkg"),
            to = paste0(path, "/geometries/stage2/_ADM2__madeUp.gpkg"))

  # 3. register dataseries ----
  if (any(theSteps %in% "regDataseries")) {
    regDataseries(name = "gadm",
                  description = "Database of Global Administrative Areas",
                  homepage = "https://gadm.org/index.html",
                  version = "1.0",
                  licence_link = "https://gadm.org/license.html")

    regDataseries(name = "madeUp",
                  description = "Made-Up Concepts",
                  homepage = "https://en.wikipedia.org/wiki/String_theory",
                  version = "1.0",
                  licence_link = "https://creativecommons.org/share-your-work/public-domain/cc0/")

    regDataseries(name = "icc",
                  description = "Indicative Crop Classification",
                  homepage = "https://www.fao.org/economic/the-statistics-division-ess/methodology/methodology-systems/indicative-crop-classification-icc/",
                  version = "1.1",
                  licence_link = "https://www.fao.org/contact-us/terms")

    regDataseries(name = "unsd",
                  description = "United Nations geoscheme",
                  homepage = "https://unstats.un.org/unsd/methodology/m49/",
                  version = "2024",
                  licence_link = "https://www.un.org/en/about-us/copyright")
  }

  # 4. register vocabularies ----
  if(any(theSteps %in% "regVocabulary")){

    schema_voc <-
      setIDVar(name = "cid", columns = 1) |>
      setIDVar(name = "class", columns = 3) |>
      setObsVar(name = "label", columns = 2, type = "c") |>
      setObsVar(name = "parent_label", columns = 4, type = "c")

    regVocabulary(name = "commodity",
                  vSeries = "icc",
                  description = "Backbone ontology of agricultural commodities.",
                  schema = schema_voc,
                  archive = "base_ontology.csv",
                  archiveLink = "https://www.fao.org/economic/the-statistics-division-ess/methodology/methodology-systems/indicative-crop-classification-icc/",
                  downloadDate = "2024-01-01",
                  version = "1.1",
                  licence_link = "https://www.fao.org/contact-us/terms")

    regVocabulary(name = "gazetteer",
                  vSeries = "unsd",
                  description = "Backbone gazetteer of territorial concepts.",
                  schema = schema_voc,
                  archive = "terms.csv",
                  archiveLink = "https://unstats.un.org/unsd/methodology/m49/",
                  downloadDate = "2024-01-01",
                  version = "2024",
                  licence_link = "https://www.un.org/en/about-us/copyright")
  }

  # 5. normalise vocabularies
  if(any(theSteps %in% "normVocabulary")){
    normVocabulary(verbose = verbose)
  }

  # 6. register geometries ----
  if(any(theSteps %in% "regGeometry")){

    regGeometry(gSeries = "gadm",
                match = list(ADM0 = "NAME_0"),
                layer = "example_geom1",
                archive = "example_geom.7z|example_geom1.gpkg",
                archiveLink = "https://gadm.org/",
                downloadDate = as.Date("2019-10-01"),
                updateFrequency = "quarterly")

    regGeometry(gSeries = "gadm",
                match = list(ADM0 = "NAME_0", ADM1 = "NAME_1"),
                layer = "example_geom2",
                archive = "example_geom.7z|example_geom2.gpkg",
                archiveLink = "https://gadm.org/",
                downloadDate = as.Date("2019-10-01"),
                updateFrequency = "quarterly")

    regGeometry(gSeries = "gadm",
                match = list(ADM0 = "NAME_0", ADM1 = "NAME_1", ADM2 = "NAME_2"),
                layer = "example_geom3",
                archive = "example_geom.7z|example_geom3.gpkg",
                archiveLink = "https://gadm.org/",
                downloadDate = as.Date("2019-10-01"),
                updateFrequency = "quarterly")

    regGeometry(gSeries = "madeUp",
                match = list(ADM0 = "NAME_0", ADM1 = "NAME_1", ADM2 = "NAME_2"),
                layer = "example_geom4",
                archive = "example_geom.7z|example_geom4.gpkg",
                archiveLink = "https://gadm.org/",
                downloadDate = as.Date("2019-10-01"),
                updateFrequency = "quarterly")

  }

  # 7. register tables ----
  if(any(theSteps %in% "regTable")){

    meta_madeUp_1 <-
      setIDVar(name = "ADM0", columns = 1)  |>
      setIDVar(name = "year", columns = 2) |>
      setIDVar(name = "commodity", columns = 3) |>
      setObsVar(name = "harvested", columns = 4) |>
      setObsVar(name = "production", columns = 5)

    meta_madeUp_2 <-
      setFormat(decimal = ".", na_values = c("", "NA")) |>
      setIDVar(name = "ADM0", columns = 1) |>
      setIDVar(name = "ADM1", columns = 2) |>
      setIDVar(name = "year", columns = 3) |>
      setIDVar(name = "commodity", columns = 4) |>
      setObsVar(name = "harvested", columns = 5) |>
      setObsVar(name = "production", columns = 6)

    regTable(subset = "barleyMaize",
             dSeries = "madeUp",
             gSeries = "gadm",
             match = "ADM0",
             begin = 1990,
             end = 2017,
             schema = meta_madeUp_1,
             archive = "example_table.7z|example_table1.csv",
             archiveLink = "https://en.wikipedia.org/wiki/Data_lineage",
             downloadDate = as.Date("2019-10-01"),
             updateFrequency = "quarterly",
             metadataLink = "https://en.wikipedia.org/wiki/Metadata",
             metadataPath = "my/local/path")

    regTable(ADM0 = "aNation",
             subset = "barleyMaize",
             dSeries = "madeUp",
             gSeries = "gadm",
             match = "ADM1",
             begin = 1990,
             end = 2017,
             schema = meta_madeUp_2,
             archive = "example_table.7z|example_table2.csv",
             archiveLink = "https://en.wikipedia.org/wiki/Data_lineage",
             downloadDate = as.Date("2019-10-01"),
             updateFrequency = "quarterly",
             metadataLink = "https://en.wikipedia.org/wiki/Metadata",
             metadataPath = "my/local/path")

  }

  # 8. normalise geometries ----
  if(any(theSteps %in% "normGeometry")){
    normGeometry(verbose = verbose)
  }

  # 9. normalise tables ----
  if(any(theSteps %in% "normTable")){
    normTable(verbose = verbose)
  }

}
