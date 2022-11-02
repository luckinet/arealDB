library(sf)
library(testthat)
library(checkmate)
context("normGeometry")

test_that("geometries can be normalised", {

  dbpath <- paste0(tempdir(), "/newDB")
  makeExampleDB(until = "regTable", path = dbpath)

  inPath <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)

  # normalise first level ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/adb_geometries/stage2/_al1__gadm.gpkg"),
                         update = TRUE)
  expect_tibble(x = output, nrows = 1, ncols = 12, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "label", "source_file",
                                                   "layer", "hierarchy", "orig_file", "orig_link",
                                                   "download_date", "next_update", "update_frequency", "notes"))
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_geometries/stage2/processed/_al1__gadm.gpkg"))

  # normalise second level ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/adb_geometries/stage2/_al2__gadm.gpkg"),
                         update = TRUE)
  expect_tibble(x = output, nrows = 1, ncols = 12, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "label", "source_file",
                                                   "layer", "hierarchy", "orig_file", "orig_link",
                                                   "download_date", "next_update", "update_frequency", "notes"))
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_geometries/stage2/processed/_al2__gadm.gpkg"))

  # normalise third level ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/adb_geometries/stage2/_al3__gadm.gpkg"),
                         update = TRUE)
  expect_tibble(x = output, nrows = 1, ncols = 12, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "label", "source_file",
                        "layer", "hierarchy", "orig_file", "orig_link",
                        "download_date", "next_update", "update_frequency", "notes"))
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_geometries/stage2/processed/_al3__gadm.gpkg"))

  # normalise a non-gadm dataset that has been attached to the DB ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/adb_geometries/stage2/_al3__madeUp.gpkg"),
                         update = TRUE)
  expect_tibble(x = output, nrows = 1, ncols = 12, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "label", "source_file",
                        "layer", "hierarchy", "orig_file", "orig_link",
                        "download_date", "next_update", "update_frequency", "notes"))

  # test whether the resulting file is "correct" ----
  final <- st_read(dsn = paste0(getOption("adb_path"), "/adb_geometries/stage3/a_nation.gpkg"), layer = "al1", quiet = TRUE)
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 1, ncols = 5)
  expect_names(x = names(final), identical.to = c("ahName", "ahID", "onto_class", "geoID", "geom"))

  final <- st_read(dsn = paste0(getOption("adb_path"), "/adb_geometries/stage3/a_nation.gpkg"), layer = "al2", quiet = TRUE)
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 4, ncols = 5)
  expect_names(x = names(final), identical.to = c("ahName", "ahID", "onto_class", "geoID", "geom"))

  final <- st_read(dsn = paste0(getOption("adb_path"), "/adb_geometries/stage3/a_nation.gpkg"), layer = "al3", quiet = TRUE)
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 12, ncols = 5)
  expect_names(x = names(final), identical.to = c("ahName", "ahID", "onto_class", "geoID", "geom"))

})
