library(arealDB)
library(sf)
library(testthat)
library(checkmate)
context("normGeometry")

test_that("geometries can be normalised", {

  dbpath <- paste0(tempdir(), "/newDB")
  adb_example(until = "regTable", path = dbpath)

  inPath <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)

  # normalise first level ----
  output <- normGeometry(input = paste0(.adb_state$path, "/geometries/stage2/_ADM0__gadm.gpkg"))
  expect_tibble(x = output, nrows = 1, ncols = 12, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "stage2_name", "layer", "label", "ancillary", "stage1_name", "stage1_url", "download_date", "update_frequency", "notes"))

  final <- st_read(dsn = paste0(.adb_state$path, "/geometries/stage3/a_nation.gpkg"), layer = "ADM0", quiet = TRUE)
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 1, ncols = 7)
  expect_names(x = names(final), identical.to = c("gazID", "gazName", "gazClass", "match", "external", "geoID", "geom"))

  # normalise second level ----
  output <- normGeometry(input = paste0(.adb_state$path, "/geometries/stage2/_ADM1__gadm.gpkg"))
  expect_tibble(x = output, nrows = 1, ncols = 12, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "stage2_name", "layer", "label", "ancillary", "stage1_name", "stage1_url", "download_date", "update_frequency", "notes"))

  final <- st_read(dsn = paste0(.adb_state$path, "/geometries/stage3/a_nation.gpkg"), layer = "ADM1", quiet = TRUE)
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 4, ncols = 7)
  expect_names(x = names(final), identical.to = c("gazID", "gazName", "gazClass", "match", "external", "geoID", "geom"))

  # normalise third level ----
  output <- normGeometry(input = paste0(.adb_state$path, "/geometries/stage2/_ADM2__gadm.gpkg"))
  expect_tibble(x = output, nrows = 1, ncols = 12, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "stage2_name", "layer", "label", "ancillary", "stage1_name", "stage1_url", "download_date", "update_frequency", "notes"))

  # normalise a non-gadm dataset that has been attached to the DB ----
  output <- normGeometry(input = paste0(.adb_state$path, "/geometries/stage2/_ADM2__madeUp.gpkg"))
  expect_tibble(x = output, nrows = 1, ncols = 12, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "stage2_name", "layer", "label", "ancillary", "stage1_name", "stage1_url", "download_date", "update_frequency", "notes"))

  final <- st_read(dsn = paste0(.adb_state$path, "/geometries/stage3/a_nation.gpkg"), layer = "ADM2", quiet = TRUE)
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 10, ncols = 7)
  expect_names(x = names(final), identical.to = c("gazID", "gazName", "gazClass", "match", "external", "geoID", "geom"))

})
