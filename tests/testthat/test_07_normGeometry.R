library(sf)
library(testthat)
library(checkmate)
context("normGeometry")

test_that("geometries can be normalised", {

  makeExampleDB(until = "regTable")

  inPath <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)

  # normalise first level ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/adb_geometries/stage2/_1__gadm.gpkg"),
                         update = TRUE)
  expect_tibble(x = output, nrows = 1, ncols = 13, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "level", "source_file",
                                                   "layer", "nation_column", "unit_column", "orig_file", "orig_link",
                                                   "download_date", "next_update", "update_frequency", "notes"))
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_geometries/stage2/processed/_1__gadm.gpkg"))

  # normalise second level ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/adb_geometries/stage2/_2__gadm.gpkg"),
                         update = TRUE)
  expect_tibble(x = output, nrows = 1, ncols = 13, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "level", "source_file",
                                                   "layer", "nation_column", "unit_column", "orig_file", "orig_link",
                                                   "download_date", "next_update", "update_frequency", "notes"))
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_geometries/stage2/processed/_2__gadm.gpkg"))

  # normalise third level ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/adb_geometries/stage2/_3__gadm.gpkg"), update = TRUE)
  expect_tibble(x = output, nrows = 1, ncols = 13, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "level", "source_file",
                        "layer", "nation_column", "unit_column", "orig_file", "orig_link",
                        "download_date", "next_update", "update_frequency", "notes"))
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_geometries/stage2/processed/_3__gadm.gpkg"))

  # normalise a non-gadm dataset that has been attached to the DB ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/adb_geometries/stage2/_3__madeUp.gpkg"), nation = "estonia", update = TRUE)
  expect_tibble(x = output, nrows = 1, ncols = 13, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "level", "source_file",
                        "layer", "nation_column", "unit_column", "orig_file", "orig_link",
                        "download_date", "next_update", "update_frequency", "notes"))

  # test whether the resulting file is "correct" ----
  final <- st_read(dsn = paste0(getOption("adb_path"), "/adb_geometries/stage3/Estonia.gpkg"), layer = "level_1", quiet = TRUE)
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 1, ncols = 7)
  expect_names(x = names(final), identical.to = c("nation", "name", "level", "ahID", "geoID", "al1_id", "geom"))

  final <- st_read(dsn = paste0(getOption("adb_path"), "/adb_geometries/stage3/Estonia.gpkg"), layer = "level_2", quiet = TRUE)
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 4, ncols = 8)
  expect_names(x = names(final), identical.to = c("nation", "name", "level", "ahID", "geoID", "al1_id", "al2_id", "geom"))

  final <- st_read(dsn = paste0(getOption("adb_path"), "/adb_geometries/stage3/Estonia.gpkg"), layer = "level_3", quiet = TRUE)
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 12, ncols = 9)
  expect_names(x = names(final), identical.to = c("nation", "name", "level", "ahID", "geoID", "al1_id", "al2_id", "al3_id", "geom"))
})
