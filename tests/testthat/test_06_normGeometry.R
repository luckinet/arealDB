library(sf)
library(testthat)
library(checkmate)
library(arealDB)
library(ontologics)
context("normGeometry")

test_that("geometries can be normalised", {

  dbpath <- paste0(tempdir(), "/newDB")
  makeExampleDB(until = "regTable", path = dbpath)

  inPath <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)

  # normalise first level ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/adb_geometries/stage2/_al1__gadm.gpkg"),
                         update = TRUE)
  expect_tibble(x = output, nrows = 1, ncols = 11, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "source_file",
                                                   "layer", "label", "orig_file", "orig_link",
                                                   "download_date", "next_update", "update_frequency", "notes"))
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_geometries/stage2/processed/_al1__gadm.gpkg"))

  final <- st_read(dsn = paste0(getOption("adb_path"), "/adb_geometries/stage3/a_nation.gpkg"), layer = "al1", quiet = TRUE)
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 1, ncols = 7)
  expect_names(x = names(final), identical.to = c("gazID", "gazName", "gazClass", "match", "external", "geoID", "geom"))
  onto <- load_ontology(path = paste0(dbpath, "/territories.rds"))
  expect_set_equal(x = onto@concepts$harmonised$label,
                   y = c("a_nation", "county_1", "municipality1_1", "municipality1_2", "county_2", "municipality2_1",
                         "county_3", "municipality3", "county_4", "municipality4_1", "municipality4_2"))
  expect_set_equal(x = onto@concepts$harmonised$has_close_match,
                   y = c("gadm_1.3",NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), )

  # normalise second level ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/adb_geometries/stage2/_al2__gadm.gpkg"),
                         update = TRUE)
  expect_tibble(x = output, nrows = 1, ncols = 11, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "label", "source_file",
                                                   "layer", "orig_file", "orig_link",
                                                   "download_date", "next_update", "update_frequency", "notes"))
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_geometries/stage2/processed/_al2__gadm.gpkg"))

  final <- st_read(dsn = paste0(getOption("adb_path"), "/adb_geometries/stage3/a_nation.gpkg"), layer = "al2", quiet = TRUE)
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 4, ncols = 7)
  expect_names(x = names(final), identical.to = c("gazID", "gazName", "gazClass", "match", "external", "geoID", "geom"))
  onto <- load_ontology(path = paste0(dbpath, "/territories.rds"))
  expect_set_equal(x = onto@concepts$harmonised$label,
                   y = c("a_nation", "county_1", "municipality1_1", "municipality1_2", "county_2", "municipality2_1",
                         "county_3", "municipality3", "county_4", "municipality4_1", "municipality4_2"))
  expect_set_equal(x = onto@concepts$harmonised$has_close_match,
                   y = c("gadm_1.3","gadm_2.3", NA, NA, "gadm_3.3", NA, "gadm_4.3", NA, "gadm_5.3", NA, NA), )

  # normalise third level ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/adb_geometries/stage2/_al3__gadm.gpkg"),
                         update = TRUE)
  expect_tibble(x = output, nrows = 1, ncols = 11, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "label", "source_file",
                        "layer", "orig_file", "orig_link",
                        "download_date", "next_update", "update_frequency", "notes"))
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_geometries/stage2/processed/_al3__gadm.gpkg"))

  # normalise a non-gadm dataset that has been attached to the DB ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/adb_geometries/stage2/_al3__madeUp.gpkg"),
                         priority = "spatial",
                         update = TRUE)
  expect_tibble(x = output, nrows = 1, ncols = 11, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "label", "source_file",
                        "layer", "orig_file", "orig_link",
                        "download_date", "next_update", "update_frequency", "notes"))

  final <- st_read(dsn = paste0(getOption("adb_path"), "/adb_geometries/stage3/a_nation.gpkg"), layer = "al3", quiet = TRUE)
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 12, ncols = 7)
  expect_names(x = names(final), identical.to = c("gazID", "gazName", "gazClass", "match", "external", "geoID", "geom"))
  expect_names(x = final$gazID, identical.to = c(".001.001.001", ".001.001.002", ".001.001.003", ".001.001.004", ".001.002.001",
                                                 ".001.002.001", ".001.003.001", ".001.003.001", ".001.004.001", ".001.004.001",
                                                 ".001.004.002", ".001.004.002"))
  expect_names(x = final$match, identical.to = c("close", "close", "broader [83<>100_.001.001.001] | broader [17<>20_.001.001.002]",
                                                 "narrower [100<>80_.001.001.002]", "close", "close [100<>100_.001.002.001]",
                                                 "close", "close [100<>100_.001.003.001]", "close", "close [100<>100_.001.004.001]",
                                                 "close", "close [100<>100_.001.004.002]"))
  onto <- load_ontology(path = paste0(dbpath, "/territories.rds"))
  expect_set_equal(x = onto@concepts$harmonised$label,
                   y = c("a_nation", "county_1", "municipality1_1", "municipality1_2", "Gemeinde 13", "Gemeinde 14", "county_2",
                         "municipality2_1", "county_3", "municipality3", "county_4", "municipality4_1",
                         "municipality4_2"))
  expect_set_equal(x = onto@concepts$harmonised$has_close_match,
                   y = c("gadm_1.3 | madeUp_1.3", "gadm_2.3", "gadm_6.3", "gadm_7.3", NA, NA, "gadm_3.3", "gadm_8.3 | madeUp_2.3",
                         NA, "gadm_4.3", "gadm_9.3 | madeUp_3.3", NA, "gadm_5.3", "gadm_10.3 | madeUp_4.3", "gadm_11.3 | madeUp_5.3",
                         NA, NA ))
  expect_set_equal(x = onto@concepts$harmonised$has_broader_match,
                   y = c(NA, NA, NA, NA, "madeUp_7.3", "madeUp_7.3", NA, NA, NA, NA, NA))
  expect_set_equal(x = onto@concepts$harmonised$has_narrower_match,
                   y = c(NA, NA, NA, "madeUp_6.3", NA, NA, NA, NA, NA, NA, NA))

})
