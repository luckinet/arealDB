library(arealDB)
library(testthat)
library(tabshiftr)
library(readr)
library(checkmate)
library(ontologics)
context("normTable")


test_that("tables can be normalised (without matched variables)", {

  dbpath <- paste0(tempdir(), "/newDB")
  makeExampleDB(until = "normGeometry", path = dbpath)

  output <- normTable(input = paste0(getOption("adb_path"), "/adb_tables/stage2/_al1_barleyMaize_1990_2017_madeUp.csv"),
                      update = TRUE)
  onto <- load_ontology(path = paste0(dbpath, "/territories.rds"))
  expect_set_equal(x = onto@concepts$harmonised$label,
                   y = c("a_nation", "county_1", "municipality1_1", "municipality1_2", "county_2", "municipality2_1",
                         "county_3", "municipality3", "county_4", "municipality4_1", "municipality4_2"))
  expect_set_equal(x = onto@concepts$harmonised$has_close_match,
                   y = c("gadm_1.3 | madeUp_1.3", "gadm_2.3 | madeUp_2.3", "gadm_6.3", "gadm_7.3", "gadm_3.3 | madeUp_3.3",
                         "gadm_8.3 | madeUp_6.3", "gadm_4.3 | madeUp_4.3", "gadm_9.3 | madeUp_7.3", "gadm_5.3 | madeUp_5.3",
                         "gadm_10.3 | madeUp_8.3", "gadm_11.3 | madeUp_9.3"))
  expect_set_equal(x = onto@concepts$harmonised$has_broader_match,
                   y = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  expect_set_equal(x = onto@concepts$harmonised$has_narrower_match,
                   y = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

  # test whether the resulting file is "correct" ----
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_tables/stage2/processed/_al1_barleyMaize_1990_2017_madeUp.csv"))
  final <- readRDS(file = paste0(getOption("adb_path"), "/adb_tables/stage3/a_nation.rds"))
  expect_tibble(x = final, types = c("integer", "integer", "integer", "character", "integer", "character", "double", "double"))
  expect_data_frame(x = final, nrows = 56, ncols = 9)
  expect_names(x = names(final), identical.to = c("tabID", "geoID", "gazID", "gazName", "gazMatch", "year", "commodity", "harvested", "production"))

})

test_that("tables can be normalised (with matched variables)", {

  dbpath <- paste0(tempdir(), "/newDB")
  makeExampleDB(until = "normGeometry", path = dbpath)

  output <- normTable(update = TRUE)

  # test whether the resulting file is "correct" ----
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_tables/stage2/processed/_al1_barleyMaize_1990_2017_madeUp.csv"))
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_tables/stage2/processed/aNation_al2_barleyMaize_1990_2017_madeUp.csv"))
  final <- readRDS(file = paste0(getOption("adb_path"), "/adb_tables/stage3/a_nation.rds"))
  expect_data_frame(x = final, nrows = 280, ncols = 9)
  expect_names(x = names(final), identical.to = c("tabID", "geoID", "gazID", "gazName", "gazMatch", "year", "commodity", "harvested", "production"))

})
