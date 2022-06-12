library(testthat)
library(tabshiftr)
library(readr)
library(checkmate)
context("normTable")


test_that("tables can be normalised (without matched variables)", {

  dbpath <- paste0(tempdir(), "/newDB")
  makeExampleDB(until = "normGeometry", path = dbpath)

  output <- normTable(input = paste0(getOption("adb_path"), "/adb_tables/stage2/_1_barleyMaize_1990_2017_madeUp.csv"),
                      update = TRUE)

  # test whether the resulting file is "correct" ----
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_tables/stage2/processed/_1_barleyMaize_1990_2017_madeUp.csv"))
  final <- readRDS(file = paste0(getOption("adb_path"), "/adb_tables/stage3/a_nation.rds"))
  expect_tibble(x = final, types = c("integer", "integer", "integer", "character", "integer", "character", "double", "double"))
  expect_data_frame(x = final, nrows = 56, ncols = 9)
  expect_names(x = names(final), identical.to = c("id", "ahName", "ahID", "tabID", "geoID", "year", "commodities", "harvested", "production"))

})

test_that("tables can be normalised (with matched variables)", {

  dbpath <- paste0(tempdir(), "/newDB")
  makeExampleDB(until = "normGeometry", path = dbpath)

  output <- normTable(update = TRUE)

  # test whether the resulting file is "correct" ----
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_tables/stage2/processed/_1_barleyMaize_1990_2017_madeUp.csv"))
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_tables/stage2/processed/aNation_2_barleyMaize_1990_2017_madeUp.csv"))
  final <- readRDS(file = paste0(getOption("adb_path"), "/adb_tables/stage3/a_nation.rds"))
  expect_data_frame(x = final, nrows = 280, ncols = 9)
  expect_names(x = names(final), identical.to = c("id", "ahName", "ahID", "tabID", "geoID", "year", "commodities", "harvested", "production"))

})
