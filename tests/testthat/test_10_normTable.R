library(testthat)
library(tabshiftr)
library(readr)
library(magrittr)
library(checkmate)
context("normTable")


test_that("tables can be normalised (without matched variables)", {

  makeExampleDB(until = "normGeometry")

  output <- normTable(input = paste0(getOption("adb_path"), "/adb_tables/stage2/est_1_soyMaize_1990_2017_madeUp.csv"),
                      update = TRUE)

  # test whether the resulting file is "correct" ----
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_tables/stage2/processed/est_1_soyMaize_1990_2017_madeUp.csv"))
  final <- read_csv(file = paste0(getOption("adb_path"), "/adb_tables/stage3/estonia.csv"))
  expect_tibble(x = final, types = c("integer", "integer", "integer", "character", "integer", "character", "double", "double"))
  expect_data_frame(x = final, nrows = 56, ncols = 8)
  expect_names(x = names(final), identical.to = c("id", "tabID", "geoID", "ahID", "year", "commodities", "harvested", "production"))
})

test_that("tables can be normalised (with matched variables)", {

  makeExampleDB(until = "normGeometry")

  output <- normTable(input = paste0(getOption("adb_path"), "/adb_tables/stage2/est_1_soyMaize_1990_2017_madeUp.csv"),
                      faoID = list(commodities = "target"),
                      update = TRUE)

  # test whether the resulting file is "correct" ----
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_tables/stage2/processed/est_1_soyMaize_1990_2017_madeUp.csv"))
  final <- read_csv(file = paste0(getOption("adb_path"), "/adb_tables/stage3/estonia.csv"))
  expect_data_frame(x = final, nrows = 56, ncols = 8)
  expect_names(x = names(final), identical.to = c("id", "tabID", "geoID", "ahID", "faoID", "year", "harvested", "production"))
})
