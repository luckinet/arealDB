library(arealDB)
library(testthat)
library(tabshiftr)
library(readr)
library(checkmate)
context("normTable")


test_that("tables can be normalised (without matched variables)", {

  dbpath <- paste0(tempdir(), "/newDB")
  adb_example(until = "normGeometry", path = dbpath)

  output <- normTable(input = paste0(arealDB:::.adb_state$path, "/tables/stage2/_ADM0_barleyMaize_1990_2017_madeUp.csv"))

  # test whether the resulting file is "correct" ----
  final <- arrow::read_parquet(paste0(arealDB:::.adb_state$path, "/tables/stage3/a_nation.parquet"))
  expect_data_frame(x = final, nrows = 56, ncols = 13)
  expect_names(x = names(final), identical.to = c("tabID", "geoID", "gazetteerName", "gazetteerID", "gazetteerMatch", "gazetteerClass", "commodityName", "commodityID", "commodityMatch", "commodityClass", "year", "harvested", "production"))

})

test_that("tables can be normalised (with matched variables)", {

  dbpath <- paste0(tempdir(), "/newDB")
  adb_example(until = "normGeometry", path = dbpath)

  output <- normTable()

  # test whether the resulting file is "correct" ----
  final <- arrow::read_parquet(paste0(.adb_state$path, "/tables/stage3/a_nation.parquet"))
  expect_data_frame(x = final, nrows = 280, ncols = 13)
  expect_names(x = names(final), identical.to = c("tabID", "geoID", "gazetteerName", "gazetteerID", "gazetteerMatch", "gazetteerClass", "commodityName", "commodityID", "commodityMatch", "commodityClass", "year", "harvested", "production"))

})
