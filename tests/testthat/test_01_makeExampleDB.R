library(testthat)
library(checkmate)
library(readr)
context("makeExampleDB")


test_that("make example DB until setPath", {

  dbpath <- paste0(tempdir(), "/newDB")
  makeExampleDB(until = "setPath", path = dbpath)

  expect_file_exists(x = paste0(dbpath, "/inv_dataseries.csv"))
  expect_file_exists(x = paste0(dbpath, "/inv_geometries.csv"))
  expect_file_exists(x = paste0(dbpath, "/inv_tables.csv"))
  assert_directory_exists(x = paste0(dbpath, "/adb_geometries"))
  assert_directory_exists(x = paste0(dbpath, "/adb_tables"))
  assert_directory_exists(x = paste0(dbpath, "/incoming"))

  assert_file_exists(x = paste0(dbpath, "/adb_geometries/stage1/example_geom.7z"))
  assert_file_exists(x = paste0(dbpath, "/adb_tables/stage1/example_table.7z"))
  assert_file_exists(x = paste0(dbpath, "/adb_tables/meta/schemas/example_schema.rds"))

})

test_that("make example DB until setVariables", {

  dbpath <- paste0(tempdir(), "/newDB")
  makeExampleDB(until = "setVariables", path = dbpath)

  expect_file_exists(x = paste0(dbpath, "/id_commodities.csv"))
  expect_file_exists(x = paste0(dbpath, "/tt_commodities.csv"))
  expect_file_exists(x = paste0(dbpath, "/id_territories.csv"))
  expect_file_exists(x = paste0(dbpath, "/tt_territories.csv"))

})

test_that("make example DB until regDataseries", {

  dbpath <- paste0(tempdir(), "/newDB")

  makeExampleDB(until = "regDataseries", path = dbpath)

  datID <- read_csv(file = paste0(dbpath, "/inv_dataseries.csv"), col_types = "icccccc")
  expect_true(object = all(dim(datID) == c(2, 7)))

})

test_that("make example DB until regGeometry", {

  dbpath <- paste0(tempdir(), "/newDB")

  makeExampleDB(until = "regGeometry", path = dbpath)

  geoID <- read_csv(file = paste0(dbpath, "/inv_geometries.csv"), col_types = "iiiccccccDDcc")
  expect_true(object = all(dim(geoID) == c(4, 13)))

  expect_file_exists(x = paste0(dbpath, "/adb_geometries/stage2/_1__gadm.gpkg"))
  expect_file_exists(x = paste0(dbpath, "/adb_geometries/stage2/_2__gadm.gpkg"))
  expect_file_exists(x = paste0(dbpath, "/adb_geometries/stage2/_3__gadm.gpkg"))
  expect_file_exists(x = paste0(dbpath, "/adb_geometries/stage2/_3__madeUp.gpkg"))

})

test_that("make example DB until regTable", {

  dbpath <- paste0(tempdir(), "/newDB")

  makeExampleDB(until = "regTable", path = dbpath)

  geoID <- read_csv(file = paste0(dbpath, "/inv_tables.csv"), col_types = "iiiccccDDcccc")
  expect_true(object = all(dim(geoID) == c(2, 13)))

  expect_file_exists(x = paste0(dbpath, "/adb_tables/stage2/est_1_barleyMaize_1990_2017_madeUp.csv"))
  expect_file_exists(x = paste0(dbpath, "/adb_tables/stage2/est_2_barleyMaize_1990_2017_madeUp.csv"))

})

test_that("make example DB until normGeometry", {

  dbpath <- paste0(tempdir(), "/newDB")

  makeExampleDB(until = "normGeometry", path = dbpath)

  expect_file_exists(x = paste0(dbpath, "/adb_geometries/stage3/Estonia.gpkg"))

})

test_that("make example DB until normTable", {

  dbpath <- paste0(tempdir(), "/newDB")

  makeExampleDB(until = "normTable", path = dbpath)

  expect_file_exists(x = paste0(dbpath, "/adb_tables/stage3/Estonia.rds"))

})


