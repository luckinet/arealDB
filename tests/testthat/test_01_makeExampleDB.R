library(arealDB)
library(testthat)
library(checkmate)
library(readr)
context("makeExampleDB")


test_that("make example DB until start_arealDB", {

  dbpath <- paste0(tempdir(), "/newDB")
  makeExampleDB(until = "start_arealDB", path = dbpath)

  expect_file_exists(x = paste0(dbpath, "/inv_dataseries.csv"))
  expect_file_exists(x = paste0(dbpath, "/inv_geometries.csv"))
  expect_file_exists(x = paste0(dbpath, "/inv_tables.csv"))
  assert_directory_exists(x = paste0(dbpath, "/adb_geometries"))
  assert_directory_exists(x = paste0(dbpath, "/adb_tables"))
  assert_directory_exists(x = paste0(dbpath, "/incoming"))

  assert_file_exists(x = paste0(dbpath, "/adb_geometries/stage1/example_geom.7z"))
  assert_file_exists(x = paste0(dbpath, "/adb_tables/stage1/example_table.7z"))
  assert_file_exists(x = paste0(dbpath, "/meta/schemas/example_schema.rds"))

})

# test_that("make example DB until match_ontology", {
#
#   dbpath <- paste0(tempdir(), "/newDB")
#   makeExampleDB(until = "match_ontology", path = dbpath)
#
#   expect_file_exists(x = paste0(dbpath, "/territories.rds"))
#   temp <- read_rds(paste0(dbpath, "/territories.rds"))
#
#   expect_class(x = temp, classes = "onto")
#   expect_list(x = temp@classes, len = 2)
#   expect_tibble(x = temp@classes$harmonised, nrows = 3, ncols = 8)
#   expect_tibble(x = temp@classes$external, nrows = 0, ncols = 4)
#   expect_list(x = temp@concepts, len = 2)
#   expect_tibble(x = temp@concepts$harmonised, nrows = 11, ncols = 9)
#   expect_tibble(x = temp@concepts$external, nrows = 18, ncols = 4)
#
# })

test_that("make example DB until regDataseries", {

  dbpath <- paste0(tempdir(), "/newDB")

  makeExampleDB(until = "regDataseries", path = dbpath)

  datID <- read_csv(file = paste0(dbpath, "/inv_dataseries.csv"), col_types = "icccccc")
  expect_true(object = all(dim(datID) == c(2, 7)))

})

test_that("make example DB until regGeometry", {

  dbpath <- paste0(tempdir(), "/newDB")

  makeExampleDB(until = "regGeometry", path = dbpath)

  geoID <- read_csv(file = paste0(dbpath, "/inv_geometries.csv"), col_types = "iicccccDDcc")
  expect_true(object = all(dim(geoID) == c(4, 11)))

  expect_file_exists(x = paste0(dbpath, "/adb_geometries/stage2/_al1__gadm.gpkg"))
  expect_file_exists(x = paste0(dbpath, "/adb_geometries/stage2/_al2__gadm.gpkg"))
  expect_file_exists(x = paste0(dbpath, "/adb_geometries/stage2/_al3__gadm.gpkg"))
  expect_file_exists(x = paste0(dbpath, "/adb_geometries/stage2/_al3__madeUp.gpkg"))

})

test_that("make example DB until regTable", {

  dbpath <- paste0(tempdir(), "/newDB")

  makeExampleDB(until = "regTable", path = dbpath)

  geoID <- read_csv(file = paste0(dbpath, "/inv_tables.csv"), col_types = "iiiccccDDcccc")
  expect_true(object = all(dim(geoID) == c(2, 13)))

  expect_file_exists(x = paste0(dbpath, "/adb_tables/stage2/_al1_barleyMaize_1990_2017_madeUp.csv"))
  expect_file_exists(x = paste0(dbpath, "/adb_tables/stage2/aNation_al2_barleyMaize_1990_2017_madeUp.csv"))

})

test_that("make example DB until normGeometry", {

  dbpath <- paste0(tempdir(), "/newDB")

  makeExampleDB(until = "normGeometry", path = dbpath)

  expect_file_exists(x = paste0(dbpath, "/adb_geometries/stage3/a_nation.gpkg"))

})

test_that("make example DB until normTable", {

  dbpath <- paste0(tempdir(), "/newDB")

  makeExampleDB(until = "normTable", path = dbpath)

  expect_file_exists(x = paste0(dbpath, "/adb_tables/stage3/a_nation.rds"))

})


