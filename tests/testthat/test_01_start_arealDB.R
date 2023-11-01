library(testthat)
library(checkmate)
context("start_arealDB")

test_that("path has been added to the global options", {

  inPath <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)
  dbpath <- paste0(tempdir(), "/newDB")
  start_arealDB(root = dbpath, gazetteer = paste0(inPath, "/territories.rds"), ontology = list(commodity = paste0(inPath, "/ontology.rds")))

  out <- getOption("adb_path")
  expect_character(x = out)
  expect_true(out == dbpath)
})

test_that("Error if arguments have wrong value", {
  expect_error(start_arealDB(root = 1))
})
