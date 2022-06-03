library(testthat)
library(checkmate)
context("setPath")

test_that("path has been added to the global options", {

  dbpath <- paste0(tempdir(), "/newDB")
  start_arealDB(root = dbpath)

  out <- getOption("adb_path")
  expect_character(x = out)
  expect_true(out == dbpath)
})

test_that("Error if arguments have wrong value", {
  expect_error(start_arealDB(root = 1))
})
