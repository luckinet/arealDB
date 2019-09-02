library(testthat)
library(checkmate)
context("setPath")

test_that("path has been added to the global options", {
  # path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  # setPath(root = paste0(path, "/"))
  #
  # out <- getOption("adb_path")
  # expect_character(x = out)
  # expect_true(out == path)
})

test_that("Error if arguments have wrong value", {
  expect_error(registerTables(root = 1))
})

