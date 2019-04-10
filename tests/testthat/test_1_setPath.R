library(testthat)
library(checkmate)
context("registerTables")

test_that("path has been added to the global options", {
  path <- system.file("test_data", package="DMT", mustWork = TRUE)
  setPath(root = paste0(path, "/"))

  out <- getOption("dmt_path")
  expect_character(x = out)
  expect_true(out == path)
})

test_that("Error if arguments have wrong value", {
  expect_error(registerTables(root = 1))
})

