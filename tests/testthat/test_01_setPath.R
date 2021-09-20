library(testthat)
library(checkmate)
context("setPath")

test_that("path has been added to the global options", {

  path <- tempdir()
  setPath(root = path)

  out <- getOption("adb_path")
  expect_character(x = out)
  expect_true(out == path)

  unlink(path, recursive = TRUE)
})

test_that("Error if arguments have wrong value", {
  expect_error(setPath(root = 1))
})
