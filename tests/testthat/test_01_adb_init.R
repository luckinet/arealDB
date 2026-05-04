library(testthat)
library(checkmate)
context("adb_init")

test_that("path has been added to the global options", {

  dbpath <- paste0(tempdir(), "/newDB")
  if(dir.exists(dbpath)) unlink(dbpath, recursive = TRUE)

  adb_init(root = dbpath,
           version = "some0.0.1", licence = "https://creativecommons.org/licenses/by-sa/4.0/",
           author = list(cre = "Jane Doe", aut = "John Doe", ctb = "Jamie Roe"),
           level = "ADM0")

  expect_true(dir.exists(file.path(dbpath, "vocabularies", "stage1")))
  expect_true(dir.exists(file.path(dbpath, "vocabularies", "stage2")))
  expect_true(dir.exists(file.path(dbpath, "vocabularies", "stage3")))
  expect_true(dir.exists(file.path(dbpath, "vocabularies", "mappings")))
  expect_true(dir.exists(file.path(dbpath, "vocabularies", "schemas")))
  expect_true(dir.exists(file.path(dbpath, "tables", "schemas")))

  out <- .adb_state$path
  expect_character(x = out)
  expect_true(out == dbpath)
})

test_that("Error if arguments have wrong value", {
  expect_error(adb_init(root = 1))
})
