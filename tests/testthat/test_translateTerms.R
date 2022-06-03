library(testthat)
library(checkmate)
context("translateTerms")

test_that("already known terms are correct", {

  dbpath <- paste0(tempdir(), "/newDB")
  makeExampleDB(until = "start_arealDB", path = dbpath)
  options(adb_testing = TRUE)

  input <- c("albania", "botswana", "estonia", "germany")
  output <- translateTerms(terms = input,
                           source = list("geoID" = 1),
                           index = "tt_nations")

  expect_tibble(x = output, nrows = 4, col.names = "strict")
  expect_names(names(output), must.include = c("origin", "target", "source", "ID", "notes"))
})

test_that("Error if arguments have wrong value", {

  dbpath <- paste0(tempdir(), "/newDB")
  makeExampleDB(until = "start_arealDB", path = dbpath)

  input <- c("albania", "botswana", "estonia", "germany")

  expect_error(translateTerms(terms = 1))
  expect_error(translateTerms(terms = input, index = "bla"))
  expect_error(translateTerms(terms = input, index = id_commodities))
})
