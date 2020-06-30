library(testthat)
library(checkmate)
context("translateTerms")

test_that("already known terms are correct", {

  makeExampleDB(until = "setPath")

  input <- c("albania", "botswana", "estonia", "germany")
  output <- translateTerms(terms = input,
                           source = list("geoID" = 1),
                           index = "tt_nations")

  expect_tibble(x = output, nrows = 4, col.names = "strict")
  expect_names(names(output), must.include = c("origin", "target", "source", "ID", "notes"))
})

test_that("not yet known terms are properly translated", {

  makeExampleDB(until = "setPath")

  input <- c("coruscant", "kashyyyk", "alderaan")
  output <- translateTerms(terms = input,
                           source = list("geoID" = 1),
                           index = "tt_nations",
                           inline = FALSE)

  expect_tibble(x = output, nrows = 3, col.names = "strict")
  expect_names(names(output), must.include = c("origin", "target", "source", "ID", "notes"))
})

test_that("Error if arguments have wrong value", {

  makeExampleDB(until = "setPath")

  input <- c("albania", "botswana", "estonia", "germany")

  expect_error(translateTerms(terms = 1))
  expect_error(translateTerms(terms = input, index = "bla"))
  expect_error(translateTerms(terms = input, index = id_commodities))
})
