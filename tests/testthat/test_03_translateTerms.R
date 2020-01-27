library(testthat)
library(checkmate)
context("translateTerms")

test_that("already known terms are correct", {
  # this tests whether terms that are already known are recognised
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  input <- c("albania", "botswana", "estonia", "germany")
  output <- translateTerms(terms = input,
                        source = list("geoID" = 1),
                        index = "tt_territories")

  expect_tibble(x = output, nrows = 4, col.names = "strict")
  expect_names(names(output), must.include = c("origin", "target", "source", "ID", "notes"))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

test_that("not yet known terms are properly translated", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  input <- c("coruscant", "kashyyyk", "alderaan")
  output <- translateTerms(terms = input,
                        source = list("geoID" = 1),
                        index = "tt_territories",
                        inline = FALSE)

  expect_tibble(x = output, nrows = 3, col.names = "strict")
  expect_names(names(output), must.include = c("origin", "target", "source", "ID", "notes"))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

test_that("Error if arguments have wrong value", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  input <- c("albania", "botswana", "estonia", "germany")

  expect_error(translateTerms(terms = 1))
  expect_error(translateTerms(terms = input, index = "bla"))
  expect_error(translateTerms(terms = input, index = id_commodities))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})
