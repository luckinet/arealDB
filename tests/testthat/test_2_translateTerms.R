library(testthat)
library(checkmate)
context("translateTerms")

test_that("translate already known terms", {
  # # this tests whether terms that are already known are recognised
  # path <- system.file("test_data", package="DMT", mustWork = TRUE)
  # registerTables(root = paste0(path, "/"))
  # input <- c("Cacao", "Malva", "Maní", "Cartamo")
  # out <- translateTerms(terms = input, index = "trans_commodities")
  #
  # expect_character(x = out, any.missing = FALSE, len = 4)
  #
  # restoreIndex(pattern = "trans_commodities", select = "oldest")
})

test_that("translate terms that are not yet known", {
  # doesn't seem to work, because of the user interaction
  # # this tests whether terms that are not yet known are recognised and inserted
  # # into the respective table
  # path <- system.file("test_data", package="DMT", mustWork = TRUE)
  # registerTables(root = paste0(path, "/"))
  # input <- c("banaan", "Malva", "ploom", "õun", "Cartamo")
  # out <- translateTerms(terms = input, index = "trans_commodities")
  #
  # expect_character(x = out, any.missing = FALSE, len = 5)
  #
  # restoreIndex(pattern = "trans_commodities", select = "oldest")
  # file.remove(list.files(path = paste0(path, "/log/"), full.names = T))
})

test_that("Error if arguments have wrong value", {
  # path <- system.file("test_data", package="DMT", mustWork = TRUE)
  # registerTables(root = paste0(path, "/"))
  # input <- c("Cacao", "Malva", "Maní", "Cartamo")
  #
  # expect_error(translateTerms(terms = 1))
  # expect_error(translateTerms(terms = input, index = "bla"))
  # expect_error(translateTerms(terms = input, index = id_commodities))
})
