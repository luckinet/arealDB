library(testthat)
library(checkmate)
library(readr)
context("setVariables")


test_that("setting an imported table works", {

  makeExampleDB(until = "setPath")
  inPath <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)

  input <- read_csv(file = paste0(inPath, "/id_units.csv"), col_types = "iccc")
  output <- setVariables(input = input, variable = "territories", pid = "anID",
                         origin = "origin", target = "names")

  expect_tibble(x = output, nrows = 12, col.names = "strict")
  expect_names(names(output), must.include = c("anID", "target", "descr", "origin"))
  expect_names(list.files(path = getOption("adb_path")), must.include = c("id_territories.csv", "tt_territories.csv"))
})

test_that("Error if arguments have wrong value", {

  makeExampleDB(until = "setPath")
  inPath <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)

  input <- read_csv(file = paste0(inPath, "/id_units.csv"), col_types = "iccc")

  expect_error(setVariables(input = "bla"))
  expect_error(setVariables(input = input, variable = 1))
  expect_error(setVariables(input = input, variable = "bla", pid = "bla"))
  expect_error(setVariables(input = input, variable = "bla", origin = "bla"))
  expect_error(setVariables(input = input, variable = "bla", target = "bla"))
})

