library(testthat)
library(checkmate)
context("setVariables")


test_that("setting an imported table works", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))

  input <- read_csv(file = paste0(path, "/id_units.csv"), col_types = "icc")
  output <- setVariables(input = input, variable = "territories", pid = "anID", target = "names")

  expect_tibble(x = output, nrows = 10, col.names = "strict")
  expect_names(names(output), must.include = c("anID", "target", "descr"))
  expect_names(list.files(path = paste0(path, "/newDB")), must.include = c("id_territories.csv", "tt_territories.csv"))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

test_that("Error if arguments have wrong value", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))

  input <- read_csv(file = paste0(path, "/id_units.csv"), col_types = "icc")

  expect_error(setVariables(input = "bla"))
  expect_error(setVariables(input = input, variable = 1))
  expect_error(setVariables(input = input, variable = "bla", pid = "bla"))
  expect_error(setVariables(input = input, variable = "bla", origin = "bla"))
  expect_error(setVariables(input = input, variable = "bla", target = "bla"))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

