library(testthat)
library(checkmate)
context("regDataseries")


test_that("a dataseries inventory entry can be produced", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))

  output <- regDataseries(name = "gadm",
                          description = "Database of Global Administrative Areas",
                          homepage = "https://gadm.org/index.html",
                          licence_link = "https://gadm.org/license.html",
                          licence_path = ".../Luckinet/licenceFiles/licence.txt",
                          update = TRUE)

  expect_tibble(x = output, nrows = 1, ncols = 7, col.names = "strict")
  expect_names(x = names(output), must.include = c("datID", "name", "description", "homepage", "licence_link", "licence_path", "notes"))

  # check also whether the entry is in inv_dataseries.csv
  db <- read_csv(file = paste0(path, "/newDB/inv_dataseries.csv"), col_types = c("icccccc"))

  expect_equal(output, db)

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

test_that("function asks for details, if not provided", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  options(adb_testing = TRUE)

  expect_message(object = regDataseries())
  output <- capture_messages(code = regDataseries())
  expect_character(x = output, len = 5, any.missing = FALSE, unique = TRUE)
  expect_equal(object = output[1], expected = "please type in the dataseries abbreviation: \n")
  expect_equal(object = output[2], expected = "please type in the long name or description of the series: \n")
  expect_equal(object = output[3], expected = "please type in the dataseries homepage: \n")
  expect_equal(object = output[4], expected = "please type in the weblink to the dataseries licence: \n")
  expect_equal(object = output[5], expected = "please type in the path to the local folder where the licence is stored: \n")

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

test_that("Error if arguments have wrong value", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))

  expect_error(regDataseries(update = 1))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})