library(testthat)
library(checkmate)
library(readr)
context("regDataseries")


test_that("a dataseries inventory entry can be produced", {

  dbpath <- paste0(tempdir(), "/newDB")
  makeExampleDB(until = "start_arealDB", path = dbpath)
  inPath <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)

  output <- regDataseries(name = "gadm",
                          description = "Database of Global Administrative Areas",
                          homepage = "https://gadm.org/index.html",
                          licence_link = "https://gadm.org/license.html",
                          licence_path = ".../Luckinet/licenceFiles/licence.txt", # this requires an example path, because otherwise the output of that function is a question that asks for this path.
                          update = TRUE)

  expect_tibble(x = output, nrows = 1, ncols = 7, col.names = "strict")
  expect_names(x = names(output), must.include = c("datID", "name", "description", "homepage", "licence_link", "licence_path", "notes"))

  # check also whether the entry is in inv_dataseries.csv
  db <- read_csv(file = paste0(getOption("adb_path"), "/inv_dataseries.csv"), col_types = c("icccccc"))

  expect_equal(output[-7], db[-7])
})

test_that("function asks for details, if not provided", {

  dbpath <- paste0(tempdir(), "/newDB")
  makeExampleDB(until = "start_arealDB", path = dbpath)

  expect_message(object = regDataseries())
  output <- capture_messages(code = regDataseries())
  expect_character(x = output, len = 5, any.missing = FALSE, unique = TRUE)
  expect_equal(object = output[1], expected = "please type in the dataseries abbreviation: \n")
  expect_equal(object = output[2], expected = "please type in the long name or description of the series: \n")
  expect_equal(object = output[3], expected = "please type in the dataseries homepage: \n")
  expect_equal(object = output[4], expected = "please type in the weblink to the dataseries licence: \n")
  expect_equal(object = output[5], expected = "please type in the path to the local folder where the licence is stored: \n")
})