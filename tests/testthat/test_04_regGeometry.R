library(testthat)
library(checkmate)
library(arealDB)
context("regGeometry")


test_that("a geometry inventory entry is produced", {

  dbpath <- paste0(tempdir(), "/newDB")
  makeExampleDB(until = "regDataseries", path = dbpath)

  output <- regGeometry(gSeries = "gadm",
                        label = list(al1 = "NAME_0"),
                        layer = "example_geom1",
                        archive = "example_geom.7z|example_geom1.gpkg",
                        archiveLink = "https://gadm.org/downloads/example_geom.7z.html",
                        nextUpdate = "2019-10-01",
                        updateFrequency = "quarterly",
                        update = TRUE)

  expect_tibble(x = output, nrows = 1, ncols = 11, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "source_file", "layer",
                                                   "label", "orig_file", "orig_link", "download_date",
                                                   "next_update", "update_frequency", "notes"))

})

test_that("function asks for details, if not provided", {

  dbpath <- paste0(tempdir(), "/newDB")
  makeExampleDB(until = "regDataseries", path = dbpath)
  options(adb_testing = TRUE)

  expect_message(object = regGeometry(label = list(al1 = "NAME_0")))
  output <- capture_messages(code = regGeometry(label = list(al1 = "NAME_0")))
  expect_character(x = output, len = 6, any.missing = FALSE, unique = TRUE)
  expect_equal(object = output[1], expected = "please type in to which series the geometry belongs: \n")
  expect_equal(object = output[2], expected = "please type in the archives' file name: \n")
  expect_equal(object = output[3], expected = "please type in the weblink from which the archive was downloaded: \n")
  expect_equal(object = output[4], expected = "please type in the frequency in which the table gets updated ...\n")
  expect_equal(object = output[5], expected = "please type in when the geometry gets its next update (YYYY-MM-DD): \n")
  expect_equal(object = output[6], expected = "... the filename is '_al1__gadm.gpkg'.\n")

})
