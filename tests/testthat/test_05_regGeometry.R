library(testthat)
library(checkmate)
context("regGeometry")


test_that("a geometry inventory entry is produced", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  options(adb_testing = TRUE)

  regDataseries(name = "gadm",
                description = "Database of Global Administrative Areas",
                homepage = "https://gadm.org/index.html",
                update = TRUE)
  file.copy(from = paste0(path, "/example_geom.7z"),
            to = paste0(path, "/newDB/adb_geometries/stage1/example_geom.7z"))
  file.copy(from = paste0(path, "/example_geom1.gpkg"),
            to = paste0(path, "/newDB/adb_geometries/stage2/_1__gadm.gpkg"))

  output <- regGeometry(nation = "NAME_0",
                        gSeries = "gadm",
                        level = 1,
                        layer = "example_geom1",
                        nameCol = "NAME_0",
                        archive = "example_geom.7z|example_geom1.gpkg",
                        archiveLink = "https://gadm.org/downloads/example_geom.7z.html",
                        nextUpdate = "2019-10-01",
                        updateFrequency = "quarterly",
                        update = TRUE)

  expect_tibble(x = output, nrows = 1, ncols = 13, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "level", "source_file",
                        "layer", "nation_column", "unit_column", "orig_file", "orig_link",
                        "download_date", "next_update", "update_frequency", "notes"))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

test_that("function asks for details, if not provided", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  options(adb_testing = TRUE)

  regDataseries(name = "gadm",
                description = "Database of Global Administrative Areas",
                homepage = "https://gadm.org/index.html",
                update = TRUE)
  file.copy(from = paste0(path, "/example_geom.7z"),
            to = paste0(path, "/newDB/adb_geometries/stage1/example_geom.7z"))
  file.copy(from = paste0(path, "/example_geom1.gpkg"),
            to = paste0(path, "/newDB/adb_geometries/stage2/arg_1__gadm.gpkg"))

  expect_message(object = regGeometry())
  output <- capture_messages(code = regGeometry())
  expect_character(x = output, len = 8, any.missing = FALSE, unique = TRUE)
  expect_equal(object = output[1], expected = "please type in either a nation or the name of the column that contains nation names: \n")
  expect_equal(object = output[2], expected = "please type in the name of the column that contains unit names: \n")
  expect_equal(object = output[3], expected = "please type in to which series the geometry belongs: \n")
  expect_equal(object = output[4], expected = "please type in the administrative level of the units: \n")
  expect_equal(object = output[5], expected = "please type in the archives' file name: \n")
  expect_equal(object = output[6], expected = "please type in the weblink from which the archive was downloaded: \n")
  expect_equal(object = output[7], expected = "please type in the frequency in which the table gets updated ...\n")
  expect_equal(object = output[8], expected = "please type in when the geometry gets its next update (YYYY-MM-DD): \n")

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

test_that("errors show up if arguments have wrong value", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  options(adb_testing = TRUE)

  regDataseries(name = "gadm",
                description = "Database of Global Administrative Areas",
                homepage = "https://gadm.org/index.html",
                update = TRUE)
  file.copy(from = paste0(path, "/example_geom.7z"),
            to = paste0(path, "/newDB/adb_geometries/stage1/example_geom.7z"))
  file.copy(from = paste0(path, "/example_geom.gpkg"),
            to = paste0(path, "/newDB/adb_geometries/stage2/arg_1__gadm.gpkg"))

  expect_error(regGeometry(update = 1))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})
