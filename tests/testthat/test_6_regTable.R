library(testthat)
library(checkmate)
context("regTable")


test_that("a table inventory entry is produced", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  options(adb_testing = TRUE)

  regDataseries(name = "test",
                description = "something",
                website = "https://",
                update = TRUE)
  file.copy(from = paste0(path, "/example_table.7z"),
            to = paste0(path, "/newDB/adb_tables/stage1/example_table.7z"))
  file.copy(from = paste0(path, "/example_table.csv"),
            to = paste0(path, "/newDB/adb_tables/stage2/arg_1_soyMaize_test_1_1990_2017.csv"))

  output <- regTable(nation = "Argentina",
                     subset = "soyMaize",
                     dSeries = "test", gSeries = "test",
                     level = 1,
                     algo = 1,
                     begin = 1990, end = 2017,
                     archive = "example_table.7z|example_table.csv",
                     update = TRUE)

  expect_tibble(x = output, nrows = 1, ncols = 7, col.names = "strict")
  expect_names(x = names(output), must.include = c("tabID", "geoID", "datID", "source_file", "date", "orig_file", "notes"))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

test_that("function asks for details, if not provided", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  options(adb_testing = TRUE)

  regDataseries(name = "test",
                description = "something",
                website = "https://",
                update = TRUE)
  file.copy(from = paste0(path, "/example_table.7z"),
            to = paste0(path, "/newDB/adb_tables/stage1/example_table.7z"))
  file.copy(from = paste0(path, "/example_table.csv"),
            to = paste0(path, "/newDB/adb_tables/stage2/_1__test_1_1990_2019.csv"))

  expect_message(object = regTable())
  output <- capture_messages(code = regTable())
  expect_character(x = output, len = 7, any.missing = FALSE, unique = TRUE)
  expect_equal(object = output[1], expected = "please type in to which data series this table belongs: \n")
  expect_equal(object = output[2], expected = "please type in to which geometry series this table belongs: \n")
  expect_equal(object = output[3], expected = "please type in the administrative level of the units: \n")
  expect_equal(object = output[4], expected = "please type in the integer identifying this tables' schema description: \n")
  expect_equal(object = output[5], expected = "please type in the first year in the table: \n")
  expect_equal(object = output[6], expected = "please type in the last year in the table: \n")
  expect_equal(object = output[7], expected = "please type in the archives' file name: \n")

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

test_that("Error if arguments have wrong value", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  options(adb_testing = TRUE)

  regDataseries(name = "test",
                description = "something",
                website = "https://",
                update = TRUE)
  file.copy(from = paste0(path, "/example_table.7z"),
            to = paste0(path, "/newDB/adb_tables/stage1/example_table.7z"))
  file.copy(from = paste0(path, "/example_table.csv"),
            to = paste0(path, "/newDB/adb_tables/stage2/arg_1_soyMaize_test_1_1990_2017.csv"))

  expect_error(regTable(update = 1))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})