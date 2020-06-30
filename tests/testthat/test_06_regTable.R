library(testthat)
library(checkmate)
library(tabshiftr)
context("regTable")


test_that("a table inventory entry is produced", {

  makeExampleDB(until = "regGeometry")

  meta_maia_1 <- makeSchema(
    list(header = list(row = 1),
         variables = list(al1 =
                            list(type = "id", col = 1),
                          year =
                            list(type = "id", col = 2),
                          commodities =
                            list(type = "id", col = 3),
                          harvested =
                            list(type = "measured", unit = "ha", factor = 1, col = 4),
                          production =
                            list(type = "measured", unit = "t", factor = 1, col = 5))))

  output <- regTable(nation = "estonia",
                     subset = "soyMaize",
                     dSeries = "madeUp",
                     gSeries = "gadm",
                     level = 1,
                     schema = meta_maia_1,
                     begin = 1990,
                     end = 2017,
                     archive = "example_table.7z|example_table1.csv",
                     archiveLink = "https://ec.europa.eu/eurostat/de/table1",
                     nextUpdate = "2019-10-01",
                     updateFrequency = "quarterly",
                     metadataLink = "https://ec.europa.eu/eurostat/de/table1/metadata",
                     update = TRUE)

  expect_tibble(x = output, nrows = 1, ncols = 13, col.names = "strict")
  expect_names(x = names(output), must.include = c("tabID", "geoID", "datID", "source_file",
                                  "schema", "orig_file", "orig_link", "download_date",
                                  "next_update", "update_frequency", "metadata_link",
                                  "metadata_path", "notes"))
  expect_file_exists(x = paste0(getOption("adb_path"), "/adb_tables/meta/schemas/schema_1.rds"))
})

test_that("function asks for details, if not provided", {

  makeExampleDB(until = "regGeometry")
  options(adb_testing = TRUE)

  meta_maia_1 <- makeSchema(
    list(header = list(row = 1),
         variables = list(al1 =
                            list(type = "id", col = 1),
                          year =
                            list(type = "id", col = 2),
                          commodities =
                            list(type = "id", col = 3),
                          harvested =
                            list(type = "measured", unit = "ha", factor = 1, col = 4),
                          production =
                            list(type = "measured", unit = "t", factor = 1, col = 5))))

  expect_message(object = regTable())
  output <- capture_messages(code = regTable())
  expect_character(x = output, len = 13, any.missing = FALSE, unique = TRUE)
  expect_equal(object = output[1], expected = "please type in to which data series this table belongs: \n")
  expect_equal(object = output[2], expected = "please type in to which geometry series this table belongs: \n")
  expect_equal(object = output[3], expected = "please type in the administrative level of the units: \n")
  expect_equal(object = output[4], expected = "please type in the first year in the table: \n")
  expect_equal(object = output[5], expected = "please type in the last year in the table: \n")
  expect_equal(object = output[6], expected = "please provide the schema description for this table: \n")
  expect_equal(object = output[7], expected = "please type in the archives' file name: \n")
  expect_equal(object = output[8], expected = "please type in the weblink from which the archive was downloaded: \n")
  expect_equal(object = output[9], expected = "please type in the frequency in which the table gets updated \n -> select one of: continual, daily, weekly, fortnightly, quarterly, biannually, annually, asNeeded, irregular, notPlanned, unknown, periodic, semimonthly, biennially: \n")
  expect_equal(object = output[10], expected = "please type in when the table gets its next update (YYYY-MM-DD): \n")
  expect_equal(object = output[11], expected = "if there is already metadata available:\n -> type in the weblink to the metadataset: \n")
  expect_equal(object = output[12], expected = "if there was an existing metadataset downloaded:\n -> type in the local path to the metadataset: \n")
})
