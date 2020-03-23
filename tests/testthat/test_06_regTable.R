library(testthat)
library(checkmate)
library(tabshiftr)
context("regTable")


test_that("a table inventory entry is produced", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  options(adb_testing = TRUE)

  regDataseries(name = "gadm",
                description = "Database of Global Administrative Areas",
                homepage = "https://gadm.org/index.html",
                licence_link = "https://gadm.org/license.html",
                licence_path = "C:/Users/arue/Projects/GeoKur/Luckinet/licenceFiles/licence.txt",
                update = TRUE)
  regDataseries(name = "maia",
                description = "ministerio de agricultura ganaderia y pesca",
                homepage = "http://datosestimaciones.magyp.gob.ar",
                licence_link = "http://datosestimaciones.magyp.gob.ar/license.html",
                licence_path = "C:/Users/arue/Projects/GeoKur/Luckinet/licenceFiles/licence2.txt",
                update = TRUE)
  file.copy(from = paste0(path, "/example_table.7z"),
            to = paste0(path, "/newDB/adb_tables/stage1/example_table.7z"))
  file.copy(from = paste0(path, "/example_table1.csv"),
            to = paste0(path, "/newDB/adb_tables/stage2/arg_1_soyMaize_1990_2017_maia.csv"))
  file.copy(from = paste0(path, "/example_table2.csv"),
            to = paste0(path, "/newDB/adb_tables/stage2/arg_2_soyMaize_1990_2017_maia.csv"))
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

  meta_maia_1 <- makeSchema(list(
    clusters = list(row = NULL, col = NULL, width = NULL, height = NULL,
                    id = NULL),
    header = list(row = 1),
    variables = list(
      territories =
        list(type = "id", col = 1),
      period =
        list(type = "id", col = 2),
      commodities =
        list(type = "id", col = 3),
      harvested =
        list(type = "measured", unit = "ha", factor = 1, col = 4),
      production =
        list(type = "measured", unit = "t", factor = 1, col = 5))))

  output <- regTable(nation = "Argentina",
                     subset = "soyMaize",
                     dSeries = "maia",
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
                     metadataPath = "C:/Users/arue/Projects/GeoKur/Luckinet/census/table1_meta.txt",
                     update = TRUE)

  expect_tibble(x = output, nrows = 1, ncols = 13, col.names = "strict")
  expect_names(x = names(output), must.include = c("tabID", "geoID", "datID", "source_file",
                                  "schema", "orig_file", "orig_link", "download_date",
                                  "next_update", "update_frequency", "metadata_link",
                                  "metadata_path", "notes"))
  expect_file_exists(x = paste0(path, "/newDB/adb_tables/meta/schemas/schema_1.rds"))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

test_that("function asks for details, if not provided", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  options(adb_testing = TRUE)

  regDataseries(name = "gadm",
                description = "Database of Global Administrative Areas",
                homepage = "https://gadm.org/index.html",
                licence_link = "https://gadm.org/license.html",
                licence_path = "C:/Users/arue/Projects/GeoKur/Luckinet/licenceFiles/licence.txt",
                update = TRUE)
  regDataseries(name = "maia",
                description = "ministerio de agricultura ganaderia y pesca",
                homepage = "http://datosestimaciones.magyp.gob.ar",
                licence_link = "http://datosestimaciones.magyp.gob.ar/licence.html",
                licence_path = "C:/Users/arue/Projects/GeoKur/Luckinet/licenceFiles/licence2.txt",
                update = TRUE)
  file.copy(from = paste0(path, "/example_table.7z"),
            to = paste0(path, "/newDB/adb_tables/stage1/example_table.7z"))
  file.copy(from = paste0(path, "/example_table1.csv"),
            to = paste0(path, "/newDB/adb_tables/stage2/_1__1990_2019_maia.csv"))
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

  meta_maia_1 <- makeSchema(list(
    clusters = list(row = NULL, col = NULL, width = NULL, height = NULL,
                    id = NULL),
    header = list(row = 1),
    variables = list(
      territories =
        list(type = "id", col = 1),
      period =
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

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

test_that("Error if arguments have wrong value", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  options(adb_testing = TRUE)

  regDataseries(name = "gadm",
                description = "Database of Global Administrative Areas",
                homepage = "https://gadm.org/index.html",
                licence_link = "https://gadm.org/license.html",
                licence_path = "C:/Users/arue/Projects/GeoKur/Luckinet/licenceFiles/licence.txt",
                update = TRUE)
  file.copy(from = paste0(path, "/example_table.7z"),
            to = paste0(path, "/newDB/adb_tables/stage1/example_table.7z"))
  file.copy(from = paste0(path, "/example_table1.csv"),
            to = paste0(path, "/newDB/adb_tables/stage2/arg_1_soyMaize_1990_2017_maia.csv"))

  expect_error(regTable())
  expect_error(regTable(update = 1))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})
