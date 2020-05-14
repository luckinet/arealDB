library(testthat)
library(readr)
library(magrittr)
library(checkmate)
library(tabshiftr)
context("matchUnits")


test_that("units are matched", {
  path <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)
  # unlink(paste0(path, "/newDB"), recursive = TRUE)
  setPath(root = paste0(path, "/newDB"))
  territories <- read_csv(file = paste0(path, "/id_units.csv"), col_types = "iccc")
  setVariables(input = territories, variable = "territories", pid = "anID", origin = "origin", target = "names")
  options(adb_testing = TRUE)

  regDataseries(name = "gadm",
                description = "Database of Global Administrative Areas",
                homepage = "https://gadm.org/index.html",
                licence_link = "https://gadm.org/license.html",
                update = TRUE)
  regDataseries(name = "maia",
                description = "ministerio de agricultura ganaderia y pesca",
                homepage = "http://datosestimaciones.magyp.gob.ar",
                licence_link = "http://datosestimaciones.magyp.gob.ar/license.html",
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
  file.copy(from = paste0(path, "/example_geom2.gpkg"),
            to = paste0(path, "/newDB/adb_geometries/stage2/_2__gadm.gpkg"))
  file.copy(from = paste0(path, "/example_geom3.gpkg"),
            to = paste0(path, "/newDB/adb_geometries/stage2/_3__gadm.gpkg"))
  file.copy(from = paste0(path, "/example_geom4.gpkg"),
            to = paste0(path, "/newDB/adb_geometries/stage2/arg_3__maia.gpkg"))

  # register geometries
  regGeometry(nation = "NAME_0",
              gSeries = "gadm",
              level = 1,
              layer = "example_geom1",
              nameCol = "NAME_0",
              archive = "example_geom.7z|example_geom1.gpkg",
              archiveLink = "https://gadm.org/downloads/example_geom.7z.html",
              nextUpdate = "2019-10-01",
              updateFrequency = "quarterly",
              update = TRUE)
  regGeometry(nation = "NAME_0",
              gSeries = "gadm",
              level = 2,
              layer = "example_geom2",
              nameCol = "NAME_0|NAME_1",
              archive = "example_geom.7z|example_geom2.gpkg",
              archiveLink = "https://gadm.org/downloads/example_geom.7z.html",
              nextUpdate = "2019-10-01",
              updateFrequency = "quarterly",
              update = TRUE)
  regGeometry(nation = "NAME_0",
              gSeries = "gadm",
              level = 3,
              layer = "example_geom3",
              nameCol = "NAME_0|NAME_1|NAME_2",
              archive = "example_geom.7z|example_geom3.gpkg",
              archiveLink = "https://gadm.org/downloads/example_geom.7z.html",
              nextUpdate = "2019-10-01",
              updateFrequency = "quarterly",
              update = TRUE)
  regGeometry(nation = "argentina",
              gSeries = "maia",
              level = 3,
              layer = "example_geom4",
              nameCol = "NAME_0|NAME_1|NAME_2",
              archive = "example_geom.7z|example_geom4.gpkg",
              archiveLink = "https://gadm.org/downloads/example_geom.7z.html",
              nextUpdate = "2019-10-01",
              updateFrequency = "quarterly",
              update = TRUE)

  # make schemas
  meta_maia_1 <- makeSchema(
    list(clusters = list(row = NULL, col = NULL, width = NULL, height = NULL,
                         id = NULL),
         header = list(row = 1),
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
  meta_maia_2 <- makeSchema(
    list(clusters = list(row = NULL, col = NULL, width = NULL, height = NULL,
                         id = NULL),
         header = list(row = 1),
         meta = list(dec = ".", na = c("", "NA")),
         variables = list(al1 =
                            list(type = "id", col = 1),
                          al2 =
                            list(type = "id", col = 2),
                          year =
                            list(type = "id", col = 3),
                          commodities =
                            list(type = "id", col = 4),
                          harvested =
                            list(type = "measured", unit = "ha", factor = 1, col = 5),
                          production =
                            list(type = "measured", unit = "t", factor = 1, col = 6))))

  # register data tables
  regTable(nation = "Argentina",
           subset = "soyMaize",
           dSeries = "maia", gSeries = "gadm",
           level = 1,
           begin = 1990, end = 2017,
           schema = meta_maia_1,
           archive = "example_table.7z|example_table1.csv",
           archiveLink = "https://ec.europa.eu/eurostat/de/table1",
           nextUpdate = "2019-10-01",
           updateFrequency = "quarterly",
           metadataLink = "https://ec.europa.eu/eurostat/de/table1/metadata",
           update = TRUE)
  regTable(nation = "Argentina",
           subset = "soyMaize",
           dSeries = "maia", gSeries = "gadm",
           level = 2,
           begin = 1990, end = 2017,
           schema = meta_maia_2,
           archive = "example_table.7z|example_table2.csv",
           archiveLink = "https://ec.europa.eu/eurostat/de/table2",
           nextUpdate = "2019-10-01",
           updateFrequency = "quarterly",
           metadataLink = "https://ec.europa.eu/eurostat/de/table2/metadata",
           update = TRUE)

  # normalise geometries
  normGeometry(nation = "argentina", update = TRUE)

  # test for a table that only has values at the first administrative level
  input <- read_csv(file = paste0(path, "/newDB/adb_tables/stage2/arg_1_soyMaize_1990_2017_maia.csv"),
                    col_names = FALSE) %>%
    reorganise(schema = meta_maia_1) %>%
    mutate(id = seq_along(year),
           tabID = 1,
           geoID = 1)
  output <- matchUnits(input = input, source = list("tabID" = 1))
  expect_tibble(x = output, nrows = 56, ncols = 8, col.names = "strict")
  expect_character(x = output$ahID, any.missing = FALSE)
  expect_names(x = names(output), permutation.of = c("year", "commodities", "harvested", "production", "id", "tabID", "geoID", "ahID"))

  # ... with keeping original columns
  output <- matchUnits(input = input, source = list("tabID" = 1), keepOrig = TRUE)
  expect_tibble(x = output, nrows = 56, ncols = 9, col.names = "strict")
  expect_character(x = output$ahID, any.missing = FALSE)
  expect_names(x = names(output), permutation.of = c("year", "commodities", "harvested", "production", "id", "tabID", "geoID", "al1_name", "ahID"))

  # test for a table that has values at the second administrative level
  input <- read_csv(file = paste0(path, "/newDB/adb_tables/stage2/arg_2_soyMaize_1990_2017_maia.csv"),
                    col_names = FALSE) %>%
    reorganise(schema = meta_maia_2) %>%
    mutate(id = seq_along(year),
           tabID = 1,
           geoID = 1)
  output <- matchUnits(input = input, source = list("tabID" = 1), keepOrig = TRUE)
  expect_tibble(x = output, nrows = 224, ncols = 13, col.names = "strict")
  expect_character(x = output$ahID, any.missing = FALSE)
  expect_names(x = names(output), permutation.of = c("year", "commodities", "harvested", "production", "id", "tabID", "geoID", "al1_name", "al1_id", "al2_name", "al2_id", "al3_id", "ahID"))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})