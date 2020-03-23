library(testthat)
library(tabshiftr)
library(readr)
library(magrittr)
library(checkmate)
context("normTable")


test_that("tables can be normalised (without matched variables)", {
  path <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  territories <- read_csv(file = paste0(path, "/id_units.csv"), col_types = "iccc")
  setVariables(input = territories, variable = "territories", pid = "anID", origin = "origin", target = "names")
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

  regTable(nation = "Argentina",
           subset = "soyMaize",
           dSeries = "maia", gSeries = "gadm",
           schema = meta_maia_1,
           level = 1,
           begin = 1990, end = 2017,
           archive = "example_table.7z|example_table.csv",
           archiveLink = "https://ec.europa.eu/eurostat/de/table",
           nextUpdate = "2019-10-01",
           updateFrequency = "quarterly",
           metadataLink = "https://ec.europa.eu/eurostat/de/table/metadata",
           metadataPath = "C:/Users/arue/Projects/GeoKur/Luckinet/census/table_meta.txt",
           update = TRUE)

  normGeometry(nation = "argentina", update = TRUE)

  output <- normTable(input = paste0(path, "/newDB/adb_tables/stage2/arg_1_soyMaize_1990_2017_maia.csv"),
                      update = TRUE)
  expect_file_exists(x = paste0(path, "/newDB/adb_tables/stage2/processed/arg_1_soyMaize_1990_2017_maia.csv"))

  # test whether the resulting file is "correct" ----
  final <- read_csv(file = paste0(path, "/newDB/adb_tables/stage3/argentina.csv"))
  expect_tibble(x = final, types = c("double", "double", "double", "character", "double", "character", "double", "double"))
  expect_data_frame(x = final, nrows = 56, ncols = 8)
  expect_names(x = names(final), identical.to = c("id", "tabID", "geoID", "ahID", "year", "commodities", "harvested", "production"))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

test_that("tables can be normalised (with matched variables)", {
  path <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  territories <- read_csv(file = paste0(path, "/id_units.csv"), col_types = "iccc")
  setVariables(input = territories, variable = "territories", pid = "anID", origin = "origin", target = "names")
  comm <- read_csv(file = paste0(path, "/id_commodities.csv"), col_types = "iccc")
  setVariables(input = comm, variable = "commodities", pid = "faoID", target = "simpleName")
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

  regTable(nation = "Argentina",
           subset = "soyMaize",
           dSeries = "maia", gSeries = "gadm",
           schema = meta_maia_1,
           level = 1,
           begin = 1990, end = 2017,
           archive = "example_table.7z|example_table.csv",
           archiveLink = "https://ec.europa.eu/eurostat/de/table",
           nextUpdate = "2019-10-01",
           updateFrequency = "quarterly",
           metadataLink = "https://ec.europa.eu/eurostat/de/table/metadata",
           metadataPath = "C:/Users/arue/Projects/GeoKur/Luckinet/census/table_meta.txt",
           update = TRUE)

  normGeometry(nation = "argentina", update = TRUE)

  output <- normTable(input = paste0(path, "/newDB/adb_tables/stage2/arg_1_soyMaize_1990_2017_maia.csv"),
                      faoID = list(commodities = "target"),
                      update = TRUE)
  expect_file_exists(x = paste0(path, "/newDB/adb_tables/stage2/processed/arg_1_soyMaize_1990_2017_maia.csv"))

  # test whether the resulting file is "correct" ----
  final <- read_csv(file = paste0(path, "/newDB/adb_tables/stage3/argentina.csv"))
  expect_tibble(x = final, types = c("double", "double", "double", "character", "double", "character", "double", "double", "double"))
  expect_data_frame(x = final, nrows = 56, ncols = 8)
  expect_names(x = names(final), identical.to = c("id", "tabID", "geoID", "ahID", "year", "harvested", "production", "faoID"))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})
