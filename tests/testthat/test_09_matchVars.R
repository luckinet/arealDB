library(testthat)
library(rectifyr)
library(checkmate)
context("matchVars")

test_that("", {
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
              archive = "example_geom.7z|example_geom2.gpkg",archiveLink = "https://gadm.org/downloads/example_geom.7z.html",
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
    list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                         id = NULL),
         header = list(row = 1, rel = FALSE),
         meta = list(dec = ".", na = c("", "NA"), types = NULL),
         variables = list(al1 =
                            list(type = "id", name = "territories", split = NULL,
                                 row = NULL, col = 1, rel = FALSE),
                          year =
                            list(type = "id", name = "period", split = NULL,
                                 row = NULL, col = 2, rel = FALSE),
                          commodities =
                            list(type = "id", name = NULL, split = NULL,
                                 row = NULL, col = 3, rel = FALSE),
                          harvested =
                            list(type = "values", unit = "ha", factor = 1,
                                 row = NULL, col = 4, rel = FALSE,
                                 key = NULL, value = NULL),
                          production =
                            list(type = "values", unit = "t", factor = 1,
                                 row = NULL, col = 5, rel = FALSE,
                                 key = NULL, value = NULL))))

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

  input <- read_csv(file = paste0(path, "/newDB/adb_tables/stage2/arg_1_soyMaize_1990_2017_maia.csv"),
                    col_names = FALSE) %>%
    reorganise(schema = meta_maia_1) %>%
    mutate(id = seq_along(year),
           tabID = 1,
           geoID = 1) %>%
    matchUnits(source = 1, keepOrig = FALSE)

  # discard output
  output <- matchVars(input = input, faoID = list(commodities = "target"),
                      source = 1, keepOrig = FALSE)
  expect_tibble(x = output, nrows = 56, ncols = 8, col.names = "strict")
  expect_double(x = output$faoID, any.missing = FALSE)
  expect_names(x = names(output), permutation.of = c("year", "harvested", "production", "id", "tabID", "geoID", "ahID", "faoID"))

  # keep output
  output <- matchVars(input = input, faoID = list(commodities = "target"),
                      source = 1, keepOrig = TRUE)
  expect_tibble(x = output, nrows = 56, ncols = 9, col.names = "strict")
  expect_double(x = output$faoID, any.missing = FALSE)
  expect_names(x = names(output), permutation.of = c("year", "commodities", "harvested", "production", "id", "tabID", "geoID", "ahID", "faoID"))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

