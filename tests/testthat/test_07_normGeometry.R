library(sf)
library(testthat)
library(checkmate)
context("normGeometry")

test_that("geometries can be normalised", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  territories <- read_csv(file = paste0(path, "/id_units.csv"), col_types = "iccc")
  setVariables(input = territories, variable = "territories", pid = "anID", target = "names")
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

  # set up three levels of polygons ----
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
  output <- normGeometry(input = paste0(path, "/newDB/adb_geometries/stage2/_1__gadm.gpkg"), update = TRUE)
  expect_tibble(x = output, nrows = 1, ncols = 13, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "level", "source_file",
                        "layer", "nation_column", "unit_column", "orig_file", "orig_link",
                        "download_date", "next_update", "update_frequency", "notes"))
  expect_file_exists(x = paste0(path, "/newDB/adb_geometries/stage2/processed/_1__gadm.gpkg"))

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
  output <- normGeometry(input = paste0(path, "/newDB/adb_geometries/stage2/_2__gadm.gpkg"), update = TRUE)
  expect_tibble(x = output, nrows = 1, ncols = 13, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "level", "source_file",
                        "layer", "nation_column", "unit_column", "orig_file", "orig_link",
                        "download_date", "next_update", "update_frequency", "notes"))
  expect_file_exists(x = paste0(path, "/newDB/adb_geometries/stage2/processed/_2__gadm.gpkg"))

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
  output <- normGeometry(input = paste0(path, "/newDB/adb_geometries/stage2/_3__gadm.gpkg"), update = TRUE)
  expect_tibble(x = output, nrows = 1, ncols = 13, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "level", "source_file",
                        "layer", "nation_column", "unit_column", "orig_file", "orig_link",
                        "download_date", "next_update", "update_frequency", "notes"))
  expect_file_exists(x = paste0(path, "/newDB/adb_geometries/stage2/processed/_3__gadm.gpkg"))

  # also include a second dataset, that has to be attached to al3 ----
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
  output <- normGeometry(input = paste0(path, "/newDB/adb_geometries/stage2/arg_3__maia.gpkg"), nation = "argentina", update = TRUE)
  expect_tibble(x = output, nrows = 1, ncols = 13, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "level", "source_file",
                        "layer", "nation_column", "unit_column", "orig_file", "orig_link",
                        "download_date", "next_update", "update_frequency", "notes"))

  # test whether the resulting file is "correct" ----
  final <- st_read(dsn = paste0(path, "/newDB/adb_geometries/stage3/argentina.gpkg"), layer = "level_1")
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 1, ncols = 7)
  expect_names(x = names(final), identical.to = c("nation", "name", "level", "ahID", "geoID", "al1_id", "geom"))

  final <- st_read(dsn = paste0(path, "/newDB/adb_geometries/stage3/argentina.gpkg"), layer = "level_2")
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 4, ncols = 8)
  expect_names(x = names(final), identical.to = c("nation", "name", "level", "ahID", "geoID", "al1_id", "al2_id", "geom"))

  final <- st_read(dsn = paste0(path, "/newDB/adb_geometries/stage3/argentina.gpkg"), layer = "level_3")
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 12, ncols = 9)
  expect_names(x = names(final), identical.to = c("nation", "name", "level", "ahID", "geoID", "al1_id", "al2_id", "al3_id", "geom"))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})
