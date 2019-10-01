context("normTable")


test_that("", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  options(adb_testing = TRUE)

  regDataseries(name = "gadm",
                description = "Database of Global Administrative Areas",
                website = "https://gadm.org/index.html",
                update = TRUE)
  regDataseries(name = "maia",
                description = "ministerio de agricultura ganaderia y pesca",
                website = "http://datosestimaciones.magyp.gob.ar",
                update = TRUE)
  file.copy(from = paste0(path, "/example_table.7z"),
            to = paste0(path, "/newDB/adb_tables/stage1/example_table.7z"))
  file.copy(from = paste0(path, "/example_table.csv"),
            to = paste0(path, "/newDB/adb_tables/stage2/arg_1_soyMaize_maia_1_1990_2017.csv"))
  file.copy(from = paste0(path, "/example_geom.7z"),
            to = paste0(path, "/newDB/adb_geometries/stage1/example_geom.7z"))
  file.copy(from = paste0(path, "/example_geom1.gpkg"),
            to = paste0(path, "/newDB/adb_geometries/stage2/_1__gadm.gpkg"))
  file.copy(from = paste0(path, "/example_geom2.gpkg"),
            to = paste0(path, "/newDB/adb_geometries/stage2/_2__gadm.gpkg"))
  file.copy(from = paste0(path, "/example_geom3.gpkg"),
            to = paste0(path, "/newDB/adb_geometries/stage2/_3__gadm.gpkg"))
  file.copy(from = paste0(path, "/example_geom3.gpkg"),
            to = paste0(path, "/newDB/adb_geometries/stage2/arg_3__maia.gpkg"))

  meta_maia_1 <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                                      id = NULL),
                      variables = list(territories =
                                         list(type = "id", name = "al1", form = "long",
                                              row = NULL, col = 1, rel = FALSE),
                                       period =
                                         list(type = "id", name = "year", form = "long",
                                              row = NULL, col = 2, rel = FALSE),
                                       commodities =
                                         list(type = "id", name = NULL, form = "long",
                                              row = NULL, col = 3, rel = FALSE),
                                       harvested =
                                         list(type = "values", unit = "ha", factor = 1,
                                              row = NULL, col = 4, rel = FALSE,
                                              id = NULL, level = NULL),
                                       production =
                                         list(type = "values", unit = "t", factor = 1,
                                              row = NULL, col = 5, rel = FALSE,
                                              id = NULL, level = NULL)))

  regTable(nation = "Argentina",
           subset = "soyMaize",
           dSeries = "maia", gSeries = "gadm",
           level = 1,
           begin = 1990, end = 2017,
           archive = "example_table.7z|example_table.csv",
           update = TRUE)
  regGeometry(nation = "NAME_0",
              gSeries = "gadm",
              level = 1,
              layer = "example_geom1",
              nameCol = "NAME_0",
              archive = "example_geom.7z|example_geom1.gpkg",
              update = TRUE)
  regGeometry(nation = "NAME_0",
              gSeries = "gadm",
              level = 2,
              layer = "example_geom2",
              nameCol = "NAME_0|NAME_1",
              archive = "example_geom.7z|example_geom2.gpkg",
              update = TRUE)
  regGeometry(nation = "NAME_0",
              gSeries = "gadm",
              level = 3,
              layer = "example_geom3",
              nameCol = "NAME_0|NAME_1|NAME_2",
              archive = "example_geom.7z|example_geom3.gpkg",
              update = TRUE)
  regGeometry(nation = "argentina",
              gSeries = "maia",
              level = 3,
              layer = "example_geom4",
              nameCol = "NAME_0|NAME_1|NAME_2",
              archive = "example_geom.7z|example_geom4.gpkg",
              update = TRUE)
  normGeometry(nation = "argentina", update = TRUE)

  # output <- normTable(input = paste0(path, "/newDB/adb_tables/stage2/arg_1_soyMaize_maia_1_1990_2017.csv"))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

test_that("Error if arguments have wrong value", {

})