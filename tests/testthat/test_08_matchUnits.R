context("matchUnits")


test_that("", {
  path <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))
  territories <- read_csv(file = paste0(path, "/id_units.csv"), col_types = "icc")
  setVariables(input = territories, variable = "territories", pid = "anID", target = "names")
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
  file.copy(from = paste0(path, "/example_table1.csv"),
            to = paste0(path, "/newDB/adb_tables/stage2/arg_1_soyMaize_maia_1_1990_2017.csv"))
  file.copy(from = paste0(path, "/example_table2.csv"),
            to = paste0(path, "/newDB/adb_tables/stage2/arg_2_soyMaize_maia_2_1990_2017.csv"))
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

  meta_maia_1 <<- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
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
  meta_maia_2 <<- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                                       id = NULL),
                       variables = list(territories =
                                          list(type = "id", name = "al1", form = "long",
                                               row = NULL, col = 1, rel = FALSE),
                                        province =
                                          list(type = "id", name = "al2", form = "long",
                                               row = NULL, col = 2, rel = FALSE),
                                        period =
                                          list(type = "id", name = "year", form = "long",
                                               row = NULL, col = 3, rel = FALSE),
                                        commodities =
                                          list(type = "id", name = NULL, form = "long",
                                               row = NULL, col = 4, rel = FALSE),
                                        harvested =
                                          list(type = "values", unit = "ha", factor = 1,
                                               row = NULL, col = 5, rel = FALSE,
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
           archive = "example_table.7z|example_table1.csv",
           update = TRUE)
  regTable(nation = "Argentina",
           subset = "soyMaize",
           dSeries = "maia", gSeries = "gadm",
           level = 2,
           begin = 1990, end = 2017,
           archive = "example_table.7z|example_table2.csv",
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

  # test for a table that only has values at the first administrative level
  input <- read_csv(file = paste0(path, "/newDB/adb_tables/stage2/arg_1_soyMaize_maia_1_1990_2017.csv"),
                    col_names = FALSE) %>%
    reorganise(schema = meta_maia_1) %>%
    mutate(id = seq_along(year),
           tabID = 1,
           geoID = 1)
  output <- matchUnits(input = input, source = 1)
  expect_tibble(x = output, nrows = 56, ncols = 8, col.names = "strict")
  expect_character(x = output$ahID, any.missing = FALSE)
  expect_names(x = names(output), permutation.of = c("year", "commodities", "harvested", "production", "id", "tabID", "geoID", "ahID"))

  # ... with keeping original columns
  output <- matchUnits(input = input, source = 1, keepOrig = TRUE)
  expect_tibble(x = output, nrows = 56, ncols = 9, col.names = "strict")
  expect_character(x = output$ahID, any.missing = FALSE)
  expect_names(x = names(output), permutation.of = c("al1_alt", "year", "commodities", "harvested", "production", "id", "tabID", "geoID", "ahID"))

  # test for a table that has values at the second administrative level
  input <- read_csv(file = paste0(path, "/newDB/adb_tables/stage2/arg_2_soyMaize_maia_2_1990_2017.csv"),
                    col_names = FALSE) %>%
    reorganise(schema = meta_maia_2) %>%
    mutate(id = seq_along(year),
           tabID = 1,
           geoID = 1)
  output <- matchUnits(input = input, source = 1, keepOrig = TRUE)
  expect_tibble(x = output, nrows = 224, ncols = 10, col.names = "strict")
  expect_character(x = output$ahID, any.missing = FALSE)
  expect_names(x = names(output), permutation.of = c("al1_alt", "al2_alt", "year", "commodities", "harvested", "production", "id", "tabID", "geoID", "ahID"))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

test_that("Error if arguments have wrong value", {
  # path <- system.file("test_data", package="DMT", mustWork = TRUE)
  # registerTables(root = paste0(path, "/"))
  # input <- tibble(al1 = c("Brazil", "Bolivia", "Argentina"))
  # false_input <- tibble(al = c("Brazil", "Bolivia", "Argentina"))
  #
  # expect_error(matchAdminUnits(units = "bla"))
  # expect_error(matchAdminUnits(units = list(input)))
  # expect_error(matchAdminUnits(units = false_input))
  # expect_error(matchAdminUnits(units = input, level = "bla"))
  # expect_error(matchAdminUnits(units = input, bla = "blubb"))
})