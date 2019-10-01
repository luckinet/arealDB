context("matchUnits")


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

  input <- read_csv(file = paste0(path, "/newDB/adb_tables/stage2/arg_1_soyMaize_maia_1_1990_2017.csv"),
                    col_names = FALSE) %>%
    reorganise(schema = meta_maia_1) %>%
    mutate(id = seq_along(year),
           tabID = 1,
           geoID = 1)
  output <- matchUnits(input = input, source = 1)

  expect_tibble(x = output, nrows = 56, ncols = 8, col.names = "strict")
  expect_names(x = names(output), permutation.of = c("year", "commodities", "harvested", "production", "id", "tabID", "geoID", "ahID"))

  output <- matchUnits(input = input, source = 1, keepOrig = TRUE)

  expect_tibble(x = output, nrows = 56, ncols = 9, col.names = "strict")
  expect_names(x = names(output), permutation.of = c("al1_alt", "year", "commodities", "harvested", "production", "id", "tabID", "geoID", "ahID"))

  unlink(paste0(path, "/newDB"), recursive = TRUE)
})

test_that("match simple vector of already known units", {
  # this tests whether the function works at its most basic level
  # path <- system.file("test_data", package="DMT", mustWork = TRUE)
  # registerTables(root = paste0(path, "/"))
  # input <- tibble(al1 = c("Brazil", "Bolivia", "Argentina"))
  # out <- matchAdminUnits(units = input)
  #
  # expect_integerish(x = out, any.missing = FALSE, len = 3)
  #
  # restoreIndex(pattern = "id_adminUnits", select = "oldest")
  # restoreIndex(pattern = "hierarchy_adminUnits", select = "oldest")
})

test_that("match simple vector partly containing unknown units", {
  # this tests whether unknown units are recognised and properly inserted into
  # the respective tables
  # path <- system.file("test_data", package="DMT", mustWork = TRUE)
  # registerTables(root = paste0(path, "/"))
  # input <- tibble(al1 = c("Estonia", "Bolivia", "Germany"))
  # out <- matchAdminUnits(units = input)
  #
  # expect_integerish(x = out, any.missing = FALSE, len = 3)
  # expect_true(object = all(c("Estonia", "Germany") %in% id_adminUnits$name))
  # expect_true(object = all(c(185, 186) %in% hierarchy_adminUnits$al1_id))
  #
  # restoreIndex(pattern = "id_sourceGeom", select = "oldest")
  # restoreIndex(pattern = "id_adminUnits", select = "oldest")
  # restoreIndex(pattern = "hierarchy_adminUnits", select = "oldest")
})

test_that("match units that have 'parents'", {
  # this tests whether units that are only distinguished due to their parent
  # (i.e. the unit one level above) are also processed correctly
  # path <- system.file("test_data", package="DMT", mustWork = TRUE)
  # registerTables(root = paste0(path, "/"))
  # input <- tibble(al2 = c("Buenos Aires", "San Luis", "Río Negro", "Buenos Aires", "Chaco"),
  #                 al3 = c("Adolfo Alsina", "Chacabuco", "Adolfo Alsina", "Chacabuco", "Chacabuco"))
  # out <- matchAdminUnits(units = input, level = 3)
  #
  # expect_integerish(x = out, any.missing = FALSE, len = 5)
  #
  # restoreIndex(pattern = "id_adminUnits", select = "oldest")
  # restoreIndex(pattern = "hierarchy_adminUnits", select = "oldest")
})

test_that("subsetting with ...", {
  # this tests whether a subset can be properly defined
  # path <- system.file("test_data", package="DMT", mustWork = TRUE)
  # registerTables(root = paste0(path, "/"))
  # input <- tibble(al3 = c("Belisario Boeto", "Hernando Siles", "Jaime Zudáñez",
  #                         "Juana Azurduay de Padilla", "Luis Calvo", "Nor Cinti",
  #                         "Oropeza", "Sud Cinti", "Tomina", "Yamparáez", "Arani",
  #                         "Arque", "Ayopaya", "Capinota"))
  # out <- matchAdminUnits(units = input, level = 3, al1 = "Bolivia")
  #
  # expect_integerish(x = out, any.missing = FALSE, len = 14)
  #
  # restoreIndex(pattern = "id_adminUnits", select = "oldest")
  # restoreIndex(pattern = "hierarchy_adminUnits", select = "oldest")
  # file.remove(list.files(path = paste0(path, "/log/"), full.names = T))
})

test_that("match units that are not valid for all years", {
  # this tests whether a unit that is not valid for a set of years is processed
  # correctly
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