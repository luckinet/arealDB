context("matchUnits")


test_that("", {
  path <- system.file("test_datasets", package="arealDB", mustWork = TRUE)
  setPath(root = paste0(path, "/newDB"))

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