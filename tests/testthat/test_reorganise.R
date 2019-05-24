library(testthat)
library(readr)
library(magrittr)
library(checkmate)
context("reorganise")


test_that("recognise several vertical clusters of otherwise tidy data", {
  algo <- list(clusters = list(top = c(2, 9), left = 1, width = NULL, height = NULL,
                               id = "territories"),
               variables = list(territories =
                                  list(type = "id", name = NULL, form = "long",
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
                                       id = NULL, value = NULL),
                                production =
                                  list(type = "values", unit = "t", factor = 1,
                                       row = NULL, col = 5, rel = FALSE,
                                       id = NULL, value = NULL)))

  input <- read_csv(paste0(system.file("test_datasets", package="censusTools"), "/table1.csv"),
                    col_names = FALSE, col_types = "ccccc") %>%
    record(what = "algorithm", name = "algo") %>%
    reorganise()

})

test_that("recognise several horizontal clusters of otherwise tidy data", {
  algo <- list(clusters = list(top = 2, left = c(2, 5), width = NULL, height = NULL,
                               id = "territories"),
               variables = list(territories =
                                  list(type = "id", name = NULL, form = "wide",
                                       row = 2, col = NULL, rel = FALSE),
                                period =
                                  list(type = "id", name = "year", form = "long",
                                       row = NULL, col = 1, rel = FALSE),
                                commodities =
                                  list(type = "id", name = NULL, form = "long",
                                       row = NULL, col = c(2, 5), rel = FALSE),
                                harvested =
                                  list(type = "values", unit = "ha", factor = 1,
                                       row = NULL, col = c(3, 6), rel = FALSE,
                                       id = NULL, value = NULL),
                                production =
                                  list(type = "values", unit = "t", factor = 1,
                                       row = NULL, col = c(4, 7), rel = FALSE,
                                       id = NULL, value = NULL)))

  input <- read_csv(paste0(system.file("test_datasets", package="censusTools"), "/table2.csv"),
                    col_names = FALSE) %>%
    record(what = "algorithm", name = "algo") %>%
    reorganise()

})

test_that("rename variables, in wide table", {
  algo <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                               id = NULL),
               variables = list(territories =
                                  list(type = "id", name = NULL, form = "long",
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
                                       id = NULL, value = NULL),
                                production =
                                  list(type = "values", unit = "t", factor = 1,
                                       row = NULL, col = 5, rel = FALSE,
                                       id = NULL, value = NULL)))

  input <- read_csv(paste0(system.file("test_datasets", package="censusTools"), "/table3.csv"),
                    col_names = FALSE) %>%
    record(what = "algorithm", name = "algo") %>%
    reorganise()

})

test_that("spread long table", {
  algo <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                               id = "territories"),
               variables = list(territories =
                                  list(type = "id", name = NULL, form = "long",
                                       row = NULL, col = NULL, rel = FALSE),
                                period =
                                  list(type = "id", name = "year", form = "long",
                                       row = NULL, col = NULL, rel = FALSE),
                                commodities =
                                  list(type = "id", name = NULL, form = "long",
                                       row = NULL, col = NULL, rel = FALSE),
                                harvested =
                                  list(type = "values", unit = "ha", factor = 1,
                                       row = NULL, col = NULL, rel = FALSE,
                                       id = NULL, value = NULL),
                                production =
                                  list(type = "values", unit = "t", factor = 1,
                                       row = NULL, col = NULL, rel = FALSE,
                                       id = NULL, value = NULL)))

  input <- read_csv(paste0(system.file("test_datasets", package="censusTools"), "/table4.csv"),
                    col_names = FALSE) %>%
    record(what = "algorithm", name = "algo") %>%
    reorganise()

})

test_that("bring wide identifying variable into long form", {
  algo <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                               id = "territories"),
               variables = list(territories =
                                  list(type = "id", name = NULL, form = "long",
                                       row = NULL, col = NULL, rel = FALSE),
                                period =
                                  list(type = "id", name = "year", form = "long",
                                       row = NULL, col = NULL, rel = FALSE),
                                commodities =
                                  list(type = "id", name = NULL, form = "long",
                                       row = NULL, col = NULL, rel = FALSE),
                                harvested =
                                  list(type = "values", unit = "ha", factor = 1,
                                       row = NULL, col = NULL, rel = FALSE,
                                       id = NULL, value = NULL),
                                production =
                                  list(type = "values", unit = "t", factor = 1,
                                       row = NULL, col = NULL, rel = FALSE,
                                       id = NULL, value = NULL)))

  input <- read_csv(paste0(system.file("test_datasets", package="censusTools"), "/table5.csv"),
                    col_names = FALSE) %>%
    record(what = "algorithm", name = "algo") %>%
    reorganise()

})

test_that("bring wide identifying variable into long form and spread long table", {
  algo <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                               id = "territories"),
               variables = list(territories =
                                  list(type = "id", name = NULL, form = "long",
                                       row = NULL, col = NULL, rel = FALSE),
                                period =
                                  list(type = "id", name = "year", form = "long",
                                       row = NULL, col = NULL, rel = FALSE),
                                commodities =
                                  list(type = "id", name = NULL, form = "long",
                                       row = NULL, col = NULL, rel = FALSE),
                                harvested =
                                  list(type = "values", unit = "ha", factor = 1,
                                       row = NULL, col = NULL, rel = FALSE,
                                       id = NULL, value = NULL),
                                production =
                                  list(type = "values", unit = "t", factor = 1,
                                       row = NULL, col = NULL, rel = FALSE,
                                       id = NULL, value = NULL)))

  input <- read_csv(paste0(system.file("test_datasets", package="censusTools"), "/table6.csv"),
                    col_names = FALSE) %>%
    record(what = "algorithm", name = "algo") %>%
    reorganise()

})

test_that("bring several wide identifying variables into long form", {
  algo <- list(clusters = list(top = NULL, left = NULL, width = NULL, height = NULL,
                               id = "territories"),
               variables = list(territories =
                                  list(type = "id", name = NULL, form = "long",
                                       row = NULL, col = NULL, rel = FALSE),
                                period =
                                  list(type = "id", name = "year", form = "long",
                                       row = NULL, col = NULL, rel = FALSE),
                                commodities =
                                  list(type = "id", name = NULL, form = "long",
                                       row = NULL, col = NULL, rel = FALSE),
                                harvested =
                                  list(type = "values", unit = "ha", factor = 1,
                                       row = NULL, col = NULL, rel = FALSE,
                                       id = NULL, value = NULL),
                                production =
                                  list(type = "values", unit = "t", factor = 1,
                                       row = NULL, col = NULL, rel = FALSE,
                                       id = NULL, value = NULL)))

  input <- read_csv(paste0(system.file("test_datasets", package="censusTools"), "/table7.csv"),
                    col_names = FALSE) %>%
    record(what = "algorithm", name = "algo") %>%
    reorganise()

})

test_that("Error if arguments have wrong value", {

})