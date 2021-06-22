library(testthat)
library(readr)
library(magrittr)
library(dplyr)
library(checkmate)
library(tabshiftr)
context("matchUnits")


test_that("units are matched", {

  makeExampleDB(until = "normGeometry")

  meta_madeUp_1 <-
    setIDVar(name = "al1", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", unit = "ha", columns = 4) %>%
    setObsVar(name = "production", unit = "t", columns = 5)

  meta_madeUp_2 <-
    setIDVar(name = "al1", columns = 1) %>%
    setIDVar(name = "al2", columns = 2) %>%
    setIDVar(name = "year", columns = 3) %>%
    setIDVar(name = "commodities", columns = 4) %>%
    setObsVar(name = "harvested", unit = "ha", columns = 5) %>%
    setObsVar(name = "production", unit = "t", columns = 6)

  # test for a table that only has values at the first administrative level
  input <- read_csv(file = paste0(getOption("adb_path"), "/adb_tables/stage2/est_1_barleyMaize_1990_2017_madeUp.csv"),
                    col_names = FALSE) %>%
    reorganise(schema = meta_madeUp_1) %>%
    mutate(id = seq_along(year),
           tabID = 1,
           geoID = 1)

  output <- matchUnits(input = input, source = list("tabID" = 1))
  expect_tibble(x = output, nrows = 56, ncols = 10, col.names = "strict")
  expect_character(x = output$ahID, any.missing = FALSE)
  expect_names(x = names(output), permutation.of = c("al1_alt", "year", "commodities", "harvested", "production", "id", "tabID", "geoID", "al1_name", "ahID"))

  # test for a table that has values at the second administrative level
  input <- read_csv(file = paste0(getOption("adb_path"), "/adb_tables/stage2/est_2_barleyMaize_1990_2017_madeUp.csv"),
                    col_names = FALSE) %>%
    reorganise(schema = meta_madeUp_2) %>%
    mutate(id = seq_along(year),
           tabID = 1,
           geoID = 1)

  output <- matchUnits(input = input, source = list("tabID" = 1))
  expect_tibble(x = output, nrows = 224, ncols = 15, col.names = "strict")
  expect_character(x = output$ahID, any.missing = FALSE)
  expect_names(x = names(output), permutation.of = c("al1_alt", "al2_alt", "year", "commodities", "harvested", "production", "id", "tabID", "geoID", "al1_name", "al1_id", "al2_name", "al2_id", "al3_id", "ahID"))
})
