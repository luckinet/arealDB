library(testthat)
library(readr)
library(magrittr)
library(dplyr)
library(checkmate)
library(tabshiftr)
context("matchUnits")


test_that("units are matched", {

  makeExampleDB(until = "normGeometry")

  meta_madeUp_1 <- makeSchema(
    list(header = list(row = 1),
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

  meta_madeUp_2 <- makeSchema(
    list(header = list(row = 1),
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

  # test for a table that only has values at the first administrative level
  input <- read_csv(file = paste0(getOption("adb_path"), "/adb_tables/stage2/est_1_soyMaize_1990_2017_madeUp.csv"),
                    col_names = FALSE) %>%
    reorganise(schema = meta_madeUp_1) %>%
    mutate(id = seq_along(year),
           tabID = 1,
           geoID = 1)

  output <- matchUnits(input = input, source = list("tabID" = 1))
  expect_tibble(x = output, nrows = 56, ncols = 9, col.names = "strict")
  expect_character(x = output$ahID, any.missing = FALSE)
  expect_names(x = names(output), permutation.of = c("year", "commodities", "harvested", "production", "id", "tabID", "geoID", "al1_name", "ahID"))

  # test for a table that has values at the second administrative level
  input <- read_csv(file = paste0(getOption("adb_path"), "/adb_tables/stage2/est_2_soyMaize_1990_2017_madeUp.csv"),
                    col_names = FALSE) %>%
    reorganise(schema = meta_madeUp_2) %>%
    mutate(id = seq_along(year),
           tabID = 1,
           geoID = 1)

  output <- matchUnits(input = input, source = list("tabID" = 1))
  expect_tibble(x = output, nrows = 224, ncols = 13, col.names = "strict")
  expect_character(x = output$ahID, any.missing = FALSE)
  expect_names(x = names(output), permutation.of = c("year", "commodities", "harvested", "production", "id", "tabID", "geoID", "al1_name", "al1_id", "al2_name", "al2_id", "al3_id", "ahID"))
})