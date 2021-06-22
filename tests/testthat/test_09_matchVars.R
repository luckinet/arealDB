library(testthat)
library(tabshiftr)
library(readr)
library(checkmate)
context("matchVars")

test_that("variables are matched", {

  makeExampleDB(until = "normGeometry")

  meta_madeUp_1 <-
    setIDVar(name = "al1", columns = 1) %>%
    setIDVar(name = "year", columns = 2) %>%
    setIDVar(name = "commodities", columns = 3) %>%
    setObsVar(name = "harvested", unit = "ha", columns = 4) %>%
    setObsVar(name = "production", unit = "t", columns = 5)

  input <- read_csv(file = paste0(getOption("adb_path"), "/adb_tables/stage2/est_1_barleyMaize_1990_2017_madeUp.csv"),
                    col_names = FALSE) %>%
    reorganise(schema = meta_madeUp_1) %>%
    mutate(id = seq_along(year),
           tabID = 1,
           geoID = 1) %>%
    matchUnits(source = list("tabID" = 1))

  output <- matchVars(input = input, faoID = list(commodities = "target"),
                      source = list("tabID" = 1))
  expect_tibble(x = output, nrows = 56, ncols = 11, col.names = "strict")
  expect_double(x = output$faoID, any.missing = FALSE)
  expect_names(x = names(output), permutation.of = c("al1_alt", "year", "commodities", "harvested", "production", "id", "tabID", "geoID", "al1_name", "ahID", "faoID"))
})

