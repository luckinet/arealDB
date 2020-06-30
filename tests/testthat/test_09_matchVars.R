library(testthat)
library(tabshiftr)
library(readr)
library(checkmate)
context("matchVars")

test_that("variables are matched", {

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

  input <- read_csv(file = paste0(getOption("adb_path"), "/adb_tables/stage2/est_1_soyMaize_1990_2017_madeUp.csv"),
                    col_names = FALSE) %>%
    reorganise(schema = meta_madeUp_1) %>%
    mutate(id = seq_along(year),
           tabID = 1,
           geoID = 1) %>%
    matchUnits(source = list("tabID" = 1))

  output <- matchVars(input = input, faoID = list(commodities = "target"),
                      source = 1)
  expect_tibble(x = output, nrows = 56, ncols = 10, col.names = "strict")
  expect_double(x = output$faoID, any.missing = FALSE)
  expect_names(x = names(output), permutation.of = c("year", "commodities", "harvested", "production", "id", "tabID", "geoID", "al1_name", "ahID", "faoID"))
})

