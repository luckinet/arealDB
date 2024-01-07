library(arealDB)
context("adb_translations")


test_that("", {

  dbpath <- paste0(tempdir(), "/newDB")

  adb_example(until = "normTable", path = dbpath)

  match <- adb_translations(type = "gazetteer", dataseries = "madeUp")

  expect_data_frame(x = match, nrows = 9, ncols = 9)
  expect_names(x = names(match), identical.to = c("label", "class", "id", "has_broader", "description", "has_broader_match", "has_close_match", "has_exact_match", "has_narrower_match"))

})