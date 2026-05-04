library(arealDB)
library(testthat)
library(checkmate)
library(tabshiftr)
context("adb_translations")


test_that("adb_translations returns mapping table for a dataseries", {

  dbpath <- paste0(tempdir(), "/newDB")
  adb_example(until = "normGeometry", path = dbpath)

  match <- adb_translations(vocabulary = "gazetteer", dataseries = "madeUp")

  expect_data_frame(x = match)
  expect_names(x = names(match), identical.to = c("source_label", "source", "canonical_id", "parent_id", "note"))
  expect_true(all(match$source == "madeUp"))
  expect_true(nrow(match) >= 4)

})
