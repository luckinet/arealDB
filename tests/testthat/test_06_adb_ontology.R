library(arealDB)
library(testthat)
library(checkmate)
context("adb_ontology")


test_that("adb_ontology returns gazetteer with mapping columns after normGeometry", {

  dbpath <- paste0(tempdir(), "/newDB")
  adb_example(until = "normGeometry", path = dbpath)

  onto <- adb_ontology(vocabulary = "gazetteer")

  # base terms columns plus gadm and madeUp mapping columns
  expect_data_frame(x = onto)
  expect_names(x = names(onto), must.include = c("id", "label", "class", "parent_id", "gadm", "madeUp"))

  # all canonical terms are present
  expect_true("a_nation" %in% onto$label)
  expect_true(all(c("ADM0", "ADM1", "ADM2") %in% onto$class))

})

test_that("adb_ontology filter argument works", {

  dbpath <- paste0(tempdir(), "/newDB")
  adb_example(until = "normGeometry", path = dbpath)

  onto_adm0 <- adb_ontology(class == "ADM0", vocabulary = "gazetteer")
  expect_true(all(onto_adm0$class == "ADM0"))

})
