library(arealDB)
library(testthat)
library(checkmate)
library(tibble)
library(tabshiftr)
context("matchOntology")


test_that("gazetteer mappings are correct after geometry normalisation", {

  dbpath <- paste0(tempdir(), "/newDB")
  adb_example(until = "regTable", path = dbpath)

  gazName <- "gazetteer"

  # normalise first level ----
  normGeometry(input = paste0(.adb_state$path, "/geometries/stage2/_ADM0__gadm.gpkg"))

  gadm_maps <- .read_mappings(gazName, "gadm")
  expect_true("a_nation" %in% .read_terms(gazName)$label)
  expect_true(any(gadm_maps$canonical_id == ".001"))

  # normalise second level ----
  normGeometry(input = paste0(.adb_state$path, "/geometries/stage2/_ADM1__gadm.gpkg"))

  gadm_maps <- .read_mappings(gazName, "gadm")
  expect_true(nrow(gadm_maps[gadm_maps$note != "class_label" | is.na(gadm_maps$note), ]) >= 4)

  # normalise third level ----
  normGeometry(input = paste0(.adb_state$path, "/geometries/stage2/_ADM2__gadm.gpkg"))

  gadm_maps <- .read_mappings(gazName, "gadm")
  expect_true(nrow(gadm_maps) >= 5)

  # normalise a non-gadm dataset ----
  normGeometry(input = paste0(.adb_state$path, "/geometries/stage2/_ADM2__madeUp.gpkg"))

  madeUp_maps <- .read_mappings(gazName, "madeUp")
  expect_true(nrow(madeUp_maps) >= 4)

})

test_that("gazetteer mappings are correct after table normalisation", {

  dbpath <- paste0(tempdir(), "/newDB")
  adb_example(until = "normGeometry", path = dbpath)

  output <- normTable()

  gazName <- "gazetteer"
  terms <- .read_terms(gazName)
  expect_true("a_nation" %in% terms$label)

})
