library(arealDB)
context("adb_ontology")

test_that("", {

  dbpath <- paste0(tempdir(), "/newDB")

  adb_example(until = "normTable", path = dbpath)

  onto <- adb_ontology(type = "gazetteer")



})
