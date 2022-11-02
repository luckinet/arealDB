library(arealDB)
library(testthat)
library(tabshiftr)
library(readr)
library(checkmate)
context("matchontology")


test_that("", {

  dbpath <- paste0(tempdir(), "/newDB")
  makeExampleDB(until = "normTable", path = dbpath)


})
