library(testthat)
library(checkmate)
library(tibble)
context("testCompressed")


test_that("testCompressed works", {

  # test a couple of file-names that have endings which indicate compressed files
  # .gz
  expect_true(testCompressed(x = "bla.gz"))

  # .bz2
  expect_true(testCompressed(x = "bla.bz2"))

  # .tar
  expect_true(testCompressed(x = "bla.tar"))

  # .zip
  expect_true(testCompressed(x = "bla.zip"))

  # .tgz
  expect_true(testCompressed(x = "bla.tgz"))

  # .gzip
  expect_true(testCompressed(x = "bla.gzip"))

  # .7z
  expect_true(testCompressed(x = "bla.7z"))

})

test_that("Error if arguments have wrong value", {

  expect_error(object = testCompressed(x = 1))

})
