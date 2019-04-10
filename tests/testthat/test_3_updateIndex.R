library(testthat)
library(checkmate)
context("updateIndex")

test_that("stores backup and creates new index that is larger than backup", {
  # this tests whether a backup is created and whether the new file contains
  # more rows than the backup
  # path <- system.file("test_data", package="DMT", mustWork = TRUE)
  # registerTables(root = paste0(path, "/"))

  # check which files are available before
  # files1 <- list.files(paste0(path, "/log/"))

  # run function to test
  # input <- tibble(origin = "ploom", target = "plum", notes = "estonian")
  # updateIndex(index = input, name = "trans_commodities")

  # check which files are available then and compare
  # files2 <- list.files(paste0(path, "/log/"))
  # expect_false(all(files2 %in% files1))

  # diff <- which(!files2 %in% files1)
  # origFile <- read_csv(paste0(path, "/log/", files2[diff]))
  # newFile <- read_csv(paste0(path, "/trans_commodities.csv"))
  #
  # expect_true(dim(newFile)[1] > dim(origFile)[1])
})

test_that("Error if arguments have wrong value", {
  # path <- system.file("test_data", package="DMT", mustWork = TRUE)
  # registerTables(root = paste0(path, "/"))
  #
  # input <- tibble(origin = "ploom", target = "plum", notes = "estonian")
  # wrongInput <- tibble(orig = "ploom", terget = "plum", notes = "estonian")
  # expect_error(updateIndex(index = input, name = "bla"))
  # expect_error(updateIndex(index = wrongOut, name = "trans_commodities"))
})

