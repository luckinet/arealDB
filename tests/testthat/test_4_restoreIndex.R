library(testthat)
library(checkmate)
context("restoreIndex")

test_that("function prints list of backed up files", {
  # # this tests that a complete list of files in the backup folder is printed
  # path <- system.file("test_data", package="DMT", mustWork = TRUE)
  # registerTables(root = paste0(path, "/"))
  #
  # out <- capture_output_lines(restoreIndex(pattern = "trans_commodities", print = T))
  # expect_character(x = out, len = 9)
  # expect_true(out[1] == "# A tibble: 6 x 3")
})

test_that("function successfully restores a file", {
  # # this tests that the function actually reverts the file back to the backup
  # # file
  # path <- system.file("test_data", package="DMT", mustWork = TRUE)
  # registerTables(root = paste0(path, "/"))
  #
  # # create a new index, and thus its backup
  # input <- tibble(origin = "ploom", target = "plum", notes = "estonian")
  # updateIndex(index = input, name = "trans_commodities")
  #
  # # get list of files in backup folder
  # files <- list.files(paste0(path, "/log"))
  # backup <- read_csv(paste0(path, "/log/", files[1]))
  #
  # # restore the first file in backup folder
  # restoreIndex(pattern = "trans_commodities", select = files[1])
  #
  # # grab the file that has been reverted and check whether it's the same as the
  # # selected backup
  # newFile <- read_csv(paste0(path, "/trans_commodities.csv"))
  # expect_identical(backup, newFile)
})

test_that("function restores oldest/newest file", {
  # # this tests whether the function actually restores the oldest or newest file
  # path <- system.file("test_data", package="DMT", mustWork = TRUE)
  # registerTables(root = paste0(path, "/"))
  #
  # # create yet another index, that contains more data
  # input <- tibble(origin = c("ploom"), target = c("plum"), notes = "estonian")
  # updateIndex(index = input, name = "trans_commodities")
  # # insert a delay of one second, because otherwise the two files that are
  # # created by calling this function twice, don't have a different timestamp,
  # # and hence there is no old and no new file, there is only one file.
  # Sys.sleep(time = 1)
  # input <- tibble(origin = c("ploom2"), target = c("plum2"), notes = "estonian")
  # updateIndex(index = input, name = "trans_commodities")
  #
  # # get list of files in backup folder
  # files <- list.files(paste0(path, "/log"))
  #
  # # restore the newest file in backup folder (which should have 103 rows)
  # restoreIndex(pattern = "trans_commodities", select = "newest")
  # newFile <- read_csv(paste0(path, "/trans_commodities.csv"))
  # expect_true(dim(newFile)[1] == 103)
  #
  # # restore the oldest file in backup folder (which should have 102 rows)
  # restoreIndex(pattern = "trans_commodities", select = "oldest")
  # newFile <- read_csv(paste0(path, "/trans_commodities.csv"))
  # expect_true(dim(newFile)[1] == 102)
})

test_that("Error if arguments have wrong value", {
  # path <- system.file("test_data", package="DMT", mustWork = TRUE)
  # registerTables(root = paste0(path, "/"))
  #
  # expect_error(restoreIndex(pattern = "bla", print = TRUE))
  # expect_error(restoreIndex(pattern = "trans_commodities", print = 1))
  # expect_error(restoreIndex(pattern = "trans_commodities", select = "bla"))
})
