library(arealDB)
library(testthat)
library(checkmate)
library(tabshiftr)
library(tibble)
library(arrow)
context("regVocabulary")


test_that("regVocabulary registers a backbone contributor", {

  dbpath <- paste0(tempdir(), "/newDB")
  adb_example(until = "regDataseries", path = dbpath)

  # wipe the auto-ingested gazetteer + mappings so this contributor is the backbone
  file.remove(list.files(file.path(dbpath, "vocabularies", "mappings"),
                         full.names = TRUE))

  stage2 <- file.path(dbpath, "vocabularies", "stage2", "gazetteer__gadm.csv")
  write_csv(
    tibble(label = c("a_nation", "county_1"),
           class = c("ADM0", "ADM1"),
           parent_label = c(NA, "a_nation")),
    stage2)

  schema <-
    setIDVar(name = "label",        columns = 1) |>
    setIDVar(name = "class",        columns = 2) |>
    setIDVar(name = "parent_label", columns = 3)

  doc <- regVocabulary(name = "gazetteer",
                       dSeries = "gadm",
                       description = "GADM-derived test gazetteer",
                       schema = schema,
                       archive = "test.csv",
                       archiveLink = "https://example.org",
                       downloadDate = "2026-04-26",
                       version = "1.0",
                       licence_link = "https://example.org/licence")

  inventory <- readRDS(file.path(dbpath, "inventory.rds"))
  expect_true("gazetteer" %in% inventory$vocabularies$name)
  expect_equal(inventory$vocabularies$description[inventory$vocabularies$name == "gazetteer"],
               "GADM-derived test gazetteer")
  expect_true(file.exists(file.path(dbpath, "vocabularies", "schemas",
                                    "gazetteer__gadm_schema.rds")))
})


test_that("regVocabulary rejects schema missing required backbone columns", {

  dbpath <- paste0(tempdir(), "/newDB")
  adb_example(until = "regDataseries", path = dbpath)

  file.remove(list.files(file.path(dbpath, "vocabularies", "stage3"),
                         full.names = TRUE))

  stage2 <- file.path(dbpath, "vocabularies", "stage2", "gazetteer__gadm.csv")
  write_csv(tibble(label = "a_nation"), stage2)

  bad_schema <- setIDVar(name = "label", columns = 1)

  expect_error(
    regVocabulary(name = "gazetteer",
                  dSeries = "gadm",
                  description = "incomplete schema",
                  schema = bad_schema),
    regexp = "missing required column")
})


test_that("regVocabulary errors when dataseries is unknown", {

  dbpath <- paste0(tempdir(), "/newDB")
  adb_example(until = "regDataseries", path = dbpath)

  expect_error(
    regVocabulary(name = "gazetteer",
                  dSeries = "nonexistent",
                  description = "x"),
    regexp = "is not registered")
})
