#' Register a new vocabulary contributor
#'
#' This function registers an external dataseries' vocabulary file as a
#' contributor to a named vocabulary (e.g. \code{gazetteer},
#' \code{commodity}). The first contributor to a given vocabulary becomes its
#' \emph{backbone} (defines the canonical terms and levels at normalisation
#' time). Subsequent contributors provide \emph{mappings} from their source
#' labels to canonical IDs of the existing backbone.
#'
#' @param name [`character(1)`][character]\cr name of the target vocabulary
#'   (e.g. \code{"gazetteer"}, \code{"commodity"}).
#' @param vSeries [`character(1)`][character]\cr name of the contributing
#'   dataseries (must already exist in \code{inv_dataseries}; see
#'   \code{\link{regDataseries}}).
#' @param description [`character(1)`][character]\cr free-text description of
#'   what the vocabulary represents.
#' @param schema [`schema`][tabshiftr::schema]\cr the tabshiftr schema
#'   describing how to reshape the contributor's stage 2 file. Must produce at
#'   minimum the required columns for the role (see Details).
#' @param archive [`character(1)`][character]\cr the original (stage 1) file
#'   from which the vocabulary entries originate.
#' @param archiveLink [`character(1)`][character]\cr download link of the
#'   archive.
#' @param downloadDate [`character(1)`][character]\cr download date in
#'   YYYY-MM-DD format.
#' @param version [`character(1)`][character]\cr version of the vocabulary.
#' @param licence_link [`character(1)`][character]\cr licence URL.
#' @param notes [`character(1)`][character]\cr optional notes.
#' @param diagnose [`logical(1)`][logical]\cr if TRUE, attempt
#'   \code{\link[tabshiftr]{reorganise}} on the stage 2 file with the schema
#'   to verify it produces the required columns.
#' @param overwrite [`logical(1)`][logical]\cr whether to overwrite an
#'   existing registration of this contributor.
#' @details The role of the contributor is determined automatically at
#'   normalisation time by \code{\link{normVocabulary}}: if the target
#'   vocabulary's terms file does not yet exist, this contributor is the
#'   backbone; otherwise it provides mappings.
#'
#'   Required columns the schema must produce:
#'   \itemize{
#'     \item Backbone (first contributor): \code{label}, \code{class},
#'       \code{parent_label}. Optional: \code{description}, \code{valid_from},
#'       \code{valid_until}, \code{successor_id}.
#'     \item Mapping (subsequent contributors): \code{source_label}, plus any
#'       additional source-specific columns (preserved through to the mappings
#'       file).
#'   }
#' @return Returns a tibble of the entry that is appended to
#'   \code{inv_vocabularies}.
#' @family register functions
#' @importFrom checkmate assertCharacter assertClass assertLogical assertNames
#'   assertFileExists testFileExists testDirectoryExists
#' @importFrom tabshiftr reorganise
#' @importFrom dplyr bind_rows
#' @importFrom readr write_rds read_csv cols
#' @importFrom tibble tibble
#' @importFrom arrow read_parquet
#' @export

regVocabulary <- function(name, vSeries, description, schema = NULL,
                          archive = NULL, archiveLink = NULL,
                          downloadDate = NULL, version = NULL,
                          licence_link = NULL, notes = NULL,
                          diagnose = FALSE, overwrite = FALSE){

  # library(checkmate); intPaths = arealDB:::.adb_state$path; testing = arealDB:::.adb_state$testing

  # set internal paths
  intPaths <- .adb_state$path
  gazName <- "gazetteer"

  # get tables
  inventory <- readRDS(paste0(intPaths, "/inventory.rds"))
  inv_vocabularies <- inventory$vocabularies
  inv_dataseries <- inventory$dataseries

  if(dim(inv_dataseries)[1] == 0){
    stop("'inv_dataseries' does not contain any entries!")
  }

  # make new vocID
  newVID <- ifelse(length(inv_vocabularies$vocID) == 0, 1L,
                   as.integer(max(inv_vocabularies$vocID) + 1L))

  # in testing mode?
  testing <- .adb_state$testing

  # check validity of arguments
  assertNames(x = colnames(inv_vocabularies),
              permutation.of = c("vocID", "datID", "name", "description", "version", "licence_link", "stage2_name", "schema", "stage1_name", "stage1_url", "download_date", "status", "notes"))
  assertNames(x = colnames(inv_dataseries),
              permutation.of = c("datID", "name", "description", "homepage", "version", "licence_link", "notes"))
  assertCharacter(x = name, len = 1, any.missing = FALSE)
  assertCharacter(x = vSeries, len = 1, any.missing = FALSE)
  assertCharacter(x = description, len = 1, any.missing = FALSE)
  assertCharacter(x = archive, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = archiveLink, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = downloadDate, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = version, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = licence_link, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = notes, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = diagnose, len = 1, any.missing = FALSE)
  assertLogical(x = overwrite, len = 1, any.missing = FALSE)
  assertClass(x = schema, classes = "schema", null.ok = TRUE)

  # the contributing dataseries must already be registered
  if(!vSeries %in% inv_dataseries$name){
    stop("the dataseries '", vSeries, "' is not registered. Use regDataseries() first.")
  }
  datID <- inv_dataseries$datID[inv_dataseries$name == vSeries]

  # filename + duplicate-registration check
  tempName <- paste0(name, "__", vSeries)
  fileName <- paste0(tempName, ".csv")
  filePath <- paste0(intPaths, "/vocabularies/stage2/", fileName)

  if(any(inv_vocabularies$stage2_name %in% fileName)){
    if(overwrite){
      theSchemaName <- inv_vocabularies$schema[inv_vocabularies$stage2_name == fileName]
      newVID <- inv_vocabularies$vocID[which(inv_vocabularies$stage2_name %in% fileName)]
    } else {
      return(paste0("'", fileName, "' has already been registered."))
    }
  } else {
    theSchemaName <- paste0(tempName, "_schema")
  }

  # determine the role of this contributor (backbone vs mapping):
  # - if a prior contributor is already registered against this vocabulary,
  #   this one is a mapping (regardless of whether normVocabulary has run);
  # - otherwise check the stage 3 terms file (covers imports / hand-prepped
  #   databases where stage 3 exists but the inventory was reset).
  priorContributor <- nrow(inv_vocabularies) > 0 &&
                      any(inv_vocabularies$name == name &
                          inv_vocabularies$stage2_name != fileName)
  termsFile <- file.path(intPaths, "vocabularies", "stage3",
                         paste0(name, "_terms.parquet"))
  hasTerms <- file.exists(termsFile) &&
              nrow(arrow::read_parquet(termsFile, mmap = FALSE)) > 0
  # if this exact contributor is the first one registered under this
  # vocabulary, it is (or was) the backbone -- re-registration keeps that role
  isFirstContributor <- nrow(inv_vocabularies) > 0 &&
                        identical(inv_vocabularies$stage2_name[
                          inv_vocabularies$name == name][1], fileName)
  isBackbone <- isFirstContributor || (!priorContributor && !hasTerms)

  # validate the schema produces the required columns
  if(!is.null(schema)){
    schemaCols <- names(schema@variables)
    if(isBackbone){
      # term-grain backbone: must have label, class, parent_label
      # leaf-grain backbone: must have neither of those nor source_label
      #   (all columns are hierarchy level names; normVocabulary pivots internally)
      isTermGrain <- any(c("label", "class", "parent_label") %in% schemaCols)
      isLeafGrain <- !isTermGrain && !"source_label" %in% schemaCols
      if(!isLeafGrain){
        missingCols <- setdiff(c("label", "class", "parent_label"), schemaCols)
        if(length(missingCols) > 0){
          stop("backbone schema is missing required column(s): ",
               paste(missingCols, collapse = ", "),
               ". Provide 'label', 'class', 'parent_label' (term-grain) or name ",
               "all columns after their hierarchy level (leaf-grain).")
        }
      }
    } else {
      if(!"source_label" %in% schemaCols){
        stop("mapping schema is missing required column 'source_label'.")
      }
    }
  }

  if(!is.null(schema)){
    write_rds(x = schema,
              file = paste0(intPaths, "/vocabularies/schemas/",
                            theSchemaName, ".rds"))
  }

  if(is.null(archive)){
    message("please type in the stage1 file name: ")
    if(!testing){
      archive <- readline()
    } else {
      archive <- "example_vocab.csv"
    }
    if(is.na(archive)){
      archive <- NA_character_
    }
  }

  # test whether the stage1 file is available
  if(!is.null(archive) && !is.na(archive)){
    stage1Dir  <- paste0(intPaths, "/vocabularies/stage1/", vSeries, "/")
    stage1Path <- paste0(stage1Dir, archive)
    if(!testFileExists(x = stage1Path)){
      message(paste0("... please store the file '", archive, "' in './vocabularies/stage1/", vSeries, "/'"))
      if(!testDirectoryExists(x = stage1Dir)){
        dir.create(path = stage1Dir, recursive = TRUE)
      }
      if(!testing){
        readline(" -> press any key when done: ")
        assertFileExists(x = stage1Path)
      }
    }
  }

  # locate the contributor file. Mappings (non-backbone contributors) may
  # already be present as resolved stage 3 outputs in vocabularies/mappings/;
  # backbones must be staged in vocabularies/stage2/.
  mappingsPath <- file.path(intPaths, "vocabularies", "mappings",
                            paste0(name, "_", vSeries, ".csv"))
  if(!isBackbone && testFileExists(x = mappingsPath, extension = "csv")){
    message("  -> using existing mapping file at ./vocabularies/mappings/",
            basename(mappingsPath))
  } else if(!testFileExists(x = filePath, extension = "csv")){
    processedPath <- paste0(intPaths, "/vocabularies/stage2/processed/", fileName)
    if(testFileExists(x = processedPath, extension = "csv")){
      message("! the vocabulary file '", fileName, "' has already been normalised !")
      return(invisible())
    }
    message("... please store the file as '", fileName, "' in './vocabularies/stage2'")
    if(!testing){
      readline(" -> press any key when done: ")
      assertFileExists(x = filePath, extension = "csv")
    }
  }

  # diagnose: try the reorganise call
  if(diagnose && file.exists(filePath)){
    thisTable <- read_csv(filePath, na = "",
                                 col_types = cols(.default = "c"),
                                 show_col_types = FALSE)
    temp <- tryCatch(reorganise(input = thisTable, schema = schema),
                     error = function(e) "There was an error message",
                     warning = function(w) "There was a warning message")
    if(isBackbone){
      schemaCols <- names(schema@variables)
      isLeafGrain <- !any(c("label", "class", "parent_label", "source_label") %in% schemaCols)
      ok <- if(isLeafGrain){
        is.data.frame(temp) && ncol(temp) > 0
      } else {
        is.data.frame(temp) && all(c("label", "class", "parent_label") %in% names(temp))
      }
    } else {
      ok <- is.data.frame(temp) && "source_label" %in% names(temp)
    }
    if(ok){
      message("schema ok")
    } else {
      stop(temp)
    }
  }

  # put together new vocabulary inventory entry
  doc <- tibble(vocID = newVID,
                datID = datID,
                name = name,
                description = description,
                version = if(is.null(version)) NA_character_ else version,
                licence_link = if(is.null(licence_link)) NA_character_ else licence_link,
                stage2_name = fileName,
                schema = theSchemaName,
                stage1_name = if(is.null(archive)) NA_character_ else archive,
                stage1_url = if(is.null(archiveLink)) NA_character_ else archiveLink,
                download_date = if(is.null(downloadDate)) as.Date(NA) else as.Date(downloadDate),
                status = "staged",
                notes = if(is.null(notes)) NA_character_ else notes)

  if(dim(inv_vocabularies)[1] != 0){
    if(doc$stage2_name %in% inv_vocabularies$stage2_name){
      if(overwrite){
        doc$vocID <- inv_vocabularies$vocID[inv_vocabularies$stage2_name == doc$stage2_name]
        inv_vocabularies[inv_vocabularies$stage2_name == doc$stage2_name, ] <- doc
        inventory$vocabularies <- inv_vocabularies
      } else {
        return(paste0("'", fileName, "' has already been registered."))
      }
    } else {
      inventory$vocabularies <- bind_rows(inv_vocabularies, doc)
    }
  } else {
    inventory$vocabularies <- doc
  }
  saveRDS(object = inventory, file = paste0(intPaths, "/inventory.rds"))

  return(doc)

}
