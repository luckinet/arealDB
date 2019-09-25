#' Translate terms
#'
#' Translate terms based on fuzzy matching.
#' @param terms [\code{character(.)}]\cr terms to be translated.
#' @param source [\code{list(1)}]\cr the table or geometry ID from which the
#'   terms have been taken. List must be named with either \code{tabID} or
#'   \code{geoID} to denote where the ID comes from.
#' @param index [\code{data.frame(1)}]\cr table that contains translations.
#' @param strict [\code{logical(1)}]\cr whether or not to stick to the terms
#'   that have been defined as \code{'original'} in a translation table.
#' @param fuzzy_terms [\code{vector(.)}]\cr additional target terms with which a
#'   fuzzy match should be carried out.
#' @param fuzzy_dist [\code{integerish(1)}]\cr the maximum edit-distance for
#'   which terms of fuzzy-matching should be suggested as match.
#' @param inline [\code{logical(1)}]\cr whether or not to edit translations
#'   inline in R, or in the 'translating.csv' in your database root directory
#'   (only possible in linux).
#' @param verbose [\code{logical(1)}]\cr be verbose about what is happening
#'   (default \code{TRUE}).
#' @return translated \code{terms}.
#' @details This is basically a sophisticated matching algorithm, that adds new
#'   entries to the respective index.
#' @examples
#' \dontrun{
#'
#' translateTerms(terms = c("wheet", "weizen", "Wheat", "Weat"),
#'                index = paste0("tt_commodities"))
#' }
#' @importFrom checkmate assertCharacter assertDataFrame assertNames
#'   testCharacter
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter pull bind_rows ends_with mutate select
#' @importFrom readr read_csv write_csv
#' @importFrom rlang enexpr
#' @importFrom utils edit View adist file.edit
#' @export

translateTerms <- function(terms, index = NULL, source = NULL, strict = FALSE,
                           fuzzy_terms = NULL, fuzzy_dist = 5, inline = TRUE,
                           verbose = TRUE){

  # check validity of arguments
  assertCharacter(x = terms, any.missing = FALSE)
  assertList(x = source, len = 1, null.ok = TRUE)
  assertNames(x = names(source), subset.of = c("geoID", "tabID"))
  assertCharacter(x = index, len = 1, any.missing = FALSE)
  args <- enexpr(index)
  indFile <- index
  index <- read_csv(paste0(getOption(x = "adb_path"), "/", index, ".csv"), col_types = "cccic")
  assertNames(x = colnames(index), must.include = c("origin", "target"))
  assertCharacter(x = fuzzy_terms, any.missing = FALSE, null.ok = TRUE)
  assertIntegerish(x = fuzzy_dist, any.missing = FALSE)

  # create a table with terms that will be used for fuzzy matching.
  if(strict){
    theFuzzyTerms <- index %>%
      filter(source == "original") %>%
      pull(target)
  } else {
    theFuzzyTerms <- index %>%
      filter(target != "missing") %>%
      pull(target)
  }
  if(!is.null(fuzzy_terms)){
    if(length(theFuzzyTerms) == 0){
      theFuzzyTerms <- fuzzy_terms
    } else{
      theFuzzyTerms <- c(theFuzzyTerms, fuzzy_terms)
    }
  }
  if(length(theFuzzyTerms) == 0){
    if(verbose){
      message("  ! found no values to fuzzy match with.")
    }
  }

  tempOut <- NULL
  sourceName <- names(source)
  sourceVal <- as.character(source[[1]])
  newEntries <- FALSE
  if(verbose){
    message("    translating from '", args, ".csv' ...")
    if(length(terms) > 15){
      pb <- txtProgressBar(min = 0, max = length(terms), style = 3, char=">", width=getOption("width")-14)
    }
  }

  # go through all terms and process them
  for(i in seq_along(terms)){
    # check whether the exact term is in 'target' or 'origin'
    exists <- terms[i] %in% unique(index$target) | terms[i] %in% unique(index$origin)

    if(exists){

      temp <- index %>%
        filter(index$target %in% terms[i] | index$origin %in% terms[i]) %>%
        rename("ID" = ends_with("ID"))

      # first make sure that the term should not be ignored
      if(any(temp$target == "ignore")){
        temp <- tibble(origin = terms[i], target = "ignore", source = NA_character_, ID = NA_character_, notes = NA_character_)
      }

      # then take terms that are not 'missing'
      if(any(temp$target != "missing")){
        temp <- temp %>%
          filter(temp$target != "missing") %>%
          mutate(source = sourceName,
                 ID = sourceVal) %>%
          select(-notes) %>%
          unique() %>%
          mutate(notes = paste0("translateTerms_", Sys.Date()))
      } else {
        temp <- tibble(origin = terms[i], target = "missing", source = sourceName, ID = sourceVal, notes = NA_character_)
      }

      # make sure that there is only one translation
      if(length(unique(temp$target)) != 1){
        stop(paste0("'", terms[i], "' does not have a unique translation in ", indFile, ".csv."))
      }

      tempOut <- bind_rows(tempOut, temp)

    } else {

      # if a set of fuzzy terms is available, try to match with those
      if(length(theFuzzyTerms) != 0){
        # all terms are put to lower case, which makes matching them easier,
        # this preserves apostrophes
        distances <- adist(tolower(terms[i]), tolower(theFuzzyTerms))
        distances_sum <- cumsum(table(distances))
        thresh_dist <- as.numeric(names(distances_sum[distances_sum < fuzzy_dist]))
        # take care of terms with a too high edit distance
        if(length(thresh_dist) == 0){
          thresh_dist <- 99
        }
        theFuzz <- unlist(lapply(thresh_dist, function(x){
          theFuzzyTerms[which(distances %in% x)]
        }))

        # in case an edit distance of 0 has been found, this term is perfectly
        # matched and doesn't need to be further treated
        if(thresh_dist[1] == 0){
          app <- c(terms[i], theFuzz[1], sourceName, sourceVal, NA_character_)
        } else{
          newEntries <- TRUE
          app <- c(terms[i], "missing", sourceName, sourceVal, paste0(theFuzz, collapse = " | "))
        }
      } else{
        app <- c(terms[i], tolower(terms[i]), sourceName, sourceVal, NA_character_)
      }

      names(app) <- c("origin", "target", "source", "ID", "notes")
      tempOut <- bind_rows(tempOut, app)

    }

    if(verbose){
      if(length(terms) > 15){
        setTxtProgressBar(pb, i)
      }
    }
  }

  if(verbose){
    if(length(terms) > 15){
      close(pb)
    }
  }

  # make sure that the columns contain the correct data
  tempOut$ID <- as.integer(tempOut$ID)

  if(newEntries){

    # define paths for translating
    basePath <- paste0(getOption("adb_path"))
    translating <- paste0(basePath, "/translating.csv")

    toTranslate <- tempOut %>%
      filter(target == "missing")
    write_csv(x = toTranslate, path = translating)
    if(Sys.info()[['sysname']] == "Linux" & inline){
      file.edit(translating)
      done <- readline("\nplease replace the missing values, save the file and press any key to continue.\n")
    } else {
      done <- readline(paste0("\nplease edit the column 'target' in '", getOption(x = "adb_path"), "/'translating.csv' and then press any key to continue.\n"))
    }

    newOut <- read_csv(file = translating,
                       col_types = getColTypes(index)) %>%
      mutate(notes = paste0("translateTerms_", Sys.Date()))

    translated <- newOut$target
    translated <- translated[translated != "missing"]
    translated <- translated[translated != "ignore"]

    if(strict){
      if(!all(translated %in% theFuzzyTerms)){
        stop("there are translated terms that did not appear in the standard vocabulary.")
      }
    }

    file.remove(translating)

    out <- tempOut %>%
      filter(target != "missing") %>%
      bind_rows(newOut)
  } else{
    out <- tempOut
  }
  updateTable(index = out, name = args)

  # output should contain instead of the colum 'source' the column 'inventory' and instead of 'tabID' simply 'ID'
  return(out)
}

