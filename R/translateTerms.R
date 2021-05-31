#' Translate terms
#'
#' Translate terms based on fuzzy matching.
#' @param terms [\code{character(.)}]\cr terms to be translated.
#' @param source [\code{named list(1)}]\cr the table or geometry ID from which
#'   the terms have been taken. List must be named with either \code{tabID} or
#'   \code{geoID} to denote where the ID comes from.
#' @param index [\code{character(1)}]\cr name of a table that contains
#'   translations.
#' @param strict [\code{logical(1)}]\cr shall the translation be limited to
#'   \code{fuzzy_terms}, or shall also those terms be available that have been
#'   defined as \code{'original'} in a translation table.
#' @param fuzzy_terms [\code{vector(.)}]\cr additional target terms with which a
#'   fuzzy match should be carried out.
#' @param fuzzy_dist [\code{integerish(1)}]\cr the maximum edit-distance for
#'   which terms of fuzzy-matching should be suggested as match.
#' @param limit [\code{character(.)}]\cr a set of terms to which the translation
#'   should be limited.
#' @param inline [\code{logical(1)}]\cr whether or not to edit translations
#'   inline in R (only possible in linux), or in the 'translating.csv' in your
#'   database root directory.
#' @param verbose [\code{logical(1)}]\cr be verbose about what is happening
#'   (default \code{TRUE}).
#' @return A table of translated \code{terms}.
#' @details This is basically a sophisticated matching algorithm, that adds new
#'   entries to the respective index.
#' @importFrom checkmate assertCharacter assertDataFrame assertNames
#'   testCharacter
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter pull bind_rows ends_with mutate select slice
#' @importFrom readr read_csv write_csv
#' @importFrom rlang enexpr
#' @importFrom utils edit View adist file.edit

translateTerms <- function(terms, index = NULL, source = NULL, strict = FALSE,
                           fuzzy_terms = NULL, fuzzy_dist = 5, limit = NULL,
                           inline = TRUE, verbose = TRUE){

  # check validity of arguments
  assertCharacter(x = terms, any.missing = FALSE)
  assertList(x = source, len = 1, null.ok = TRUE)
  if(!is.null(source)){
    assertNames(x = names(source), subset.of = c("geoID", "tabID", "datID"))
  }
  inv_source <- ifelse(source[[1]] == "geoID", "inv_geometries.csv", "inv_tables.csv")
  assertCharacter(x = index, len = 1, any.missing = FALSE)
  args <- enexpr(index)
  indFile <- index
  index <- read_csv(paste0(getOption(x = "adb_path"), "/", index, ".csv"), col_types = "cccic")
  assertNames(x = colnames(index), must.include = c("origin", "target"))
  assertCharacter(x = fuzzy_terms, any.missing = FALSE, null.ok = TRUE)
  assertIntegerish(x = fuzzy_dist, any.missing = FALSE)
  assertCharacter(x = limit, any.missing = FALSE, null.ok = TRUE)

  # in testing mode?
  testing <- getOption(x = "adb_testing")
  basePath <- paste0(getOption("adb_path"))

  # create a table with terms that will be used for fuzzy matching.
  if(!is.null(fuzzy_terms)){
    theFuzzyTerms <- fuzzy_terms
  } else {
    theFuzzyTerms <- NULL
  }

  if(!strict){
    newFuzzy <- index %>%
      filter(source == "original") %>%
      pull(target)
    theFuzzyTerms <- c(theFuzzyTerms, newFuzzy)
  }

  if(length(theFuzzyTerms) == 0){
    if(verbose){
      message("  ! found no values to fuzzy match with.")
    }
  } else {
    targetTerms <- paste0(basePath, "/target_terms.csv")
    tibble(origin = sort(theFuzzyTerms)) %>%
      write_csv(file = targetTerms)
  }

  tempOut <- NULL
  sourceName <- names(source)
  if(is.null(sourceName)){
    sourceName <- NA_character_
    sourceVal <- NA_integer_
  } else {
    sourceVal <- source[[1]]
  }
  newEntries <- FALSE
  if(verbose){
    message("    translating from '", args, ".csv' ...")
    if(length(terms) > 15){
      pb <- txtProgressBar(min = 0, max = length(terms), style = 3, char=">", width=getOption("width")-14)
    }
  }

  nonAccented <- iconv(index$target, from = "UTF-8", to = "ASCII//TRANSLIT")

  # go through all terms and process them
  for(i in seq_along(terms)){

    # figure out which version of each term matches
    newTerm <- terms[i]
    doFuzzy <- FALSE
    matchTerm <- NULL
    if(tolower(newTerm) %in% tolower(index$origin)){
      matchTerm <- index$origin[which(tolower(index$origin) %in% tolower(newTerm))]
    } else if(tolower(newTerm) %in% tolower(index$target)){
      matchTerm <- index$target[which(tolower(index$target) %in% tolower(newTerm))]
    } else if(tolower(newTerm) %in% nonAccented){
      matchTerm <- index$origin[which(nonAccented %in% tolower(newTerm))]
    }

    temp <- index %>%
      mutate(rn = row_number()) %>%
      filter(target %in% matchTerm | origin %in% matchTerm) %>%
      filter(target != "missing" & !is.na(origin)) %>%
      rename("ID" = ends_with("ID"))

    # in case fuzzy terms were defined, filter target by them
    if(!is.null(theFuzzyTerms)){
      temp <- temp %>%
        filter(target %in% theFuzzyTerms | target == "ignore")
    }

    # first make sure that the term should not be ignored
    if(any(temp$target == "ignore")){
      app <- tibble(origin = newTerm, target = "ignore", source = NA_character_, ID = NA_integer_, notes = NA_character_)
    } else {
      # if there is no translation, carry out fuzzy matching to find best guess
      if(dim(temp)[1] > 0){
        # if there is more than one translation, try to match by 'sourceName'
        # and 'sourceVal'
        if(length(unique(temp$target)) > 1){

          if(any(!is.na(temp$source)) & any(!is.na(temp$ID))){
            temp <- temp %>%
              filter(!is.na(temp$ID)) %>%
              filter(source == sourceName & ID == sourceVal)
          }

          # if there is still more than one translation, let the user select
          if(length(unique(temp$target)) > 1){
            message("\nthe term '", terms[i], "' has several matches in '", indFile, ".csv'\n  -> please select the correct match (see '", inv_source, "' with '", sourceName, " = ", sourceVal, "'):")
            for(k in seq_along(unique(temp$target))){
              message("    ", k, ": ", unique(temp$target)[k])
            }
            message("    ", length(unique(temp$target))+1, ": ! none ! (allows you to provide another translation)")
            theTarget <- readline(prompt = "type in the number: ")
            tempTarget <- suppressWarnings(as.integer(theTarget))
            if(is.na(tempTarget)){
              assertIntegerish(x = theTarget, len = 1, any.missing = FALSE)
            } else {
              if(tempTarget == length(unique(temp$target))+1){
                app <- tibble(origin = newTerm,
                               target = "missing",
                               source = sourceName,
                               ID = as.integer(sourceVal),
                               notes = "already existing terms did not match")
                newEntries <- TRUE
              } else {
                app <- temp %>%
                  slice(tempTarget) %>%
                  select(-rn) %>%
                  mutate(origin = newTerm,
                         source = sourceName,
                         ID = sourceVal,
                         notes = paste0("translateTerms_", Sys.Date()))
              }

              # remove 'rn'
              if("rn" %in% names(app)){
                app <- app %>%
                  select(-rn)
              }
            }

          } else if(length(unique(temp$target)) < 1){
            doFuzzy <- TRUE
            app <- NULL
          } else {
            app <- temp %>%
              select(-rn)
          }

        } else {
          app <- temp %>%
            distinct(target) %>%
            mutate(origin = newTerm,
                   source = sourceName,
                   ID = sourceVal,
                   notes = paste0("translateTerms_", Sys.Date()))
        }
      } else {
        doFuzzy <- TRUE
        app <- NULL
      }

      if(doFuzzy){

        # if a set of fuzzy terms is available, try to match with those
        if(length(theFuzzyTerms) != 0){
          # all terms are put to lower case, which makes matching them easier,
          # this preserves apostrophes
          distances <- adist(tolower(newTerm), tolower(theFuzzyTerms))
          distances_sum <- cumsum(table(distances))
          thresh_dist <- distances_sum[as.numeric(names(distances_sum)) < fuzzy_dist]
          # take care of terms with a too high edit distance
          if(length(thresh_dist) == 0){
            thresh_dist <- 99
            names(thresh_dist) <- "missing"
          }
          theFuzz <- unlist(lapply(names(thresh_dist), function(x){
            if(x != "missing"){
              temp <- unique(theFuzzyTerms[which(distances %in% as.numeric(x))])
            } else {
              temp <- NULL
            }
          }))
          if(length(theFuzz) > 10){
            theFuzz <- theFuzz[1:10]
          }

          # in case an edit distance of 0 has been found or if the non-accented
          # matched term is the same as the input term, this term is perfectly
          # matched and doesn't need to be further treated
          if(names(thresh_dist[1]) == "0"){
            app <- tibble(origin = newTerm, target = theFuzz[1], source = sourceName, ID = as.integer(sourceVal), notes = NA_character_)
          } else if(any(newTerm %in% iconv(theFuzz, from = "UTF-8", to = "ASCII//TRANSLIT"))){
            targetFuzz <- theFuzz[which(iconv(theFuzz, from = "UTF-8", to = "ASCII//TRANSLIT") %in% newTerm)][1]
            app <- tibble(origin = newTerm, target = targetFuzz, source = sourceName, ID = as.integer(sourceVal), notes = NA_character_)
          } else {
            newEntries <- TRUE
            app <- tibble(origin = newTerm, target = "missing", source = sourceName, ID = as.integer(sourceVal), notes = paste0(theFuzz, collapse = " | "))
          }
        } else{
          app <- tibble(origin = newTerm, target = "missing", source = sourceName, ID = as.integer(sourceVal), notes = "no terms for fuzzy matching defined")
        }
      }
    }
    tempOut <- bind_rows(tempOut, app)


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
    translating <- paste0(basePath, "/translating.csv")

    toTranslate <- tempOut %>%
      filter(target == "missing")

    # # in case a set of limiting terms has been set, use to subset
    # if(!is.null(limit)){
    #   toTranslate <- toTranslate %>%
    #     filter(target %in% limit)
    # }

    if(dim(toTranslate)[1] != 0){

      newOut <- NULL
      while(any(toTranslate$target == "missing")){

        write_csv(x = toTranslate, file = translating)
        if(Sys.info()[['sysname']] == "Linux" & inline){
          message("please replace the missing values and save the file (see 'target_terms.csv' for valid terms)")
          file.edit(translating)
          done <- readline(" -> press any key when done: ")
        } else {
          message("please edit the column 'target' in '", getOption(x = "adb_path"), "/translating.csv'")
          done <- readline(" -> press any key when done: ")
        }

        if(!is.null(newOut)){
         newOut <- newOut %>%
           mutate(valid = TRUE)
        }
        newOut <- newOut %>%
          bind_rows(read_csv(file = translating,
                             col_types = getColTypes(index)) %>%
                      filter(target != "missing") %>%
                      mutate(valid = if_else(target %in% theFuzzyTerms, TRUE, if_else(target == "ignore", TRUE, FALSE))) %>%
                      mutate(notes = paste0("translateTerms_", Sys.Date())))

        invalid <- newOut %>%
          filter(!valid) %>%
          mutate(target = "missing",
                 notes = paste0("previous translation did not match, check '", indFile, ".csv' for origin terms.")) %>%
          select(-valid)

        newOut <- newOut %>%
          filter(valid) %>%
          select(-valid)

        toTranslate <- read_csv(file = translating,
                                col_types = getColTypes(index)) %>%
          filter(target == "missing") %>%
          bind_rows(invalid)



        if(testing){
          newOut <- toTranslate <- tibble(target = character(),
                                          origin = character(),
                                          source = character(),
                                          ID = integer(),
                                          notes = character())
        }
      }

      translated <- newOut$target
      translated <- translated[-which(translated == "ignore")]

      if(strict){
        if(!all(translated %in% theFuzzyTerms)){
          stop("there are translated terms that did not appear in the standard vocabulary.")
        }
      }

      file.remove(translating)
      # if(length(theFuzzyTerms) != 0){
      #   file.remove(targetTerms)
      # }

      out <- tempOut %>%
        filter(target != "missing") %>%
        bind_rows(newOut)

    } else {
      out <- tempOut
    }

  } else{
    out <- tempOut
  }
  updateTable(index = out, name = args)

  # output should contain instead of the colum 'source' the column 'inventory' and instead of 'tabID' simply 'ID'
  return(out)
}

