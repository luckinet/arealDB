#' Translate terms
#'
#' Translate terms based on fuzzy matching.
#' @param terms [\code{character(.)}]\cr terms to be translated.
#' @param index [\code{data.frame(1)}]\cr table that contains translations.
#' @param fuzzy_terms [\code{vector(.)}]\cr target terms with which a fuzzy
#'   match should be carried out.
#' @param fuzzy_dist [\code{integerish(1)}]\cr the maximum distance for
#'   fuzzy-matching.
#' @param verbose [\code{logical(1)}]\cr be verbose about what is happening
#'   (default \code{TRUE}).
#' @details This is basically a sophisticated matching algorithm, that adds new
#'   entries to the respective index, if no match was found.
#' @importFrom checkmate assertCharacter assertDataFrame assertNames
#'   testCharacter
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter pull bind_rows
#' @importFrom readr read_csv write_csv
#' @importFrom rlang enexpr
#' @importFrom utils edit View adist file.edit
#' @export

translateTerms <- function(terms, source = NULL, index = NULL, fuzzy_terms = NULL,
                           fuzzy_dist = 5, verbose = TRUE){

  # check validity of arguments
  assertCharacter(x = terms, any.missing = FALSE)
  assertIntegerish(x = source)
  assertCharacter(x = index, len = 1, any.missing = FALSE)
  args <- enexpr(index)
  index <- read_csv(paste0(getOption(x = "cT_path"), "/", index, ".csv"), col_types = "cccDi")
  assertNames(x = colnames(index), must.include = c("origin", "target"))
  assertCharacter(x = fuzzy_terms, any.missing = FALSE, null.ok = TRUE)
  assertIntegerish(x = fuzzy_dist, any.missing = FALSE)

  # create a table with terms that will be used for fuzzy matching.
  theFuzzyTerms <- index %>%
    filter(source == "original") %>%
    pull(target)
  if(!is.null(fuzzy_terms)){
    if(length(theFuzzyTerms) == 0){
      theFuzzyTerms <- fuzzy_terms
    } else{
      theFuzzyTerms <- c(theFuzzyTerms, tempFuzz)
    }
  }
  if(length(theFuzzyTerms) == 0){
    if(verbose){
      message("  ! found no values to fuzzy match with.")
    }
  }

  tempOut <- NULL
  newEntries <- FALSE
  message("    translating from '", args, ".csv' ...")
  if(verbose){
    if(length(terms) > 15){
      pb <- txtProgressBar(min = 0, max = length(terms), style = 3, char=">", width=getOption("width")-14)
    }
  }

  # go through all terms and process them
  for(i in seq_along(terms)){
    # get a row of 'index' where there might be an instance of the term
    temp <- index %>%
      filter(tolower(origin) %in% tolower(terms[i]))

    if(dim(temp)[1] < 1){

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

        # in case a edit distance of 0 has been found, this term is perfectly
        # matched and doesn't need to be further treated
        if(thresh_dist[1] == 0){
          app <- c(terms[i], theFuzz[1], "translateTerms()", paste0(Sys.Date()), source)
        } else{
          newEntries <- TRUE
          app <- c(terms[i], "missing", paste0(theFuzz, collapse = " | "), paste0(Sys.Date()), source)
        }
      } else{
        newEntries <- TRUE
        app <- c(terms[i], "missing", paste0("check out ", args, ".csv"), paste0(Sys.Date()), source)
      }

      names(app) <- colnames(index)
      tempOut <- bind_rows(tempOut, app)
    } else{
      temp <- temp %>%
        mutate(date = as.character(date),
               cenID = as.character(cenID))
      tempOut <- bind_rows(tempOut, temp)
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
  tempOut$date <- as.Date(tempOut$date)
  tempOut$cenID <- as.integer(tempOut$cenID)

  if(newEntries){

    # define paths for translating
    basePath <- paste0(getOption("cT_path"))
    translating <- paste0(basePath, "/translating.csv")

    toTranslate <- tempOut %>%
      filter(target == "missing")
    write_csv(x = toTranslate, path = translating)
    if(Sys.info()[['sysname']] == "Linux"){
      file.edit(translating)
    }
    done <- readline("\nplease replace the missing values, save the file and press any key to continue.\n")

    newOut <- read_csv(file = translating,
                       col_types = getColTypes(index))
    newOut$source <- ifelse(newOut$target != "missing", paste0("translateTerms()"), NA_character_)
    newOut$target <- ifelse(newOut$target == "missing", NA_character_, newOut$target)

    file.remove(translating)
    updateIndex(index = newOut, name = args)

    out <- tempOut %>%
      filter(target != "missing") %>%
      full_join(newOut) %>%
      select(origin, target)
  } else{
    out <- tempOut
  }

  return(out)
}

