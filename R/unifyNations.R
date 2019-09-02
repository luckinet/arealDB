#' Unify the names of Nations
#'
#' This function harmonises the nation names with the standard list in
#' \code{\link{countries}}.
#' @param unify [\code{data.frame(1)}]\cr the object in which to unify nations.
#' @param source [\code{integerish(1)}]\cr and ID, typically the census ID from
#'   which the terms are provided.
#' @param nameCol [\code{character}]\cr the name of the column that contains
#'   nation names to unify.
#' @param verbose [\code{logical(1)}]\cr be verbose about what is happening
#'   (default \code{TRUE}).
#' @return The object provided in \code{unify} where the nation values in
#'   \code{nameCol} have been harmonised.
#' @examples
#' \dontrun{
#'
#' library(sf)
#' gadm <- st_read(dsn = ".../adb_geometries/stage2/gadm36_levels.gpkg",
#'                 layer = "level0",
#'                 stringsAsFactors = FALSE)
#'
#' (gadm2 <- unifyNations(unify = gadm, nameCol = "NAME_0"))
#' }
#' @importFrom checkmate testClass assert assertDataFrame assertCharacter
#' @importFrom dplyr bind_cols left_join rowwise mutate ungroup select
#' @importFrom sf st_sf
#' @export

unifyNations <- function(unify = NULL, source = NULL, nameCol = NULL,
                         verbose = TRUE){

  # check validity of arguments
  isSf <- testClass(x = unify, classes = "sf")
  isChar <- testCharacter(x = unify)
  assert(isSf, isChar)
  assertIntegerish(x = source, null.ok = TRUE)
  assertCharacter(nameCol, len = 1, any.missing = FALSE, null.ok = TRUE)

  if(isSf){
    toUnify <- unique(eval(parse(text = nameCol), envir = unify))

    correctNames <- as.character(toUnify[toUnify %in% countries$nation])
    falseNames <- as.character(toUnify[!toUnify %in% countries$nation])

    if(length(falseNames) > 0){
      newNames <- translateTerms(terms = falseNames,
                                 source = source,
                                 index = "tt_nations",
                                 verbose = verbose)

      correctNames <- bind_cols(!!nameCol := correctNames,
                                target = correctNames)
      newNames <- bind_cols(!!nameCol := falseNames,
                            target = newNames)
      toJoin <- bind_rows(correctNames, newNames)

      out <- unify %>%
        left_join(toJoin, by = nameCol) %>%
        mutate(!!nameCol := target) %>%
        select(-target) %>%
        st_sf()
      return(out)

    } else{

      if(verbose){
        message("--> units are already unified.")
      }
      return(unify)

    }

  } else if(isChar){

    toUnify <- tibble(id = seq_along(unify),
                      terms = unify,
                      inCountry = terms %in% countries$nation)

    correctNames <- toUnify %>%
      filter(inCountry == TRUE) %>%
      pull(terms)
    falseNames <- toUnify %>%
      filter(inCountry == FALSE) %>%
      pull(terms)

    if(length(falseNames) > 0){
      newNames <- translateTerms(terms = falseNames,
                                 source = source,
                                 index = "tt_nations",
                                 verbose = verbose)

      if(dim(newNames)[1] == 0){
        return(NA_character_)
      }

      correctNames <- bind_cols(origin = correctNames,
                                target = correctNames)
      newNames <- bind_cols(origin = falseNames,
                            target = newNames)
      toJoin <- bind_rows(correctNames, newNames)

      out <- toUnify %>%
        left_join(toJoin, by = c("terms" = "origin")) %>%
        pull(target)
      return(out)

    } else{

      if(verbose){
        message("--> units are already unified.")
      }
      return(unify)

    }
  }

}