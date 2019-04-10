#' Unify the names of Nations
#'
#' In LUCKINet we use a default set of nations. This functions renames nations
#' that don't abide by those rules yet.
#' @param unify [\code{data.frame(1)}]\cr the object in which to unify nations.
#' @param nameCol [\code{character}]\cr the name of the column that contains
#'   nation names to unify.
#' @examples
#' \dontrun{
#'
#' library(sf)
#'
#' setPath(root = "/home/se87kuhe/Nextcloud/LUCKINet/data/")
#' gadm <- st_read(paste0(getOption("dmt_path"),
#'                        "administrative_boundaries/original_datasets/gadm36_levels.gpkg"),
#'                layer = "level0",
#'                stringsAsFactors = FALSE)
#'
#' (gadm2 <- unifyNations(unify = gadm, nameCol = "NAME_0"))
#' }
#' @importFrom checkmate testClass assert assertDataFrame assertCharacter
#' @importFrom dplyr bind_cols left_join rowwise mutate ungroup select
#' @importFrom sf st_sf
#' @export

unifyNations <- function(unify = NULL, nameCol = NULL){

  # check validity of arguments
  isSf <- testClass(x = unify, classes = "sf")
  isChar <- testCharacter(x = unify)
  assert(isSf, isChar)
  assertCharacter(nameCol, len = 1, any.missing = FALSE, null.ok = TRUE)

  if(isSf){
    toUnify <- unique(eval(parse(text = nameCol), envir = unify))

    correctNames <- toUnify[toUnify %in% countries$nation]
    falseNames <- toUnify[!toUnify %in% countries$nation]

    if(length(falseNames) > 0){
      newNames <- translateTerms(terms = falseNames,
                                 index = "trans_nations")

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

      message("--> units are already unified.")
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
                                 index = "trans_nations")

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

      message("--> units are already unified.")
      return(unify)

    }
  }

}