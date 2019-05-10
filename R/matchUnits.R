#' Determine the valid hierarchy ID of administrative units
#'
#' This function matches administrative units with a list of known units and
#' returns \code{input} containing the matched \code{ahID}.
#' @param input [\code{data.frame(1)}]\cr table in which to match administrative
#'   units.
#' @param source [\code{integerish(1)}]\cr the geometry ID (\code{geoID}) from
#'   which the terms have been taken.
#' @param ... [\code{character(.)}]\cr define columns in \code{input}, where the
#'   administrative units are recorded. Specify as \code{al1 = XYZ} to subset
#'   administrative level 1 (nation) to "XYZ", \code{al2 = ...} for the second
#'   level, etc.
#' @param keepOrig [\code{logical(1)}]\cr to keep the original units in the
#'   output (\code{TRUE}) or to remove them (\code{FALSE}, default). Useful for
#'   debugging.
#' @details \code{...} must contain at least the argument \code{al1 = ...} to
#'   subset for a certain nation.
#' @return The table provided in \code{input}, where the given columns are
#'   replaced by the column \code{ahID}, which contains the administrative
#'   hierarchy ID.
#' @importFrom checkmate assertCharacter assertIntegerish assertList
#'   assertDataFrame
#' @importFrom dplyr filter pull mutate mutate_at bind_rows bind_cols vars
#'   any_vars all_vars
#' @importFrom magrittr %>%
#' @importFrom rlang exprs :=
#' @importFrom sf st_geometry<- read_sf
#' @importFrom stats setNames
#' @importFrom tibble tibble
#' @importFrom tidyselect matches everything contains
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export

matchUnits <- function(input = NULL, source = NULL, ..., keepOrig = FALSE){

  # set internal objects
  intPaths <- paste0(getOption(x = "dmt_path"), "/cT_geometries/")
  adminLvls <- exprs(..., .named = TRUE)

  # check validity of arguments
  assertDataFrame(x = input)
  assertIntegerish(x = source)
  assertLogical(x = keepOrig)
  assertList(x = adminLvls)
  # assert that one of the arguments has the name "al1" so that at least nations are matched
  if(length(adminLvls) != 0){
    assertNames(x = names(adminLvls), must.include = "al1")
    wrongSubset <- !grepl(pattern = "al[[:digit:]]$", names(adminLvls))
    if(any(wrongSubset)){
      stop("Assertion on argument(s) '", paste0(names(adminLvls)[wrongSubset], " = ", adminLvls[wrongSubset]), "' failed: Must comply to pattern 'al[[:digit:]]$'")
    }
    # assert that the target columns are part of input
    targetColumns <- sapply(seq_along(adminLvls), function(x){
      as.character(adminLvls[[x]])
    })
    assertSubset(x = targetColumns, choices = names(input))
  } else {
    adminLvls <- list(al1 = as.symbol("al1"))
    assertNames(x = names(input), must.include = "al1")
  }

  out <- input
  nationCol <- adminLvls$al1

  # to manage the workload, split up input according to nations ("al1")
  nations <- unique(eval(parse(text = nationCol), envir = input))
  outhIDs <- NULL
  for(i in seq_along(nations)){
    rawNation <- nations[i]
    cleanNation <- unifyNations(unify = rawNation)

    if(is.na(cleanNation)){
      message(paste0("\n--> ! skipping ", rawNation, " because it does not match in 'tt_nations.csv'."))
      next
    }

    # make an input subset for the current nation ...
    inputSbst <- input %>%
      filter(!!nationCol == rawNation)

    # ... extract and find the unique combinations of the respective columns,
    # this asserts that only administrative units nested in their parents are
    # found
    allInputUnits <- inputSbst %>%
      select(!!!adminLvls) %>%
      unique()

    # load the nation geometries ...
    layers <- st_layers(dsn = paste0(intPaths, "stage2/", cleanNation, ".gpkg"))
    geometries <- NULL
    message("\n--> Matching geometries of '", cleanNation, "'.")
    for(j in seq_along(layers$name)){
      theGeom <- read_sf(dsn = paste0(intPaths, "stage2/", cleanNation, ".gpkg"),
                         layer = sort(layers$name)[j],
                         stringsAsFactors = FALSE)
      geometries <- rbind(geometries, theGeom)
    }

    # ... and select only unique rows of all 'al*_id'
    temp <- geometries %>%
      select(name, al1_id, al2_id, al3_id, al4_id, al5_id, al6_id)
    st_geometry(temp) <- NULL
    geometries <- geometries %>%
      filter(!duplicated(temp))

    outputUnits <- geometries
    levels <- NULL
    for(j in seq_along(adminLvls)){
      theLevel <- as.integer(sub(pattern = "\\D*(\\d+).*",
                                 replacement = "\\1",
                                 x = names(adminLvls)[j]))
      levels <- c(levels, theLevel)

      if(j == 1){
        if(length(adminLvls) == 1){
          outputUnits <- outputUnits %>%
            filter(name == cleanNation) %>%
            as_tibble() %>%
            mutate(al1_alt = rawNation) %>%
            select(al1 = name, al1_alt, level, ahID)
        } else{
          next
        }
      } else{

        # In a first step we need to build a table of the units and their
        # parents, both with the standard name of our database ('geometries'),
        # then ...
        unitSubset <- geometries %>%
          filter(level == theLevel)
        parentSubset <- geometries %>%
          filter(level == theLevel-1)

        # ... select only unique elements of the current level and its parent
        inputUnits <- allInputUnits %>%
          select(parent = paste0("al", j-1), unit = paste0("al", j)) %>%
          unique()

        # ... translate them to the default unit names
        theParents <- translateTerms(terms = unique(inputUnits[[1]]),
                                     source = source,
                                     index = "tt_units",
                                     fuzzy_terms = unique(parentSubset$name))
        theParents <- unique(theParents)
        theUnits <- translateTerms(terms = unique(inputUnits[[2]]),
                                   source = source,
                                   index = "tt_units",
                                   fuzzy_terms = unique(unitSubset$name))
        theUnits <- unique(theUnits)

        # ... join with 'inputUnits' to get the standard names into it
        tempUnits <- inputUnits %>%
          left_join(theParents, by = c("parent" = "origin")) %>%
          select(parent, !!paste0("al", j-1, "_name") := target, unit) %>%
          left_join(theUnits, by = c("unit" = "origin")) %>%
          select(parent, unit, paste0("al", j-1, "_name"), !!paste0("al", j, "_name") := target)

        # In the second step we join the respective 'al*_id' values to 'tempUnits'
        parentID <- geometries %>%
          as_tibble() %>%
          filter(level == theLevel-1) %>%
          filter(name %in% unique(tempUnits[[3]])) %>%
          select(!!paste0("al", j-1, "_name") := name, id = paste0("al", j-1, "_id")) %>%
          unique()

        # here it's crucial to get the unit ID and the ID of their parents
        unitID <- geometries %>%
          as_tibble() %>%
          filter(level == theLevel) %>%
          filter(name %in% unique(tempUnits[[4]])) %>%
          select(!!paste0("al", j, "_name") := name, idUnit = paste0("al", j, "_id"), idParent = paste0("al", j-1, "_id")) %>%
          unique()

        tempUnitsIDs <- tempUnits %>%
          left_join(parentID) %>%
          mutate(idParent = id) %>%
          select(-id) %>%
          left_join(unitID) %>%
          select(!!paste0("al", j-1, "_name"),
                 !!paste0("al", j-1, "_id") := idParent,
                 !!paste0("al", j-1, "_alt") := parent,
                 !!paste0("al", j, "_name"),
                 !!paste0("al", j, "_id") := idUnit,
                 !!paste0("al", j, "_alt") := unit
                 )

        # In the third step the geometries/outputUnits are pruned down to the
        # available units by left-joining to tempUnits via the respective
        # al*_id
        outputUnits <- tempUnitsIDs %>%
          left_join(outputUnits)

      }
    }

    hIDJoin <- outputUnits %>%
      filter(level %in% max(levels)) %>%
      select(matches("_alt"), ahID)

    outhIDs <- bind_rows(outhIDs, hIDJoin)
  }

  for(i in seq_along(adminLvls)){
    pos <- which(colnames(out) == adminLvls[[i]])
    colnames(out)[pos] <- paste0(names(adminLvls)[i], "_alt")
  }

  if(!keepOrig){
    out <- out %>%
      left_join(outhIDs) %>%
      select(-contains("_alt"))
  } else{
    out <- out %>%
      left_join(outhIDs)
  }

  return(out)
}
