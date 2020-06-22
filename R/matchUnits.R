#' Determine the administrative hierarchy ID
#'
#' This function matches territorial units with a list of known units to derive
#' the administrative hierarchy ID.
#' @param input [\code{data.frame(1)}]\cr table in which to match administrative
#'   units.
#' @param source [\code{integerish(1)}]\cr the geometry ID (\code{geoID}) from
#'   which the terms have been taken.
#' @param verbose [\code{logical(1)}]\cr be verbose about what is happening
#'   (default \code{FALSE}).
#' @details \code{names(input)} must contain at least the \code{al1 = ...} to
#'   match at least a certain nation. Further administrative levels are denoted
#'   by column names \code{al2}, \code{al3}, ...
#' @return The table provided in \code{input}, where the given columns are
#'   replaced by the column \code{ahID}, which contains the administrative
#'   hierarchy ID.
#' @importFrom checkmate assertCharacter assertIntegerish assertList
#'   assertDataFrame
#' @importFrom stringr str_extract
#' @importFrom dplyr filter pull mutate mutate_at bind_rows bind_cols vars
#'   any_vars all_vars ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang exprs :=
#' @importFrom sf st_geometry<- read_sf st_layers
#' @importFrom stats setNames
#' @importFrom tibble tibble
#' @importFrom tidyselect matches everything contains all_of
#' @importFrom utils tail txtProgressBar setTxtProgressBar

matchUnits <- function(input = NULL, source = NULL, verbose = FALSE){

  # set internal objects
  intPaths <- paste0(getOption(x = "adb_path"), "/adb_geometries/")

  # check validity of arguments
  assertDataFrame(x = input)
  assertList(x = source, len = 1)
  # assert that one of the columnnames is "al1" so that at least nations are matched
  assertNames(x = names(input), must.include = "al1")

  tempOut <- input
  adminLvls <- names(input)[grepl(pattern = "al", x = names(input))]

  # check whether all adminLvls are there
  levels <- sort(as.integer(str_extract(adminLvls, "[[:digit:]]")))
  missingLevels <- setdiff(1:max(levels), levels)
  if(length(missingLevels) != 0){
    missingLevels <- paste0("al", missingLevels)
    newLvls <- paste0("al", 1:max(levels))
  } else {
    missingLevels <- NULL
    newLvls <- adminLvls
  }

  # to manage the workload, split up input according to nations ("al1")
  nations <- unique(eval(parse(text = "al1"), envir = input))

  # test whether there is a file to match with, by the nation name
  availableNations <- list.files(path = paste0(intPaths, "stage3"), full.names = TRUE) %>%
    str_split(pattern = "/")
  availableNations <- sapply(seq_along(availableNations), function(x){
    temp <- availableNations[[x]]
    str_split(tail(temp, 1), "[.]")[[1]][1]
  })

  outhIDs <- NULL
  message("--> matching geometries of ...")
  for(i in seq_along(nations)){

    recentNation <- nations[i]

    # make an input subset for the current nation ...
    inputSbst <- input %>%
      filter(al1 == recentNation)

    # ... extract and find the unique combinations of the respective columns,
    # this asserts that only administrative units nested in their parents are
    # found
    allInputUnits <- inputSbst %>%
      select(all_of(adminLvls)) %>%
      unique()

    # load the nation geometries ...
    if(!recentNation %in% availableNations){
      message("    ... skipping '", recentNation, "' (no geoemtries available)")
      next
    } else {
      message("    ... '", recentNation, "'")
      layers <- st_layers(dsn = paste0(intPaths, "stage3/", recentNation, ".gpkg"))
    }
    geometries <- NULL
    for(j in seq_along(layers$name)){
      theGeom <- read_sf(dsn = paste0(intPaths, "stage3/", recentNation, ".gpkg"),
                         layer = sort(layers$name)[j],
                         stringsAsFactors = FALSE) %>%
        as_tibble() %>%
        select(-geom)
      geometries <- bind_rows(geometries, theGeom)
    }

    # ... select only unique rows of all 'al*_id' ...
    temp <- geometries %>%
      select(name, starts_with("al"))
    geometries <- geometries %>%
      filter(!duplicated(temp))

    outputUnits <- geometries
    levels <- NULL
    for(j in seq_along(newLvls)){
      theLevel <- as.integer(sub(pattern = "\\D*(\\d+).*",
                                 replacement = "\\1",
                                 x = newLvls)[j])
      message("        ... at level ", theLevel)
      levels <- c(levels, theLevel)

      if(j == 1){
        if(length(adminLvls) == 1){
          outputUnits <- outputUnits %>%
            filter(name == recentNation) %>%
            as_tibble() %>%
            mutate(al1_alt = recentNation) %>%
            select(al1_name = name, al1_alt, level, ahID)
        } else{
          next
        }
      } else {

        # In a first step we need to build a table of the units and their
        # parents, both with the standard name of our database ('geometries'),
        # then ...
        unitSubset <- geometries %>%
          filter(level == theLevel)
        parentSubset <- geometries %>%
          filter(level == theLevel-1)

        # ... select terms of the current level and its parent
        inputUnits <- NULL
        if(!is.null(missingLevels)){
          if(missingLevels == paste0("al", j)){
            # if the current level is missing, take unit names from 'geometries'
            parentInput <- unique(parentSubset$name)
            unitInput <- unique(geometries$name[geometries$level == theLevel])
          } else if(missingLevels == paste0("al", j-1)){
            # if the parent level is missing, take parent names from 'geometries'
            parentInput <- unique(geometries$name[geometries$level == theLevel-1])
            unitInput <- allInputUnits %>%
              pull(paste0("al", j)) %>%
              unique()
          }
        } else {
          inputUnits <- allInputUnits %>%
            select(parent = paste0("al", j-1), unit = paste0("al", j)) %>%
            unique()

          parentInput <- inputUnits$parent
          unitInput <- inputUnits$unit
        }

        # ... translate them to the default unit names
        theParents <- translateTerms(terms = unique(parentInput),
                                     index = "tt_territories",
                                     source = source,
                                     fuzzy_terms = unique(parentSubset$name),
                                     verbose = FALSE)
        theParents <- unique(theParents)
        theUnits <- translateTerms(terms = unique(unitInput),
                                   index = "tt_territories",
                                   source = source,
                                   fuzzy_terms = unique(unitSubset$name),
                                   verbose = verbose)
        theUnits <- unique(theUnits)

        if(is.null(inputUnits)){
          inputUnits <- theUnits %>%
            left_join(geometries, by = c("target" = "name")) %>%
            filter(level == theLevel) %>%
            select(unit = origin, !!paste0("al", j, "_name") := target, starts_with("al"))
          inputParents <- theParents %>%
            left_join(geometries, by = c("target" = "name")) %>%
            filter(level == theLevel-1) %>%
            select(parent = origin, !!paste0("al", j-1, "_name") := target, starts_with("al"), -paste0("al", j, "_id"))

          inputUnits <- inputUnits %>%
            left_join(inputParents) %>%
            select(parent = paste0("al", j-1, "_name"), unit)
        }

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

        tempUnitsIDs <- suppressMessages(
          tempUnits %>%
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
            ))

        # In the third step the geometries/outputUnits are pruned down to the
        # available units by left-joining to tempUnits via the respective
        # al*_id
        outputUnits <- suppressMessages(
          tempUnitsIDs %>%
            left_join(outputUnits))

      }
    }

    hIDJoin <- outputUnits %>%
      filter(level %in% max(levels)) %>%
      select(starts_with("al"), ahID)

    outhIDs <- bind_rows(outhIDs, hIDJoin)
  }

  for(i in seq_along(adminLvls)){
    pos <- which(colnames(tempOut) == adminLvls[i])
    colnames(tempOut)[pos] <- paste0(adminLvls[i], "_alt")
  }

  out <- suppressMessages(
    tempOut %>%
      left_join(outhIDs) %>%
      select(-contains("_alt"))
  )

  return(out)
}
