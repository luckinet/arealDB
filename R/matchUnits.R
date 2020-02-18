#' Determine the administrative hierarchy ID
#'
#' This function matches territorial units with a list of known units to derive
#' the administrative hierarchy ID.
#' @param input [\code{data.frame(1)}]\cr table in which to match administrative
#'   units.
#' @param source [\code{integerish(1)}]\cr the geometry ID (\code{geoID}) from
#'   which the terms have been taken.
#' @param keepOrig [\code{logical(1)}]\cr to keep the original units in the
#'   output (\code{TRUE}) or to remove them (\code{FALSE}, default). Useful for
#'   debugging.
#' @details \code{names(input)} must contain at least the \code{al1 = ...} to
#'   match at least a certain nation. Further administrative levels are denoted
#'   by column names \code{al2}, \code{al3}, ...
#' @return The table provided in \code{input}, where the given columns are
#'   replaced by the column \code{ahID}, which contains the administrative
#'   hierarchy ID.
#' @importFrom checkmate assertCharacter assertIntegerish assertList
#'   assertDataFrame
#' @importFrom dplyr filter pull mutate mutate_at bind_rows bind_cols vars
#'   any_vars all_vars ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang exprs :=
#' @importFrom sf st_geometry<- read_sf
#' @importFrom stats setNames
#' @importFrom tibble tibble
#' @importFrom tidyselect matches everything contains
#' @importFrom utils tail txtProgressBar setTxtProgressBar
#' @export

matchUnits <- function(input = NULL, source = NULL, keepOrig = FALSE){

  # set internal objects
  intPaths <- paste0(getOption(x = "adb_path"), "/adb_geometries/")

  # check validity of arguments
  assertDataFrame(x = input)
  assertIntegerish(x = source)
  assertLogical(x = keepOrig)
  # assert that one of the columnnames is "al1" so that at least nations are matched
  assertNames(x = names(input), must.include = "al1")

  out <- input
  adminLvls <- names(input)[grepl(pattern = "al", x = names(input))]

  # to manage the workload, split up input according to nations ("al1")
  theNations <- unique(eval(parse(text = "al1"), envir = input))
  nations <- translateTerms(terms = theNations,
                            index = "tt_territories",
                            source = list("tabID" = source),
                            inline = FALSE,
                            verbose = FALSE) %>%
    filter(!target %in% "ignore")

  # test whether there is a file to match with, by the nation name
  availableNations <- list.files(path = paste0(intPaths, "stage3"), full.names = TRUE) %>%
    str_split(pattern = "/")
  availableNations <- sapply(seq_along(availableNations), function(x){
    temp <- availableNations[[x]]
    str_split(tail(temp, 1), "[.]")[[1]][1]
  })

  outhIDs <- NULL
  message("--> matching geometries of ...")
  for(i in seq_along(nations$target)){
    recentNation <- nations[i,]

    if(recentNation$target %in% c("missing")){
      message("    ... skipping '", recentNation$origin, "' ('missing' in translation table)")
      next
    } else if(recentNation$target %in% c("ignore")){
      message("    ... skipping '", recentNation$origin, "' ('ignore' in translation table)")
      next
    } else {
      message("    ... '", recentNation$target, "'")
    }


    # make an input subset for the current nation ...
    inputSbst <- input %>%
      filter(al1 == recentNation$origin)

    # ... extract and find the unique combinations of the respective columns,
    # this asserts that only administrative units nested in their parents are
    # found
    allInputUnits <- inputSbst %>%
      select(adminLvls) %>%
      unique()

    # load the nation geometries ...
    if(!recentNation$target %in% availableNations){
      message("    ... skipping '", recentNation$origin, "' (no geoemtries available)")
      next
    } else {
      layers <- st_layers(dsn = paste0(intPaths, "stage3/", recentNation$target, ".gpkg"))
    }
    geometries <- NULL
    for(j in seq_along(layers$name)){
      theGeom <- read_sf(dsn = paste0(intPaths, "stage3/", recentNation$target, ".gpkg"),
                         layer = sort(layers$name)[j],
                         stringsAsFactors = FALSE) %>%
        as_tibble() %>%
        select(-geom)
      geometries <- bind_rows(geometries, theGeom)
    }

    # ... and select only unique rows of all 'al*_id'
    temp <- geometries %>%
      select(name, starts_with("al"))
    geometries <- geometries %>%
      filter(!duplicated(temp))

    outputUnits <- geometries
    levels <- NULL
    for(j in seq_along(adminLvls)){
      theLevel <- as.integer(sub(pattern = "\\D*(\\d+).*",
                                 replacement = "\\1",
                                 x = adminLvls)[j])
      levels <- c(levels, theLevel)

      if(j == 1){
        if(length(adminLvls) == 1){
          outputUnits <- outputUnits %>%
            filter(name == recentNation$target) %>%
            as_tibble() %>%
            mutate(al1_alt = recentNation$origin) %>%
            select(al1_name = name, al1_alt, level, ahID)
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
                                     index = "tt_territories",
                                     source = list("tabID" = source),
                                     fuzzy_terms = unique(parentSubset$name),
                                     verbose = FALSE)
        # this seems to give a NAs introduced by coercion for "brazil" of "schema_54"
        theParents <- unique(theParents)
        theUnits <- translateTerms(terms = unique(inputUnits[[2]]),
                                   index = "tt_territories",
                                   source = list("tabID" = source),
                                   fuzzy_terms = unique(unitSubset$name),
                                   verbose = FALSE)
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
      select(starts_with("al"), ahID)

    outhIDs <- bind_rows(outhIDs, hIDJoin)
  }

  for(i in seq_along(adminLvls)){
    pos <- which(colnames(out) == adminLvls[i])
    colnames(out)[pos] <- paste0(adminLvls[i], "_alt")
  }

  if(!keepOrig){
    out <- out %>%
      left_join(outhIDs) %>%
      select(-starts_with("al"))
  } else{
    out <- out %>%
      left_join(outhIDs) %>%
      select(-contains("_alt"))
  }

  return(out)
}
