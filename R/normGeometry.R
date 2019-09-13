#' Normalise geometries
#'
#' Harmonise and integrate geometries in a standardised format
#' @param input [\code{character(1)}]\cr path of the file to normalise.
#' @param ... [\code{character(.)}]\cr a subset of administrative units as given
#'   by \code{nation}, \code{continent}, \code{region}, \code{subregion} or
#'   \code{un_member = TRUE/FALSE}. Valid values can be found in the object
#'   \code{\link{countries}}.
#' @param thresh [\code{integerish(1)}]\cr the threshold of percentage of
#'   overlap above which to consider two territorial units "the same".
#' @param update [\code{logical(1)}]\cr whether or not the physical files should
#'   be updated (\code{TRUE}) or the function should merely return the new
#'   object (\code{FALSE} default).
#' @param verbose [\code{logical(1)}]\cr be verbose about what is happening
#'   (default \code{TRUE}).
#' @details To normalise geometries, this function proceeds as follows:
#'   \enumerate{ \item Read in \code{input} and extract initial metadata from
#'   the file name. \item Loop through every nation that shall be processed and
#'   carry out the following steps: \itemize{ \item In case the geometries are
#'   provided as a list of simple feature POLYGONS, they are dissolved into a
#'   single MULTIPOLYGON per nation. \item In case the nation to which a
#'   geometry belongs has not yet been created at stage three, the following
#'   steps are carried out: \enumerate{ \item Check whether initial GADM
#'   geometries of the required level are registered and normalised, and stop if
#'   this is not the case. \item Extract the full hierarchy of all territorial
#'   units that are part of the geometry. \item Match all territorial units of
#'   the geometry within their respective parents with the hierarchy table of
#'   the GADM geometries. \item Reconstruct ahID from the intermediate spatial
#'   objects and from the metadata. } \item In case the nation to which the
#'   geometry belongs has already been created at stage three, the following
#'   steps are carried out: \enumerate{ \item Check whether the new geometries
#'   have the same coordinate reference system as the already existing database
#'   and reproject if this is not the case. \item Check whether all new
#'   geometries are already exactly matched spatially and stop if that is the
#'   case. \item Carry out a spatial join (via \code{sf::st_join(... largest =
#'   TRUE)}). \item Match all territorial units of the geometry within their
#'   respective parents with the previous hierarchy table. \item For all units
#'   that did match, reconstruct ahID from the intermediate spatial objects and
#'   from the metadata. \item For all units that did not match, rebuild metadata
#'   and a new ahID. } \item If update = TRUE, store the processed geometry at
#'   stage three.} \item Move the geometry to the folder '/processed', if it is
#'   fully processed.}
#' @family normalisers
#' @return This function integrates unprocessed geometries at stage two into the
#'   geospatial database.
#' @examples
#' \dontrun{
#'
#' normGeometry(input = ".../adb_geometries/stage2/geometry.gpkg",
#'              nation = c("united states"),
#'              update = TRUE, verbose = FALSE)
#'
#' }
#' @importFrom checkmate assertFileExists assertIntegerish assertLogical
#'   assertCharacter assertChoice testFileExists
#' @importFrom dplyr filter distinct select mutate rowwise filter_at vars
#'   all_vars pull group_by arrange summarise mutate_if rename n if_else
#' @importFrom rlang sym
#' @importFrom readr read_csv
#' @importFrom sf st_layers read_sf st_write st_join st_buffer st_equals st_sf
#'   st_transform st_crs st_geometry_type st_area st_intersection
#' @importFrom stringr str_split
#' @importFrom tibble as_tibble
#' @importFrom tidyr unite
#' @importFrom tidyselect starts_with
#' @export

normGeometry <- function(input, ..., thresh = 90, update = FALSE, verbose = TRUE){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))

  # set internal objects
  subsets <- exprs(..., .named = TRUE)
  moveFile <- TRUE

  # get tables
  inv_geometries <- read_csv(paste0(intPaths, "/inv_geometries.csv"), col_types = "iciccccDcc")

  # check validity of arguments
  assertNames(x = colnames(inv_geometries), permutation.of = c("geoID", "datID", "level", "source_file", "layer", "nation_column", "unit_column", "date", "orig_file", "notes"))
  assertList(x = subsets)
  assertIntegerish(x = thresh, any.missing = FALSE)
  assertFileExists(x = input, access = "r")
  assertLogical(x = update, len = 1)
  assertLogical(x = verbose, len = 1)

  # scrutinize file-name (the fields, which are delimited by "_" carry important information)
  pathStr <- str_split(input, "/")[[1]]
  file_name <- pathStr[length(pathStr)]
  fields <- str_split(file_name, "_")[[1]]

  # get some variables
  lut <- inv_geometries[grep(pattern = paste0("^", file_name, "$"), x = inv_geometries$source_file),]
  if(file_name %in% lut$source_file){
    newGID <- lut$geoID
    theLayer <- lut$layer
    theLevel <- lut$level
    natCol <- lut$nation_column
    unitCol <- lut$unit_column

    # if there are several columns that contain units, split them and make
    # 'unitCol' the last value
    if(grepl(pattern = "\\|", x = unitCol)){
      unitCols <- str_split(string = unitCol, pattern = "\\|")[[1]]
      unitCol <- unitCols[length(unitCols)]
    } else{
      unitCols <- unitCol
    }
    natCol <- ifelse(is.na(natCol), unitCols[1], natCol)
  } else{
    stop(paste0("  ! the file '", file_name, "' has not been registered yet."))
  }

  # read the object
  message("--> reading new geometries from '", file_name, "' ...")
  newLayers <- st_layers(dsn = input)
  newGeom <- read_sf(dsn = input,
                     layer = theLayer,
                     stringsAsFactors = FALSE)

  # determine nation value
  if(fields[1] == ""){
    severalNations <- TRUE
    nationCol <- lut$nation_column

    theNations <- unique(eval(expr = parse(text = nationCol),
                              envir = newGeom)) %>%
      as.character()
    assertCharacter(x = theNations, min.len = 1, any.missing = FALSE)
    nations <- translateTerms(terms = theNations,
                              index = "tt_nations",
                              source = list("geoID" = newGID),
                              verbose = verbose) %>%
      mutate(target = if_else(target == "ignore", NA_character_, target)) %>%
      pull(target)

  } else{
    severalNations <- FALSE
    nation <- toupper(fields[1])
    assertChoice(x = nation, choices = countries$iso_a3)
    nations <- countries %>%
      filter(iso_a3 == !!nation) %>%
      pull(nation)
    theNations <- nations
  }

  # only process existing nations
  theNations <- theNations[!is.na(nations)]
  nations <- nations[!is.na(nations)]

  # only process nations that are part of 'countries'
  theNations <- theNations[nations %in% countries$nation]
  nations <- nations[nations %in% countries$nation]

  # potentially subset nation values
  if(length(subsets) > 0){
    assertChoice(x = names(subsets), choices = c("nation", "un_member", "continent", "region", "subregion"))

    # unify also the nations with which to subset
    if(any(names(subsets) == "nation")){
      toUnify <- eval(subsets[[which(names(subsets) == "nation")]])
      unified <- translateTerms(terms = toUnify,
                                index = "tt_nations",
                                source = list("geoID" = newGID),
                                verbose = verbose) %>%
        pull(target)
      subsets[[which(names(subsets) == "nation")]] <- unified
    }
      filter_at(vars(!!names(subsets)), any_vars(. %in% as.character(subsets[[1]]))) %>%
      pull(nation)
    subNationInd <- which(nations %in% subNations)
    nations <- nations[subNationInd]
    theNations <- theNations[subNationInd]

    # if 'subsets' is not empty, not all geometries are processed and thuse we
    # don't want to move the file to './processed'
    if(severalNations){
      moveFile <- FALSE
    }
  }

  if(length(nations) == 0){
    moveFile <- FALSE
    message("    New geometries not part of subset.")
  } else {

    # then we loop through all nations
    for(i in seq_along(nations)){

      tempNation <- nations[i]
      nationID <- as.integer(countries$ahID[countries$nation == tempNation])
      message(paste0(" -> processing '", tempNation, "' ..."))

      # create a geom specifically for the recent nation
      if(severalNations){
        sourceGeom <- newGeom %>%
          filter_at(vars(nationCol), all_vars(. %in% theNations[i])) %>%
          select(unitCols)
        assertChoice(x = natCol, choices = names(sourceGeom), .var.name = "names(nation_column)")
      } else{
        sourceGeom <- newGeom %>%
          select(unitCols)
      }

      # in case the object consists of several POLYGONs per unique name, dissolve
      # them into a single MULTIPOLYGON
      if(unique(st_geometry_type(sourceGeom)) == "POLYGON"){
        uniqueUnits <- sourceGeom %>%
          as_tibble() %>%
          select(!!unitCols) %>%
          unique()

        if(all(!is.na(uniqueUnits))){
          if(dim(sourceGeom)[1] > dim(uniqueUnits)[1]){
            message("    Dissolving multiple polygons into a single multipolygon")

            sourceGeom <- sourceGeom %>%
              group_by(.dots = unitCols) %>%
              summarise() %>%
              ungroup()
          } else {
            sourceGeom <- sourceGeom %>%
              select(!!unitCols)
          }
        } else{
          message("  ! The geometry contains only POLYGON features but no unique names to summarise them.")
        }
      }

      # determine whether a geometry with the nation as name already exists and
      # whether that contains the correct layer ...
      fileExists <- testFileExists(x = paste0(intPaths, "/adb_geometries/stage3/", tempNation, ".gpkg"))
      if(fileExists){
        targetLayers <- st_layers(dsn = paste0(intPaths, "/adb_geometries/stage3/", tempNation, ".gpkg"))
        if(!grepl(pattern = paste0("level_", theLevel), x = paste0(targetLayers$name, collapse = "|"))){
          fileExists <- FALSE
        }
      }

      # ... if yes, read it in, otherwise create it
      if(fileExists){

        message("    Reading target geometries")
        targetGeom <- read_sf(dsn = paste0(intPaths, "/adb_geometries/stage3/", tempNation, ".gpkg"),
                              layer = sort(targetLayers$name)[theLevel],
                              stringsAsFactors = FALSE)
        if(theLevel > 1){
          parentGeom <- read_sf(dsn = paste0(intPaths, "/adb_geometries/stage3/", tempNation, ".gpkg"),
                                layer = sort(targetLayers$name)[theLevel-1],
                                stringsAsFactors = FALSE)
        } else {
          parentGeom <- NULL
        }

        # reproject 'sourceGeom' to targetGeom CRS
        if(st_crs(sourceGeom) != st_crs(targetGeom)){
          message("    Reprojecting new geometries")
          sourceGeom <- st_transform(x = sourceGeom, crs = st_crs(targetGeom))
        }

        # test whether/which of the new features are already (spatially) in the target
        # Geom and stop if all of them are there already.
        message("    Checking for exact spatial matches")
        equals <- unlist(st_equals(sourceGeom, targetGeom))
        if(length(equals) == dim(sourceGeom)[1]){
          message("  ! --> all features of the new geometry are already part of the target geometry !")
          next
        }

        # determine the spatial overlap
        message("    Determining spatial overlap with source geometries")
        overlapTarget <- suppressMessages(suppressWarnings(
          sourceGeom %>%
            mutate(source_area = st_area(.)) %>%
            st_buffer(dist = 0) %>% # is needed sometimes to clarify "self-intersection" problems: https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec
            st_intersection(y = targetGeom) %>%
            mutate(overlap = as.numeric(st_area(.)/source_area*100)) %>%
            as_tibble() %>%
            select(-geom, -source_area) %>%
            group_by(.dots = unitCols) %>%
            filter(overlap == max(overlap)) %>%
            ungroup()
        ))

        wipGeom <- sourceGeom %>%
          as_tibble() %>%
          left_join(overlapTarget) %>%
          mutate(overlap = if_else(is.na(overlap), 0, overlap)) %>%
          st_sf()

        # get valid geoms that have an overlap larger than the threshold
        message("    Joining target and source geometries")
        validOverlap <- wipGeom$overlap > thresh

        validUnits <- wipGeom %>%
          filter(validOverlap) %>%
          mutate(geoID = newGID) %>%
          select(-!!unitCols, -overlap)

        # get geoms that are invalid because their overlap is smaller than
        # threshold
        invalidUnits <- wipGeom %>%
          filter(!validOverlap)

        newCols <- names(targetGeom)
        newCols <- newCols[-which(newCols %in% c("geom", "nation", unitCols))]
        if(dim(invalidUnits)[1] > 0){
          invalidUnits[, newCols] <- NA_character_
        }
        invalidUnits <- invalidUnits %>%
          mutate(level = as.integer(level),
                 ahID = as.integer(ahID),
                 geoID = as.integer(geoID),
                 nation = tempNation) %>%
          mutate_at(vars(starts_with("al")), as.integer)

        # this is a test that should ideally never be true, it would mean that there
        # was an issue with matching the nation name at some point in the pipeline.
        if(length(unique(validUnits$al1_id)) > 1){
          stop("The first administrative level ID is not unique.")
        } else{
          al1ID <- unique(validUnits$al1_id)
        }

        # new units ----
        # determine how many units there are per parent unit
        message("    Reconstructing IDs")
        if(theLevel > 1){
          prevUnits <- targetGeom %>%
            as_tibble() %>%
            select(starts_with("al"), -geom) %>%
            group_by(!!sym(paste0("al", theLevel-1, "_id"))) %>%
            mutate(prevUnits = as.numeric(max(!!sym(paste0("al", theLevel, "_id"))))) %>%
            ungroup() %>%
            select(-!!sym(paste0("al", theLevel, "_id"))) %>%
            unique()

          # also merge the parent units into this, in case there are already
          # additional units availables
          prevUnits <- parentGeom %>%
            as_tibble() %>%
            select(starts_with("al")) %>%
            unique() %>%
            left_join(prevUnits) %>%
            mutate(prevUnits = if_else(is.na(prevUnits), 0, prevUnits))
        } else{
          prevUnits <- tibble(!!natCol := tempNation, prevUnits = 0)
        }

        # determine IDs from the parent level to be able to assign them to units
        # at the current level that can't be matched with the spatial join above
        #
        # via given names (which only makes sense if the number of unitCols is
        # the same as theLevel)
        if(length(unitCols) == theLevel){
          if(theLevel == 1){
            prevIDs <- tibble(!!natCol := tempNation, al1_id = nationID)
          } else if(theLevel == 2){
            prevIDs <- invalidUnits %>%
              as_tibble() %>%
              select(-nation, -name, -ahID, -geoID, -level, -starts_with("al"), -overlap, -geom) %>%
              mutate_if(is.character, tolower) %>%
              mutate(al1_id = nationID)
          } else {
            prevIDs <- parentGeom %>%
              as_tibble() %>%
              rename(!!natCol := nation, !!unitCols[length(unitCols)-1] := name) %>%
              select(-geoID, -geom, -level, -ahID) %>%
              filter_at(vars(!!unitCols[-length(unitCols)]), any_vars(!duplicated(.))) %>%
              unique()
          }
        } else {
          # if that is not the case, we need to fall back to finding parents IDs
          # via spatial overlap.
          if(theLevel == 1){
            prevIDs <- tibble(!!natCol := tempNation, al1_id = nationID)
          } else {

            matchGeoms <- invalidUnits %>%
              select(unitCols) %>%
              mutate_if(is.character, tolower)

            message("    Determining parent IDs spatially")
            prevIDs <- suppressMessages(suppressWarnings(
              matchGeoms %>%
                mutate(source_area = st_area(.)) %>%
                st_intersection(y = parentGeom) %>%
                mutate(overlap = as.numeric(st_area(.)/source_area*100)) %>%
                as_tibble() %>%
                group_by(.dots = unitCols, source_area) %>%
                filter(overlap == max(overlap)) %>%
                select(-geoID) %>%
                unique() %>%
                ungroup() %>%
                select(-source_area, -nation, -name, -level, -ahID, -overlap, -geom) %>%
                unique()
            ))

          }
        }

        # if prevUnits is NA, fill it with 0
        if(dim(prevUnits)[1] == 1 & all(is.na(prevUnits$prevUnits))){
          prevUnits$al1_id <- prevIDs$al1_id
          prevUnits$prevUnits <- 0
        }

        # make values for unmatched geometries
        if(theLevel == 1){
          groupLevel <- 2
        } else {
          groupLevel <- theLevel
        }
        suppressMessages(
          newUnits <- invalidUnits %>%
            as_tibble() %>%
            select(-ahID, -geoID, -level, -name, -starts_with("al"), -overlap)  %>%
            mutate_if(is.character, tolower) %>%
            left_join(prevIDs) %>%
            left_join(prevUnits) %>%
            filter(!duplicated(.)) %>%
            group_by(.dots = paste0("al", groupLevel-1, "_id")) %>%
            mutate(nation = tempNation,
                   name = {if (n() > 0) translateTerms(terms = !!as.symbol(unitCols[length(unitCols)]),
                                                       index = "tt_territories",
                                                       source = list("geoID" = newGID),
                                                       verbose = FALSE)$target else ""},
                   level = theLevel,
                   tempID = seq_along(!!sym(unitCols[length(unitCols)]))) %>%
            ungroup() %>%
            mutate(!!paste0("al", theLevel, "_id") := {if (theLevel == 1) nationID else tempID + prevUnits}) %>%
            ungroup() %>%
            st_sf()
        )

        # in case 'joinedGeom' contains a nation that did not properly match, also
        # al1_id will have been reconstructed by the above code. As we know that
        # this ID does not change, we can re-assign it from 'targetGeom'.
        if(theLevel == 1 & dim(newUnits)[1] == 1){
          if(newUnits$al1_id != unique(targetGeom$al1_id)){
            newUnits$al1_id <- unique(targetGeom$al1_id)
          }
        }

        # reconstruct 'ahID'
        newUnits <- newUnits %>%
          mutate(ahID = paste0({if("al1_id" %in% names(.)) formatC(al1_id, width = 3, flag = 0) else ""},
                               {if("al2_id" %in% names(.)) formatC(al2_id, width = 3, flag = 0) else ""},
                               {if("al3_id" %in% names(.)) formatC(al3_id, width = 3, flag = 0) else ""},
                               {if("al4_id" %in% names(.)) formatC(al4_id, width = 3, flag = 0) else ""},
                               {if("al5_id" %in% names(.)) formatC(al5_id, width = 3, flag = 0) else ""},
                               {if("al6_id" %in% names(.)) formatC(al6_id, width = 3, flag = 0) else ""}),
                 geoID = newGID) %>%
          select(-prevUnits, -tempID, -!!unitCols)

        # combine old and new units and rebuild columns
        outGeom <- validUnits %>%
          rbind(newUnits) %>%
          select(nation, name, level, ahID, geoID, everything())

        outGeom <- targetGeom %>%
          rbind(outGeom) %>%
          arrange(ahID)

      } else {

        # check whether a gadm geometry has been registered previously
        if(!grepl(pattern = "gadm", x = lut$source_file)){
          message(paste0("  ! I didn't find any geometries, please provide first the GADM level ", theLevel, " data product !"))
          next
        }

        # get all gadm inventory entries
        gadmIDs <- inv_geometries$geoID[grep(pattern = "gadm", x = inv_geometries$source_file)]

        # get ahID and other information from the parent layer, if it is the first
        # level, just set up new information
        if(theLevel > 1){
          if(theLevel == 2){
            parentIDs <- read_sf(dsn = paste0(intPaths, "/adb_geometries/stage3/", tempNation, ".gpkg"),
                                 layer = sort(targetLayers$name)[theLevel-1],
                                 stringsAsFactors = FALSE) %>%
              as_tibble() %>%
              filter(geoID %in% gadmIDs) %>%
              rename(NAME_0 = nation) %>%
              select(-geom)
          } else {
            parentIDs <- read_sf(dsn = paste0(intPaths, "/adb_geometries/stage3/", tempNation, ".gpkg"),
                                 layer = sort(targetLayers$name)[theLevel-1],
                                 stringsAsFactors = FALSE) %>%
              as_tibble() %>%
              filter(geoID %in% gadmIDs) %>%
              rename(NAME_0 = nation, !!unitCols[length(unitCols)-1] := name) %>%
              select(-geom)
          }
        } else {
          parentIDs <- tibble(NAME_0 = tempNation, NAME_1 = tempNation)
          sourceGeom$NAME_1 <- tempNation
          orig_units <- unitCols
          unitCols <- c("NAME_0", "NAME_1")
        }

        # for joining the geom(s) to modified objects
        matchGeom <- sourceGeom %>%
          select(!!unitCols) %>%
          mutate_if(is.character, tolower)

        # join the object with the parent ID to derive IDs of this level that are
        # actual children of the correct parent
        suppressMessages(joinedGeom <- sourceGeom %>%
                           as_tibble() %>%
                           select(-geom)  %>%
                           mutate_if(is.character, tolower) %>%
                           left_join(parentIDs) %>%
                           select(!!unitCols, starts_with("al")) %>%
                           left_join(matchGeom) %>%
                           st_sf())

        xyz <- unitCols[!seq_along(unitCols) %in% c(1, length(unitCols))]

        # derive required information
        outGeom <- joinedGeom %>%
          group_by(!!as.symbol(unitCols[length(unitCols)-1])) %>%
          mutate(level = theLevel,
                 !!paste0("al", theLevel, "_id") := {if(theLevel == 1) nationID else seq_along(NAME_0)},
                 geoID = newGID
          ) %>%
          mutate(ahID = paste0({if("al1_id" %in% names(.)) formatC(al1_id, width = 3, flag = 0) else ""},
                               {if("al2_id" %in% names(.)) formatC(al2_id, width = 3, flag = 0) else ""},
                               {if("al3_id" %in% names(.)) formatC(al3_id, width = 3, flag = 0) else ""},
                               {if("al4_id" %in% names(.)) formatC(al4_id, width = 3, flag = 0) else ""},
                               {if("al5_id" %in% names(.)) formatC(al5_id, width = 3, flag = 0) else ""},
                               {if("al6_id" %in% names(.)) formatC(al6_id, width = 3, flag = 0) else ""})
          ) %>%
          ungroup() %>%
          select(nation = NAME_0, name = !!unitCols[length(unitCols)], level, ahID, geoID, everything(), -xyz) %>%
          mutate(name = tolower(name))

        if(theLevel == 1){
          unitCols <- orig_units
        }
      }

      if(update){
        # in case the user wants to update, output the simple feature
        st_write(obj = outGeom,
                 dsn = paste0(intPaths, "/adb_geometries/stage3/", tempNation, ".gpkg"),
                 layer = paste0("level_", theLevel),
                 layer_options = "OVERWRITE=yes",
                 quiet = TRUE)
      }
    }
  }

  if(update & moveFile){
    message(paste0("    Moving '", file_name, "' to './stage2/processed'"))
    firstStage <- paste0(intPaths, "/adb_geometries/stage2")
    file.copy(from = paste0(firstStage, "/", file_name), to = paste0(firstStage, "/processed/", file_name))
    file.remove(paste0(firstStage, "/", file_name))
  }

  message()
  return(lut)
}
