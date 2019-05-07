#' Register geometries
#'
#' Harmonise and integrate  geometries in a standardised format
#' @param input [\code{character(1)}]\cr path of the file to register.
#' @param update [\code{logical(1)}]\cr whether or not the physical files should
#'   be updated (\code{TRUE}) or the function should merely return the new
#'   object (\code{FALSE} default).
#' @param ... [\code{}]\cr chose a subset of administrative units by calling
#'   \code{nation}, \code{un_member}, \code{continent}, \code{region} or
#'   \code{subregion}.
#' @importFrom checkmate assertFileExists assertIntegerish assertLogical
#'   assertCharacter assertChoice testFileExists
#' @importFrom dplyr filter distinct select mutate rowwise filter_at vars
#'   all_vars pull group_by arrange summarise mutate_if rename
#' @importFrom rlang sym
#' @importFrom readr read_csv
#' @importFrom sf st_layers read_sf st_write st_join st_buffer st_equals st_sf
#'   st_transform st_crs st_geometry_type
#' @importFrom stringr str_split
#' @importFrom tibble as_tibble
#' @importFrom tidyr unite
#' @importFrom tidyselect starts_with
#' @export

normGeometry <- function(input, update = FALSE, ...){

  # subsets <- list(nation = c("argentina", "brazil", "paraguay", "bolivia")); input <- "/home/se87kuhe/Nextcloud/LUCKINet/data/administrative_boundaries/stage1/_1__spam.gpkg"; update = FALSE

  # set internal paths
  intPaths <- paste0(getOption(x = "dmt_path"))

  # set internal objects
  subsets <- exprs(..., .named = TRUE)
  moveFile <- TRUE

  # get tables
  inv_geometries <- read_csv(paste0(intPaths, "/inv_geometries.csv"), col_types = "iciccccDcc")

  # check validity of arguments
  assertNames(x = colnames(inv_geometries), permutation.of = c("geoID", "name", "level", "source_file", "layer", "nation_column", "unit_column", "date", "orig_file", "notes"))
  assertList(x = subsets)
  assertFileExists(x = input, access = "r")
  assertLogical(x = update, len = 1)

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
  } else{
    stop(paste0("  ! the file '", file_name, "' has not been registered yet."))
  }

  # read the object
  message("--> Reading new geometries from '", file_name, "' ...")
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
    nations <- unifyNations(unify = theNations)

    # only process existing nations
    theNations <- theNations[!is.na(nations)]
    nations <- nations[!is.na(nations)]

    # only process nations that are part of 'countries'
    theNations <- theNations[nations %in% countries$nation]
    nations <- nations[nations %in% countries$nation]
  } else{
    severalNations <- FALSE
    nation <- toupper(fields[1])
    assertChoice(x = nation, choices = countries$iso_a3)
    nations <- countries %>%
      filter(iso_a3 == !!nation) %>%
      pull(nation)
    theNations <- nations
  }

  # potentially subset nation values
  if(length(subsets) > 0){
    assertChoice(x = names(subsets), choices = c("nation", "un_member", "continent", "region", "subregion"))
    subNations <- countries %>%
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

  # then we loop through all nations
  for(i in seq_along(nations)){

    tempNation <- nations[i]
    nationID <- as.integer(countries$ahID[countries$nation == tempNation])
    message(paste0("\n--> processing '", tempNation, "' ..."))

    # create a geom specifically for the recent nation
    if(severalNations){
      nationGeom <- newGeom %>%
        filter_at(vars(nationCol), all_vars(. %in% theNations[i]))
      assertChoice(x = natCol, choices = names(nationGeom), .var.name = "names(nation_column)")
    } else{
      nationGeom <- newGeom
    }

    # in case the object consists of several POLYGONs per unique name, dissolve
    # them into a multipolygon
    if(unique(st_geometry_type(nationGeom)) == "POLYGON"){
      uniqueUnits <- nationGeom %>%
        as_tibble() %>%
        select(!!unitCols) %>%
        unique()

      if(all(!is.na(uniqueUnits))){
        if(dim(nationGeom)[1] > dim(uniqueUnits)[1]){
          message("    Dissolving multiple polygons into a single multipolygon")

          nationGeom <- nationGeom %>%
            group_by(.dots = unitCols) %>%
            summarise() %>%
            ungroup()
        }
      } else{
        message("  ! The geometry contains only POLYGON features but no unique names to summarise them.")
      }
    }

    # determine whether a geometry with the nation as name already exists and
    # whether that contains the correct layer ...
    fileExists <- testFileExists(x = paste0(intPaths, "/cT_geometries/stage2/", tempNation, ".gpkg"))
    if(fileExists){
      targetLayers <- st_layers(dsn = paste0(intPaths, "/cT_geometries/stage2/", tempNation, ".gpkg"))
      if(!grepl(pattern = paste0("level_", theLevel), x = paste0(targetLayers$name, collapse = "|"))){
        fileExists <- FALSE
      }
    }

    # ... if yes, read it in, otherwise create it
    if(fileExists){

      message("    Reading target geometries")
      targetGeom <- read_sf(dsn = paste0(intPaths, "/cT_geometries/stage2/", tempNation, ".gpkg"),
                            layer = sort(targetLayers$name)[theLevel],
                            stringsAsFactors = FALSE)

      # reproject 'nationGeom' to targetGeom CRS
      if(st_crs(nationGeom) != st_crs(targetGeom)){
        message("    Reprojecting new geometries")
        nationGeom <- st_transform(x = nationGeom, crs = st_crs(targetGeom))
      }

      # test whether/which of the new features are already (spatially) in the target
      # Geom and stop if all of them are there already.
      message("    Checking for exact spatial matches")
      equals <- unlist(st_equals(nationGeom, targetGeom))
      if(length(equals) == dim(nationGeom)[1]){
        message("  ! --> all features of the new geometry are already part of the target geometry !")
        next
      }

      # (spatial) join the new geometry with the target geometry
      message("    Carrying out spatial join")
      joinedGeom <- suppressMessages(suppressWarnings( # st_buffer and st_join are too verbose for my taste.
        nationGeom %>%
          # select(name = unitCol, geom) %>%
          select(unitCols, geom) %>%
          # mutate(name = tolower(name)) %>%
          st_buffer(dist = 0) %>% # is needed sometimes to clarify "self-intersection" problems: https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec
          st_join(y = targetGeom, largest = TRUE) %>% # this returns all features of 'nationGeom' with the attributes of 'targetGeom' where the largest spatial overlap between the two exists.
          mutate(geoID = newGID)
      ))

      # determine IDs from the parent level to be able to assign them to units
      # at the current level that can't be matched with the spatial join above
      if(severalNations){
        if(theLevel > 1){
          if(theLevel == 2){
            oldIDs <- read_sf(dsn = paste0(intPaths, "/cT_geometries/stage2/", tempNation, ".gpkg"),
                              layer = sort(targetLayers$name)[theLevel-1],
                              stringsAsFactors = FALSE) %>%
              as_tibble() %>%
              rename(!!natCol := nation) %>%
              select(-geoID, -geom) %>%
              unique()
          } else {
            oldIDs <- read_sf(dsn = paste0(intPaths, "/cT_geometries/stage2/", tempNation, ".gpkg"),
                              layer = sort(targetLayers$name)[theLevel-1],
                              stringsAsFactors = FALSE) %>%
              as_tibble() %>%
              rename(!!natCol := nation, !!unitCols[length(unitCols)-1] := name) %>%
              select(-geoID, -geom) %>%
              unique()
          }

          # determine the maximum number of units for restarting to number later on
          maxUnits <- joinedGeom %>%
            as_tibble() %>%
            select(starts_with("al"), -geom) %>%
            group_by(!!sym(paste0("al", theLevel-1, "_id"))) %>%
            mutate(maxUnits = max(!!sym(paste0("al", theLevel, "_id")))) %>%
            ungroup() %>%
            select(-!!sym(paste0("al", theLevel, "_id"))) %>%
            unique()

        } else {
          oldIDs <- tibble(!!natCol := tempNation)
          maxUnits <- tibble(!!natCol := tempNation, maxUnits = 1)
        }
      } else {
        if(theLevel > 1){
          oldIDs <- read_sf(dsn = paste0(intPaths, "/cT_geometries/stage2/", tempNation, ".gpkg"),
                            layer = sort(targetLayers$name)[theLevel-1],
                            stringsAsFactors = FALSE) %>%
            as_tibble() %>%
            rename(!!unitCols[length(unitCols)] := name) %>%
            select(-geoID, -geom) %>%
            unique()

          # determine the maximum number of units for restarting to number later on
          maxUnits <- joinedGeom %>%
            as_tibble() %>%
            select(starts_with("al"), -geom) %>%
            group_by(!!sym(paste0("al", theLevel-1, "_id"))) %>%
            mutate(maxUnits = max(!!sym(paste0("al", theLevel, "_id")))) %>%
            ungroup() %>%
            select(-!!sym(paste0("al", theLevel, "_id"))) %>%
            unique()

        } else {
          oldIDs <- tibble(!!unitCols[length(unitCols)] := tempNation)
          maxUnits <- tibble(!!unitCols[length(unitCols)] := tempNation, maxUnits = 1)
        }
      }

      newIDs <- joinedGeom %>%
        filter(is.na(nation)) %>%
        select(-nation, -name, -ahID, -geoID, -level, -starts_with("al"))  %>%
        mutate_if(is.character, tolower) %>%
        left_join(oldIDs) %>%
        mutate(level = theLevel)

      # for the old units (those that exist already), we can take all information
      # from the joined columns
      oldUnits <- joinedGeom %>%
        filter(!is.na(nation))

      # test whether joinedGeom/nationGeom contains any spatial units at all
      if(dim(oldUnits)[1] == 0){
        message("  ! --> no features of the new geometry match in the target geometry !")
        next
      }

      # this is a test that should ideally never be true, it would mean that there
      # was an issue with matching the nation name at some point in the pipeline.
      if(length(unique(oldUnits$al1_id)) > 1){
        stop("The first administrative level ID is not unique.")
      } else{
        al1ID <- unique(oldUnits$al1_id)
      }

      # determine units that don't yet exists, because they won't be joined and
      # hence don't have a value in the columns of the target geometry.
      newUnits <- joinedGeom %>%
        as_tibble() %>%
        filter(is.na(nation)) %>%
        select(-geom, -ahID, -geoID, -level, -starts_with("al"))  %>%
        mutate_if(is.character, tolower) %>%
        left_join(newIDs) %>%
        left_join(maxUnits) %>%
        filter(!duplicated(.)) %>%
        st_sf() %>%
        mutate(nation = tempNation,
               name = {if (n() > 0) translateTerms(terms = !!as.symbol(unitCols[length(unitCols)]),
                                                   index = "tt_units",
                                                   verbose = FALSE)$target else ""},
               level = theLevel,
               !!paste0("al", theLevel, "_id") := seq_along(unitCols[length(unitCols)]) + maxUnits)
      newUnits <- newUnits %>%
               mutate(ahID = paste0({if("al1_id" %in% names(.)) formatC(al1_id, width = 3, flag = 0) else ""},
                             {if("al2_id" %in% names(.)) formatC(al2_id, width = 3, flag = 0) else ""},
                             {if("al3_id" %in% names(.)) formatC(al3_id, width = 3, flag = 0) else ""},
                             {if("al4_id" %in% names(.)) formatC(al4_id, width = 3, flag = 0) else ""},
                             {if("al5_id" %in% names(.)) formatC(al5_id, width = 3, flag = 0) else ""},
                             {if("al6_id" %in% names(.)) formatC(al6_id, width = 3, flag = 0) else ""}),
               geoID = newGID) %>%
        select(nation, name, level, ahID, geoID, everything(), -maxUnits)

      # combine old and new units and rebuild columns
      outGeom <- oldUnits %>%
        rbind(newUnits) %>%
        select(nation, name, level, ahID, geoID, everything(), -unitCols)

      outGeom <- targetGeom %>%
        rbind(outGeom) %>%
        arrange(ahID)

    } else {

      if(lut$name != "gadm"){
        message(paste0("  ! I didn't find any geometries, please provide first the GADM level ", theLevel, " data product !"))
        next
      }

      gadmIDs <- inv_geometries$geoID[inv_geometries$name %in% "gadm"]

      if(theLevel > 1){
        if(theLevel == 2){
          oldIDs <- read_sf(dsn = paste0(intPaths, "/cT_geometries/stage2/", tempNation, ".gpkg"),
                            layer = sort(targetLayers$name)[theLevel-1],
                            stringsAsFactors = FALSE) %>%
            as_tibble() %>%
            filter(geoID %in% gadmIDs) %>%
            rename(NAME_0 = nation) %>%
            select(-geom)
        } else {
          oldIDs <- read_sf(dsn = paste0(intPaths, "/cT_geometries/stage2/", tempNation, ".gpkg"),
                            layer = sort(targetLayers$name)[theLevel-1],
                            stringsAsFactors = FALSE) %>%
            as_tibble() %>%
            filter(geoID %in% gadmIDs) %>%
            rename(NAME_0 = nation, !!unitCols[length(unitCols)-1] := name) %>%
            select(-geom)
        }
      } else {
        oldIDs <- tibble(NAME_0 = tempNation, NAME_1 = tempNation)
        nationGeom$NAME_1 <- tempNation
        unitCols <- c("NAME_0", "NAME_1")
      }

      matchGeom <- nationGeom %>%
        select(!!unitCols) %>%
        mutate_if(is.character, tolower)

      joinedGeom <- nationGeom %>%
        as_tibble() %>%
        select(-geom)  %>%
        mutate_if(is.character, tolower) %>%
        left_join(oldIDs) %>%
        select(!!unitCols, starts_with("al")) %>%
        left_join(matchGeom) %>%
        st_sf()

      xyz <- unitCols[!seq_along(unitCols) %in% c(1, length(unitCols))]

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
    }

    if(update){
      # in case the user wants to update, output the simple feature
      st_write(obj = outGeom,
               dsn = paste0(intPaths, "/cT_geometries/stage2/", tempNation, ".gpkg"),
               layer = paste0("level_", theLevel),
               layer_options = "OVERWRITE=yes",
               quiet = TRUE)
    }
  }

  if(update & moveFile){
    message(paste0("    Moving '", file_name, "' to './stage1/processed'"))
    firstStage <- paste0(intPaths, "/cT_geometries/stage1")
    file.copy(from = paste0(firstStage, "/", file_name), to = paste0(firstStage, "/processed/", file_name))
    file.remove(paste0(firstStage, "/", file_name))
  }

  message()
  return(lut)
}