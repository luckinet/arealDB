#' Normalise geometries
#'
#' Harmonise and integrate geometries in a standardised format
#' @param input [\code{character(1)}]\cr path of the file to normalise. If this
#'   is left empty, all files at stage two as subset by \code{pattern} are
#'   chosen.
#' @param ... [\code{character(.)}]\cr a subset of administrative units as given
#'   by \code{nation}, \code{continent}, \code{region}, \code{subregion} or
#'   \code{un_member = TRUE/FALSE}. Valid selection criteria and their values
#'   are in the object \code{\link{countries}}.
#' @param thresh [\code{integerish(1)}]\cr the deviation of percentage of
#'   overlap above which to consider two territorial units "different".
#' @param outType [\code{character(1)}]\cr the output file-type, see
#'   \code{\link{st_drivers}} for a list. If a file-type supports layers, they
#'   are stored in the same file, otherwise the different layers are provided
#'   separately. For an R-based workflow, \code{"rds"} could be an efficient
#'   option.
#' @param pattern [\code{character(1)}]\cr an optional regular expression. Only
#'   dataset names which match the regular expression will be returned.
#' @param update [\code{logical(1)}]\cr whether or not the physical files should
#'   be updated (\code{TRUE}) or the function should merely return the geometry
#'   inventory of the handled files (\code{FALSE}, default). This is helpful to
#'   check whether the metadata specification and the provided file(s) are
#'   properly specified.
#' @param verbose [\code{logical(1)}]\cr be verbose about what is happening
#'   (default \code{FALSE}). Furthermore, you can use
#'   \code{\link{suppressMessages}} to make this function completely silent.
#' @details To normalise geometries, this function proceeds as follows:
#'   \enumerate{ \item Read in \code{input} and extract initial metadata from
#'   the file name. \item Loop through every nation potentially included in the
#'   file that shall be processed and carry out the following steps: \itemize{
#'   \item In case the geometries are provided as a list of simple feature
#'   POLYGONS, they are dissolved into a single MULTIPOLYGON per nation. \item
#'   In case the nation to which a geometry belongs has not yet been created at
#'   stage three, the following steps are carried out: \enumerate{ \item Check
#'   whether the recent dataset is GADM, to build the initial administrative
#'   hierarchy from GADM geometries, and stop if this is not the case. \item
#'   Extract the full hierarchy of all territorial units that are part of the
#'   geometry. \item Reconstruct ahID from the intermediate spatial objects and
#'   from the metadata. } \item In case the nation to which the geometry belongs
#'   has already been created, the following steps are carried out: \enumerate{
#'   \item Check whether the new geometries have the same coordinate reference
#'   system as the already existing database and re-project the new geometries
#'   if this is not the case. \item Check whether all new geometries are already
#'   exactly matched spatially and stop if that is the case. \item Check whether
#'   the new geometries are all within the already defined parents, and save
#'   those that are not as a new geometry. \item Calculate spatial overlap and
#'   distinguish the geometries into those that overlap with more and those with
#'   less than \code{thresh}. \item For all units that did match, copy ahID from
#'   the geometries they overlap. \item For all units that did not match,
#'   rebuild metadata and a new ahID. } \item If update = TRUE, store the
#'   processed geometry at stage three.} \item Move the geometry to the folder
#'   '/processed', if it is fully processed.}
#' @family normalisers
#' @return This function harmonises and integrates so far unprocessed geometries
#'   at stage two into stage three of the geospatial database. It produces for
#'   each nation in the registered geometries a spatial file of the specified
#'   file-type.
#' @examples
#' library(sf)
#'
#' # build the example database
#' makeExampleDB(until = "regGeometry")
#'
#' # normalise all geometries ...
#' normGeometry(nation = "estonia", update = TRUE)
#'
#' # ... and check the result
#' st_layers(paste0(tempdir(), "/newDB/adb_geometries/stage3/Estonia.gpkg"))
#' output <- st_read(paste0(tempdir(), "/newDB/adb_geometries/stage3/Estonia.gpkg"))
#' @importFrom checkmate assertFileExists assertIntegerish assertLogical
#'   assertCharacter assertChoice testFileExists
#' @importFrom dplyr filter distinct select mutate rowwise filter_at vars
#'   all_vars pull group_by arrange summarise mutate_if rename n if_else ungroup
#'   across
#' @importFrom rlang sym exprs
#' @importFrom readr read_csv
#' @importFrom sf st_layers read_sf st_write st_join st_buffer st_equals st_sf
#'   st_transform st_crs st_geometry_type st_area st_intersection st_drivers
#' @importFrom stringr str_split
#' @importFrom tibble as_tibble
#' @importFrom tidyr unite
#' @importFrom tidyselect starts_with all_of
#' @export

normGeometry <- function(input = NULL, ..., thresh = 10, outType = "gpkg",
                         pattern = NULL, update = FALSE, verbose = FALSE){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))

  if(is.null(input)){
    input <- list.files(path = paste0(intPaths, "/adb_geometries/stage2"), full.names = TRUE, pattern = pattern)
  } else {
    assertFileExists(x = input, access = "r")
  }

  # set internal objects
  subsets <- exprs(..., .named = TRUE)
  assertList(x = subsets)
  moveFile <- TRUE

  # get tables
  inv_geometries <- read_csv(paste0(intPaths, "/inv_geometries.csv"), col_types = "iiiccccccDDcc")
  assertNames(x = colnames(inv_geometries), permutation.of = c("geoID", "datID", "level", "source_file",
                                                               "layer", "nation_column", "unit_column", "orig_file", "orig_link",
                                                               "download_date", "next_update", "update_frequency", "notes"))

  # check validity of arguments
  assertIntegerish(x = thresh, any.missing = FALSE)
  assertLogical(x = update, len = 1)
  assertNames(x = outType, subset.of = c(tolower(st_drivers()$name), "rds"))

  outLut <- NULL
  for(i in seq_along(input)){

    thisInput <- input[i]

    # scrutinize file-name (the fields, which are delimited by "_", carry important information)
    pathStr <- str_split(thisInput, "/")[[1]]
    file_name <- pathStr[length(pathStr)]
    fields <- str_split(file_name, "_")[[1]]

    if(!file_name %in% inv_geometries$source_file){
      next
    }

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
    message("\n--> reading new geometries from '", file_name, "' ...")
    newLayers <- st_layers(dsn = thisInput)
    newGeom <- read_sf(dsn = thisInput,
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
                                verbose = verbose)%>%
        mutate(valid = if_else(target == "ignore", FALSE, if_else(target %in% countries$unit, TRUE, FALSE))) %>%
        select(!!nationCol := origin, target, valid)
      theNations <- nations[[nationCol]][nations$valid]

      newGeom <- newGeom %>%
        left_join(nations) %>%
        filter(valid) %>%
        mutate(!!nationCol := target) %>%
        select(-valid, -target)
      nations <- nations$target[nations$valid]

    } else{
      severalNations <- FALSE
      nation <- toupper(fields[1])
      assertChoice(x = nation, choices = countries$iso_a3)
      nations <- countries %>%
        as_tibble() %>%
        filter(iso_a3 == !!nation) %>%
        pull(nation)
      theNations <- nations
    }

    # potentially subset nation values
    if(length(subsets) > 0){
      assertChoice(x = names(subsets), choices = c("nation", "un_member", "continent", "region", "subregion"))

      # unify also the nations with which to subset
      if(any(names(subsets) == "nation")){
        toUnify <- eval(subsets[[which(names(subsets) == "nation")]])
        nationNames <- countries$nation[!is.na(countries$nation)]
        unified <- translateTerms(terms = toUnify,
                                  index = "tt_territories",
                                  fuzzy_terms = nationNames,
                                  source = list("geoID" = newGID),
                                  verbose = verbose) %>%
          pull(target)
        subsets[[which(names(subsets) == "nation")]] <- unified
      }
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

    if(length(nations) == 0){
      moveFile <- FALSE
      message("    ! New geometries not part of subset !")
    } else {

      # then we loop through all nations
      for(j in seq_along(nations)){

        tempNation <- nations[j]
        # outNation <- theNations[j]
        nationID <- as.integer(countries$ahID[countries$unit == tempNation])
        message(paste0(" -> processing '", tempNation, "' ..."))

        # create a geom specifically for the recent nation
        if(severalNations){
          sourceGeom <- newGeom %>%
            filter_at(vars(all_of(nationCol)), all_vars(. %in% tempNation)) %>%
            select(all_of(unitCols))
          assertChoice(x = natCol, choices = names(sourceGeom), .var.name = "names(nation_column)")
        } else{
          sourceGeom <- newGeom %>%
            select(all_of(unitCols))
        }

        # dissolve ----
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
                group_by(across(all_of(unitCols))) %>%
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

        # file exists? ----
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

          # read target geoms ----
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

          # reproject new geom ----
          # start_time <- Sys.time()
          if(st_crs(sourceGeom) != st_crs(targetGeom)){
            message("    Reprojecting new geometries")
            sourceGeom <- st_transform(x = sourceGeom, crs = st_crs(targetGeom))
          }
          # test whether/which of the new features are already (spatially) in the target
          # geom and stop if all of them are there already.
          message("    Checking for exact spatial matches")
          # start_time <- Sys.time()
          equals <- unlist(st_equals(sourceGeom, targetGeom))
          if(length(equals) == dim(sourceGeom)[1]){
            message("  ! --> all features of the new geometry are already part of the target geometry !")
            next
          }

          # join geoms ----
          # first make unique FIDs for each feature
          sourceGeom <- sourceGeom %>%
            mutate(#source_area = as.numeric(st_area(.)),
                   sourceFID = seq_along(geom))

          targetGeom <- targetGeom %>%
            mutate(target_area = as.numeric(st_area(.)),
                   targetFID = seq_along(geom))

          message("    Joining target and source geometries")

          # spatial join with the parent geom (smallest geoID), to determine
          # whether all are within a parent
          if(theLevel != 1){
            minGeoID <- min(parentGeom$geoID)
            basisParent <- parentGeom[parentGeom$geoID %in% minGeoID,]
            in_parent <- suppressMessages(suppressWarnings(
              sourceGeom %>%
                st_join(basisParent, largest = TRUE)
            ))

            if(dim(sourceGeom)[1] != dim(in_parent)[1]){
              stop("spatial join between 'sourceGeom' and 'parentGeom' has an error.")
            }

            # if there were units in sourceGeom that can't be joined with the
            # basis, they are accidently part and need to be treated as an extra
            # object.
            if(any(is.na(as.numeric(in_parent$ahID)))){
              newName <- str_split(file_name, "[.]")[[1]]
              newName <- paste0(newName[1], "_not-", tempNation, ".", newName[2])
              isNA <- is.na(in_parent$ahID)

              message(paste0("  ! not all new units contain a valid ahID after joining with 'parentGeom', please see 'stage2/", newName, "' !"))
              in_parent %>%
                filter(isNA) %>%
                select(unitCols) %>%
                st_write(dsn = paste0(intPaths, "/adb_geometries/stage2/", newName),
                         layer = paste0("level_", theLevel),
                         delete_layer = TRUE,
                         quiet = TRUE)
              in_parent <- in_parent %>%
                filter(!isNA)
              sourceGeom <- sourceGeom %>%
                filter(!isNA)
            }
          }

          # then get the overlap with the targetGeom
          overlap_with_target <- suppressMessages(suppressWarnings(
            sourceGeom %>%
              st_buffer(dist = 0)%>% # is needed sometimes to clarify "self-intersection" problems: https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec
              st_intersection(y = targetGeom) %>%
              mutate(area = as.numeric(st_area(.))) %>%
              arrange(sourceFID) %>%
              group_by(sourceFID) %>%
              mutate(overlap_area = sum(area),
                     deviation = overlap_area/target_area*100 - 100) %>%
              filter(area == max(area)) %>%
              filter(row_number() == 1) %>%
              mutate(valid = abs(deviation) < thresh) %>%
              ungroup() %>%
              as_tibble() %>%
              select(-geom, -!!unitCols, -targetFID, -target_area, -area, -overlap_area, -deviation) %>%
              arrange(sourceFID)
          ))

          # ensure that 'valid', from which the subset of valid objects will be
          # taken, has the same number of rows as sourceGeom
          if(dim(sourceGeom)[1] != dim(overlap_with_target)[1]){
            # if we are at level 1, the nation may not overlap, but would still
            # be that nation. So we assume that it is valid nevertheless.
            if(theLevel == 1){
              overlap_with_target <- targetGeom %>%
                as_tibble() %>%
                select(-target_area, -targetFID, -geom) %>%
                mutate(valid = TRUE,
                       sourceFID = 1)
            } else {
              stop("validity estimation is not compatible with sourceGeom.")
            }
          }

          targetGeom <- targetGeom %>%
            select(-target_area, -targetFID)

          # get valid geoms that have an overlap larger than the threshold
          sourceOverlap <- sourceGeom %>%
            as_tibble() %>%
            left_join(overlap_with_target, by = "sourceFID")

          validUnits <- sourceOverlap %>%
            filter(valid) %>%
            mutate(geoID = newGID,
                   name = !!sym(unitCols[length(unitCols)])) %>%
            select(-sourceFID, -valid, -!!unitCols) %>%
            st_sf()

          # get geoms that are invalid because their overlap is smaller than
          # threshold
          invalidUnits <- sourceOverlap %>%
            filter(!valid) %>%
            select(-sourceFID) %>%
            select(-valid)

          # ensure that the number of valid and invalidUnits equals to the source units
          if(dim(sourceGeom)[1] != (dim(validUnits)[1] + dim(invalidUnits)[1])){
            stop("! some units were lost while joining. !")
          }

          # 1. compare for invalid units the source name and the target name
          # (perhaps with user interaction), if name similarity can be
          # confirmed, take attributes from 'sourceOverlap'.

          # 2. for those where a match can't be confirmed via the name, plot
          # them on top of each other to make a visual confirmation and likewise
          # take the attributes from 'sourceOverlap'.

          # 3. all remaining are certainly not matching and thus need a new
          # ahID. We do however know about them, that the feature they overlap
          # most with, is the one listed in 'targetFID'. So what does that say
          # about whether I can take the target ahID?

          invalidUnits <- invalidUnits %>%
            mutate(nation = tempNation,
                   name = as.character(name),
                   level = as.integer(theLevel),
                   ahID = as.integer(ahID),
                   geoID = as.integer(geoID)) %>%
            mutate_at(vars(starts_with("al")), as.integer)

          # unique features ----
          # this is a test that should ideally never be true, it would mean that there
          # was an issue with matching the nation name at some point in the pipeline.
          if(length(unique(validUnits$al1_id)) > 1){
            stop("The first administrative level ID is not unique.")
          } else{
            al1ID <- unique(validUnits$al1_id)
          }

          # make ID for already existing features ----
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
            # } else if(theLevel == 2){
            #   prevIDs <- invalidUnits %>%  #for sunday: check this at level  for estonia
            #     as_tibble() %>%
            #     select(-nation, -name, -ahID, -geoID, -level, -starts_with("al"), -geom) %>%
            #     mutate_if(is.character, tolower) %>%
            #     mutate(al1_id = nationID)
            } else {
              alCols <- in_parent %>%
                as_tibble() %>%
                select(starts_with("al")) %>%
                colnames()
              # prevIDs <- parentGeom %>%
              #   as_tibble() %>%
              #   rename(!!natCol := nation, !!unitCols[length(unitCols)-1] := name) %>%
              #   select(-geoID, -geom, -level, -ahID) %>%
              #   filter_at(vars(!!unitCols[-length(unitCols)]), any_vars(!duplicated(.))) %>%
              #   unique()
              prevIDs <- in_parent %>%
                as_tibble() %>%
                select(all_of(unitCols), all_of(alCols))
            }
          } else {
            # if that is not the case, we need to fall back to finding parents IDs
            # via spatial overlap.
            if(theLevel == 1){
              prevIDs <- tibble(!!natCol := tempNation, al1_id = nationID)
            } else {

              matchGeoms <- invalidUnits %>%
                select(unitCols, geom) %>%
                # mutate_if(is.character, tolower) %>%
                st_sf()

              if(dim(matchGeoms)[1] != 0){
                message("    Determining parent IDs spatially")
                prevIDs <- suppressMessages(suppressWarnings(
                  matchGeoms %>%
                    mutate(overlap_area = st_area(.)) %>%
                    st_intersection(y = parentGeom) %>%
                    mutate(overlap = as.numeric(st_area(.)/overlap_area*100)) %>%
                    as_tibble() %>%
                    group_by(.dots = unitCols, overlap_area) %>%
                    filter(overlap == max(overlap)) %>%
                    select(-geoID) %>%
                    unique() %>%
                    ungroup() %>%
                    select(-overlap_area, -nation, -name, -level, -ahID, -overlap, -geom) %>%
                    unique()
                ))
              } else {
                if(theLevel == 2){
                  prevIDs <- tibble(!!natCol := tempNation, al1_id = nationID)
                }
              }


            }
          }
          # prevIDs <- prevIDs %>%
            # mutate_if(is.character, tolower)


          # if prevUnits is NA, fill it with 0
          if(dim(prevUnits)[1] == 1 & all(is.na(prevUnits$prevUnits))){
            prevUnits$al1_id <- prevIDs$al1_id
            prevUnits$prevUnits <- 0
          }

          # make ID for new features ----
          if(theLevel == 1){
            groupLevel <- 2
          } else {
            groupLevel <- theLevel
          }
          suppressMessages(
            newUnits <- invalidUnits %>%
              as_tibble() %>%
              select(-ahID, -geoID, -level, -name, -starts_with("al")) %>%
              # mutate_if(is.character, tolower) %>%
              left_join(prevIDs) %>%
              mutate(al1_id = {if (any(is.na(al1_id))) nationID else al1_id}) %>%
              left_join(prevUnits) %>%
              filter(!duplicated(.)) %>%
              group_by(across(all_of(paste0("al", groupLevel-1, "_id")))) %>%
              mutate(nation = tempNation,
                     name = {if (n() > 0) !!sym(unitCols[length(unitCols)]) else ""},
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

          if(any(is.na(as.numeric(newUnits$ahID)))){
            stop("! not all new units were successfully assigned a valid ahID. !")
          }

          # combine old and new units and rebuild columns
          outGeom <- validUnits %>%
            select(nation, name, level, ahID, geoID, everything())
          if(!dim(outGeom)[1] == 0 | !dim(newUnits)[1] == 0){
            outGeom <- outGeom %>%
              rbind(newUnits)
          }

          outGeom <- targetGeom %>%
            rbind(outGeom) %>%
            arrange(ahID)

        } else {

          # check whether a gadm geometry has been registered previously
          if(!grepl(pattern = "gadm", x = lut$source_file)){
            message(paste0("  ! I didn't find any geometries, please provide first the GADM level ", theLevel, " data product !"))
            next
          }
          message("    Creating new basis dataset at level ", theLevel, ".")

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
                select(-geom, -name)
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
          # matchGeom <- sourceGeom %>%
          #   select(!!unitCols) # %>%
          # mutate(NAME_0 = tolower(NAME_0)) %>%
          # mutate_if(is.character, tolower)

          # join the object with the parent ID to derive IDs of this level that are
          # actual children of the correct parent
          # suppressMessages(joinedGeom <- sourceGeom %>%
          #                    # as_tibble() %>%
          #                    # select(-geom)  %>%
          #                    # mutate_if(is.character, tolower) %>%
          #                    # mutate(NAME_0 = tolower(NAME_0)) %>%
          #                    left_join(parentIDs)# %>%
                             # select(-level, -geoID)# %>%
                             # left_join(matchGeom) %>%
                             # st_sf()
                             # )

          xyz <- unitCols[!seq_along(unitCols) %in% c(1, length(unitCols))]

          # derive required information
          outGeom <- suppressMessages(
            sourceGeom %>%
              left_join(parentIDs) %>%
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
              select(nation = NAME_0, name = !!unitCols[length(unitCols)], level, ahID, geoID, everything(), -all_of(xyz)) %>%
              mutate(name = name))

          if(theLevel == 1){
            unitCols <- orig_units
          }
        }

        if(update){
          # in case the user wants to update, output the simple feature
          if(outType != "rds"){
            st_write(obj = outGeom,
                     dsn = paste0(intPaths, "/adb_geometries/stage3/", tempNation, ".", outType),
                     layer = paste0("level_", theLevel),
                     delete_layer = TRUE,
                     append = TRUE,
                     quiet = TRUE)
          } else {
            saveRDS(object = outGeom, file = paste0(intPaths, "/adb_geometries/stage3/", tempNation, ".rds"))
          }

        }

      }
    }

    if(update & moveFile){
      message(paste0("    Moving '", file_name, "' to './stage2/processed'"))
      firstStage <- paste0(intPaths, "/adb_geometries/stage2")
      file.copy(from = paste0(firstStage, "/", file_name), to = paste0(firstStage, "/processed/", file_name))
      file.remove(paste0(firstStage, "/", file_name))
    }

    outLut <- bind_rows(outLut, lut)

  }

  gc()

  message()
  return(outLut)
}
