#' Normalise geometries
#'
#' Harmonise and integrate geometries in a standardised format
#' @param input [\code{character(1)}]\cr path of the file to normalise. If this
#'   is left empty, all files at stage two as subset by \code{pattern} are
#'   chosen.
#' @param thresh [\code{integerish(1)}]\cr the deviation of percentage of
#'   overlap above which to consider two territorial units "different".
#' @param outType [\code{character(1)}]\cr the output file-type, see
#'   \code{\link{st_drivers}} for a list. If a file-type supports layers, they
#'   are stored in the same file, otherwise the different layers are provided
#'   separately. For an R-based workflow, \code{"rds"} could be an efficient
#'   option.
#' @param pattern [\code{character(1)}]\cr an optional regular expression. Only
#'   dataset names which match the regular expression will be processed.
#' @param ... [\code{character(1)}]\cr argument-value combination to filter the
#'   new geometries by.
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
#'   the file name. \item In case filters are set, the new geometry is filtered
#'   by those. \item The territorial names are matched with the gazetteer to
#'   harmonise new territorial names (at this step, the function might ask the
#'   user to edit the file 'matching.csv' to align new names with already
#'   harmonised names). \item Loop through every nation potentially included in
#'   the file that shall be processed and carry out the following steps:
#'   \itemize{ \item In case the geometries are provided as a list of simple
#'   feature POLYGONS, they are dissolved into a single MULTIPOLYGON per main
#'   polygon. \item In case the nation to which a geometry belongs has not yet
#'   been created at stage three, the following steps are carried out:
#'   \enumerate{ \item Store the current geometry as basis of the respective
#'   level (the user needs to make sure that all following levels of the same
#'   dataseries are perfectly nested into those parent territories, for example
#'   by using the GADM dataset) } \item In case the nation to which the geometry
#'   belongs has already been created, the following steps are carried out:
#'   \enumerate{ \item Check whether the new geometries have the same coordinate
#'   reference system as the already existing database and re-project the new
#'   geometries if this is not the case. \item Check whether all new geometries
#'   are already exactly matched spatially and stop if that is the case. \item
#'   Check whether the new geometries are all within the already defined
#'   parents, and save those that are not as a new geometry. \item Calculate
#'   spatial overlap and distinguish the geometries into those that overlap with
#'   more and those with less than \code{thresh}. \item For all units that did
#'   match, copy ahID from the geometries they overlap. \item For all units that
#'   did not match, rebuild metadata and a new ahID. } \item If update = TRUE,
#'   store the processed geometry at stage three.} \item Move the geometry to
#'   the folder '/processed', if it is fully processed.}
#' @family normalise functions
#' @return This function harmonises and integrates so far unprocessed geometries
#'   at stage two into stage three of the geospatial database. It produces for
#'   each main polygon (e.g. nation) in the registered geometries a spatial file
#'   of the specified file-type.
#' @examples
#' if(dev.interactive()){
#'   library(sf)
#'
#'   # build the example database
#'   makeExampleDB(until = "regGeometry", path = tempdir())
#'
#'   # normalise all geometries ...
#'   normGeometry(nation = "estonia", update = TRUE)
#'
#'   # ... and check the result
#'   st_layers(paste0(tempdir(), "/adb_geometries/stage3/Estonia.gpkg"))
#'   output <- st_read(paste0(tempdir(), "/adb_geometries/stage3/Estonia.gpkg"))
#' }
#' @importFrom checkmate assertFileExists assertIntegerish assertLogical
#'   assertCharacter assertChoice testFileExists
#' @importFrom ontologics load_ontology new_source new_mapping get_class
#'   get_source
#' @importFrom dplyr filter distinct select mutate rowwise filter_at vars
#'   all_vars pull group_by arrange summarise mutate_if rename n if_else ungroup
#'   across
#' @importFrom rlang sym exprs
#' @importFrom readr read_csv read_rds
#' @importFrom tools file_ext
#' @importFrom sf st_layers read_sf st_write st_join st_buffer st_equals st_sf
#'   st_transform st_crs st_crs<- st_geometry_type st_area st_intersection
#'   st_drivers NA_crs_ st_is_valid st_make_valid
#' @importFrom stringr str_split
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows slice
#' @importFrom tidyr unite
#' @importFrom tidyselect starts_with all_of
#' @importFrom utils tail
#' @export

normGeometry <- function(input = NULL, pattern = NULL, ..., thresh = 10,
                         outType = "gpkg", update = FALSE, verbose = FALSE){

  # input = NULL; pattern = "gadm"; sbst <- list(); thresh = 10; outType = "gpkg"; update = TRUE; verbose = FALSE; i = 1

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))
  gazPath <- paste0(getOption(x = "gazetteer_path"))

  if(is.null(input)){
    input <- list.files(path = paste0(intPaths, "/adb_geometries/stage2"), full.names = TRUE, pattern = pattern)
  } else {
    assertFileExists(x = input, access = "r")
  }

  # set internal objects
  moveFile <- TRUE
  sbst <- exprs(..., .named = TRUE)
  # return(sbst)

  # get tables
  inv_geometries <- read_csv(paste0(intPaths, "/inv_geometries.csv"), col_types = "iiccccccDDcc")
  inv_dataseries <- read_csv(paste0(intPaths, "/inv_dataseries.csv"), col_types = "icccccc")

  # check validity of arguments
  assertIntegerish(x = thresh, any.missing = FALSE)
  assertLogical(x = update, len = 1)
  assertNames(x = outType, subset.of = c(tolower(st_drivers()$name), "rds"))
  assertNames(x = colnames(inv_geometries),
              permutation.of = c("geoID", "datID", "label", "source_file", "layer",
                                 "hierarchy", "orig_file", "orig_link", "download_date",
                                 "next_update", "update_frequency", "notes"))
  assertNames(x = colnames(inv_dataseries),
              permutation.of = c("datID", "name", "description", "homepage",
                                 "licence_link", "licence_path", "notes"))

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
      theLabel <- lut$label

      dSeries <- inv_dataseries$name[inv_dataseries$datID == lut$datID]

      # make a new dataseries, in case it doesn't exist yet
      if(!dSeries %in% get_source(ontology = gazPath)$label){
        new_source(name = dSeries, date = Sys.Date(), ontology = gazPath)
      }

      # if there are several columns that contain units, split them and make
      oldNames <- str_split(string = lut$hierarchy, pattern = "\\|")[[1]]

      theTarget <- get_class(label = theLabel, ontology = gazPath)

      new_mapping(new = tail(oldNames, 1), target = theTarget,
                  source = dSeries, match = "exact", certainty = 3, type = "class", ontology = gazPath)

      classID <- get_class(external = TRUE, regex = TRUE,
                           label = !!oldNames,
                           ontology = gazPath)

      unitCols <- get_class(ontology = gazPath) %>%
        separate_rows(has_exact_match, sep = " \\| ") %>%
        separate(col = has_exact_match, into = c("match", "certainty"), sep = "[.]") %>%
        filter(match %in% classID$id) %>%
        distinct(label) %>%
        pull(label)

      topCol <- unitCols[1]
    } else{
      stop(paste0("  ! the file '", file_name, "' has not been registered yet."))
    }

    # read the object
    message("\n--> reading new geometries from '", file_name, "' ...")
    newLayers <- st_layers(dsn = thisInput)
    # inGeom <- read_rds(file = paste0(getwd(), "/testing.rds"))
    inGeom <- read_sf(dsn = thisInput,
                       layer = theLayer,
                       stringsAsFactors = FALSE)
    for(k in seq_along(unitCols)){
      names(inGeom)[[which(names(inGeom) == oldNames[k])]] <- unitCols[k]
    }

    message("    harmonizing nation names ...")
    inGeom <- matchOntology(table = inGeom,
                            columns = unitCols,
                            dataseries = dSeries,
                            ontology = gazPath,
                            verbose = verbose)
    # table = inGeom; columns = unitCols; dataseries = dSeries; ontology = gazPath

    # potentially filter
    if(length(sbst) != 0){
      moveFile <- FALSE

      for(k in seq_along(sbst)){
        if(!names(sbst)[k] %in% names(inGeom)){
          warning(paste0("'", names(sbst)[k], "' is not a column in the new geometry -> ignoring the filter."))
          next
        }

        inGeom <- inGeom %>%
          filter(.data[[names(sbst)[k]]] %in% as.character(sbst[[k]]))
      }
    }

    # determine top-most value
    topUnits <- unique(eval(expr = parse(text = topCol), envir = inGeom)) %>%
      as.character()

    if(length(topUnits) == 0){
      moveFile <- FALSE
      message("    ! New geometries not part of subset !")
    } else {

      # then we loop through all nations
      for(j in seq_along(topUnits)){

        tempUnit <- topUnits[j]

        if(is.na(tempUnit)) next
        message(paste0(" -> processing '", tempUnit, "' ..."))

        # create a geom specifically for the recent top territory
        sourceGeom <- inGeom %>%
          filter_at(vars(all_of(topCol)), all_vars(. %in% tempUnit)) %>%
          select(all_of(unitCols), id)
        assertChoice(x = topCol, choices = names(sourceGeom), .var.name = "names(nation_column)")


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
            }
          } else{
            message("  ! The geometry contains only POLYGON features but no unique names to summarise them.")
          }
        }

        # in case the object is not fully valid (e.g., degenerate edges), make
        # it valid
        if(!all(st_is_valid(sourceGeom))){
          sourceGeom <- st_make_valid(x = sourceGeom)
        }

        # file exists? ----
        # determine whether a geometry with the nation as name already exists and
        # whether that contains the correct layer ...
        fileExists <- testFileExists(x = paste0(intPaths, "/adb_geometries/stage3/", tempUnit, ".gpkg"))
        if(fileExists){
          targetLayers <- st_layers(dsn = paste0(intPaths, "/adb_geometries/stage3/", tempUnit, ".gpkg"))
          if(!grepl(pattern = theLabel, x = paste0(targetLayers$name, collapse = "|"))){
            fileExists <- FALSE
          }
        }

        # ... if yes, read it in, otherwise create it
        if(fileExists){

          topLayer <- targetLayers$name[which.min(targetLayers$features)]

          # read target geoms ----
          message("    Reading target geometries")
          targetGeom <- read_sf(dsn = paste0(intPaths, "/adb_geometries/stage3/", tempUnit, ".gpkg"),
                                layer = theLabel,
                                stringsAsFactors = FALSE)

          if(st_crs(targetGeom)$input == "Undefined Cartesian SRS"){
            st_crs(targetGeom) <- NA_crs_
          }

          if(theLabel != topLayer){
            parentLabel <- theTarget$has_broader
            parentGeom <- read_sf(dsn = paste0(intPaths, "/adb_geometries/stage3/", tempUnit, ".gpkg"),
                                  layer = parentLabel,
                                  stringsAsFactors = FALSE)

            if(st_crs(parentGeom)$input == "Undefined Cartesian SRS"){
              st_crs(parentGeom) <- NA_crs_
            }

          } else {
            parentGeom <- NULL
          }

          # reproject new geom ----
          if(st_crs(sourceGeom) != st_crs(targetGeom)){
            message("    Reprojecting new geometries")
            sourceGeom <- st_transform(x = sourceGeom, crs = st_crs(targetGeom))
          }
          # test whether/which of the new features are already (spatially) in the target
          # geom and stop if all of them are there already.
          message("    Checking for exact spatial matches")
          equals <- unlist(st_equals(sourceGeom, targetGeom))
          if(length(equals) == dim(sourceGeom)[1]){
            message("  ! --> all features of the new geometry are already part of the target geometry !")
            next
          }

          # join geoms ----
          # first make unique FIDs for each feature
          sourceGeom <- sourceGeom %>%
            mutate(sourceFID = seq_along(geom))

          targetGeom <- targetGeom %>%
            mutate(target_area = as.numeric(st_area(.)),
                   targetFID = seq_along(geom))

          message("    Joining target and source geometries")

          # # spatial join with the parent geom (smallest geoID), to determine
          # # whether all are within a parent
          # if(theLabel != topLayer){
          #   minGeoID <- min(parentGeom$geoID)
          #   basisParent <- parentGeom[parentGeom$geoID %in% minGeoID,]
          #   in_parent <- suppressMessages(suppressWarnings(
          #     sourceGeom %>%
          #       st_join(basisParent, largest = TRUE)
          #   ))
          #
          #   if(dim(sourceGeom)[1] != dim(in_parent)[1]){
          #     stop("spatial join between 'sourceGeom' and 'parentGeom' has an error.")
          #   }
          #
          #   # if there were units in sourceGeom that can't be joined with the
          #   # basis, they are accidentally part and need to be treated as an
          #   # extra object.
          #   if(any(is.na(in_parent$ahID))){
          #     newName <- str_split(file_name, "[.]")[[1]]
          #     newName <- paste0(newName[1], "_not-", tempUnit, ".", newName[2])
          #     isNA <- is.na(in_parent$ahID)
          #
          #     message(paste0("  ! not all new units contain a valid ahID after joining with 'parentGeom', please see 'stage2/", newName, "' !"))
          #     in_parent %>%
          #       filter(isNA) %>%
          #       select(unitCols) %>%
          #       st_write(dsn = paste0(intPaths, "/adb_geometries/stage2/", newName),
          #                layer = theLabel,
          #                append = FALSE,
          #                quiet = TRUE)
          #     in_parent <- in_parent %>%
          #       filter(!isNA)
          #     sourceGeom <- sourceGeom %>%
          #       filter(!isNA)
          #   }
          # }

          # then get the overlap with the targetGeom
          overlap_with_target <- suppressMessages(suppressWarnings(
            sourceGeom %>%
              st_buffer(dist = 0) %>% # is needed sometimes to clarify "self-intersection" problems: https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec
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
            if(theLabel == targetLayers$name[1]){
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
            left_join(overlap_with_target %>% select(-id), by = "sourceFID")

          # get all the valid geoms
          validUnits <- sourceOverlap %>%
            filter(valid) %>%
            mutate(geoID = newGID) %>%
            select(-sourceFID, -valid, -!!unitCols, -id) %>%
            st_sf()

          # get geoms that are invalid because their overlap is smaller than
          # threshold
          invalidUnits <- sourceOverlap %>%
            filter(!valid) %>%
            select(-sourceFID, -valid)

          # ensure that the number of valid and invalidUnits equals to the source units
          if(dim(sourceGeom)[1] != (dim(validUnits)[1] + dim(invalidUnits)[1])){
            stop("! some units were lost while joining. !")
          }

          # create an overlap of the invalid geometries with the parent
          # geometries to identify whether they are actually inside the parents
          message("    Determining parent IDs spatially")
          matchGeoms <- invalidUnits %>%
            select(all_of(unitCols), geom) %>%
            st_sf()

          prevIDs <- suppressMessages(suppressWarnings(
            matchGeoms %>%
              mutate(overlap_area = st_area(.)) %>%
              st_intersection(y = parentGeom) %>%
              mutate(overlap = as.numeric(st_area(.)/overlap_area*100)) %>%
              as_tibble() %>%
              group_by(.dots = all_of(unitCols), overlap_area) %>%
              filter(overlap == max(overlap)) %>%
              select(-geoID) %>%
              unique() %>%
              ungroup() %>%
              select(-overlap_area, -geom) %>%
              unique()
          ))

          if(any(round(prevIDs$overlap, 1) != 100)){
            warning("non-matching geometries don't fully overlap with their parent.")
          }

          # finalise invalid geometries for row-binding them with the valid units
          newUnits <- invalidUnits %>%
            select(-ahName, -ahID) %>%
            mutate(geoID = newGID) %>%
            unite(col = "ahName", all_of(unitCols), sep = ".") %>%
            st_sf() %>%
            select(ahName, ahID = id, geoID)

          # combine old and new units
          outGeom <- validUnits %>%
            select(ahName, ahID, geoID, everything())

          if(!dim(outGeom)[1] == 0 | !dim(newUnits)[1] == 0){
            outGeom <- outGeom %>%
              bind_rows(newUnits)
          }

          outGeom <- targetGeom %>%
            bind_rows(outGeom) %>%
            arrange(ahID)

        } else {

          message("    Creating new basis dataset for class ", theLabel, ".")

          outGeom <- suppressMessages(
            sourceGeom %>%
              unite(col = "ahName", all_of(unitCols), sep = ".") %>%
              mutate(onto_class = theLabel,
                     geoID = newGID) %>%
              ungroup() %>%
              select(ahName, ahID = id, onto_class, geoID)
            )

        }

        if(update){
          # in case the user wants to update, output the simple feature
          if(outType != "rds"){
            st_write(obj = outGeom,
                     dsn = paste0(intPaths, "/adb_geometries/stage3/", tempUnit, ".", outType),
                     layer = theLabel,
                     append = FALSE,
                     quiet = TRUE)
          } else {
            saveRDS(object = outGeom, file = paste0(intPaths, "/adb_geometries/stage3/", tempUnit, ".rds"))
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
