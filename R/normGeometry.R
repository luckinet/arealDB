#' Normalise geometries
#'
#' Harmonise and integrate geometries into a standardised format
#' @param input [\code{character(1)}]\cr path of the file to normalise. If this
#'   is left empty, all files at stage two as subset by \code{pattern} are
#'   chosen.
#' @param thresh [\code{integerish(1)}]\cr
#' @param outType [\code{character(1)}]\cr the output file-type, see
#'   \code{\link{st_drivers}} for a list. If a file-type supports layers, they
#'   are stored in the same file, otherwise the different layers are provided
#'   separately. For an R-based workflow, \code{"rds"} could be an efficient
#'   option.
#' @param pattern [\code{character(1)}]\cr an optional regular expression. Only
#'   dataset names which match the regular expression will be processed.
#' @param query [\code{character(1)}]\cr part of the SQL query (starting from
#'   WHERE) used to subset the input geometries, for example \code{where NAME_0
#'   = 'France'}. The first part of the query (where the layer is defined) is
#'   derived from the meta-data of the currently handled geometry.
#' @param priority [\code{character(1)}]\cr how to match the new geometries with
#'   the already harmonised database. This can either be
#'
#'   \itemize{
#'     \item \code{"spatial"}: where all territories are intersected spatially or
#'     \item \code{"ontology"}: where territories are matched by comparing their name with the ontology
#'     and those that do not match are intersected spatially,
#'     \item \code{"both"}: where territories are matched with the ontology and spatially, and conflicts are indicated
#'   }
#' @param beep [\code{integerish(1)}]\cr Number specifying what sound to be
#'   played to signal the user that a point of interaction is reached by the
#'   program, see \code{\link[beepr]{beep}}.
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
#'   match, copy gazID from the geometries they overlap. \item For all units that
#'   did not match, rebuild metadata and a new gazID. } \item If update = TRUE,
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
#'   st_drivers NA_crs_ st_is_valid st_make_valid st_as_sf st_geometry
#' @importFrom rmapshaper ms_simplify
#' @importFrom stringr str_split
#' @importFrom tibble as_tibble add_column
#' @importFrom dplyr bind_rows slice lag desc
#' @importFrom tidyr unite
#' @importFrom tidyselect starts_with all_of
#' @importFrom utils tail head
#' @export

normGeometry <- function(input = NULL, pattern = NULL, query = NULL, thresh = 10,
                         outType = "gpkg", priority = "ontology", beep = NULL,
                         update = FALSE, verbose = FALSE){

  # input = NULL; pattern = "ign"; query <- NULL; thresh = 10; outType = "gpkg"; priority = "ontology"; beep = 10; update = TRUE; verbose = FALSE; i = j = 1

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))
  gazPath <- paste0(getOption(x = "gazetteer_path"))

  # get territorial context
  topClass <- paste0(getOption(x = "gazetteer_top"))
  topUnits <- get_concept(table = tibble(class = topClass), ontology = gazPath) %>%
    arrange(label)

  if(is.null(input)){
    input <- list.files(path = paste0(intPaths, "/adb_geometries/stage2"), full.names = TRUE, pattern = pattern)
  } else {
    assertFileExists(x = input, access = "r")
  }

  # set internal objects
  moveFile <- TRUE

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
  assertChoice(x = priority, choices = c("ontology", "spatial", "both"), null.ok = FALSE)

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
    geom_meta <- inv_geometries[grep(pattern = paste0("^", file_name, "$"), x = inv_geometries$source_file),]
    if(file_name %in% geom_meta$source_file){
      newGID <- geom_meta$geoID
      layerName <- geom_meta$layer
      classLabel <- geom_meta$label

      dSeries <- inv_dataseries$name[inv_dataseries$datID == geom_meta$datID]

      # make a new dataseries, in case it doesn't exist yet
      if(!dSeries %in% get_source(ontology = gazPath)$label){
        new_source(name = dSeries, date = Sys.Date(), ontology = gazPath)
      }

      # if there are several columns that contain units, split them and make
      oldClass <- str_split(string = geom_meta$hierarchy, pattern = "\\|")[[1]]
      targetClass <- get_class(label = classLabel, ontology = gazPath)

      new_mapping(new = tail(oldClass, 1), target = targetClass, source = dSeries,
                  match = "exact", certainty = 3, type = "class",
                  ontology = gazPath)

      externalClass <- get_class(external = TRUE, label = !!oldClass, ontology = gazPath)

      unitCols <- get_class(ontology = gazPath) %>%
        separate_rows(has_exact_match, sep = " \\| ") %>%
        separate(col = has_exact_match, into = c("match", "certainty"), sep = "[.]")
      if(length(oldClass) == length(classLabel)){
        unitCols <- unitCols %>%
          filter(label == classLabel & match %in% externalClass$id)
      } else {
        unitCols <- unitCols %>%
          filter(match %in% externalClass$id)
      }
      unitCols <- unitCols %>%
        distinct(label) %>%
        pull(label)

      if(unitCols[1] != topClass){
        theUnits <- str_split(string = geom_meta$source_file, pattern = "_")[[1]][1]
        assertSubset(x = theUnits, choices = topUnits$label)
      } else {
        theUnits <- NULL
      }

    } else{
      stop(paste0("  ! the file '", file_name, "' has not been registered yet."))
    }

    # read the object
    message("\n--> reading new geometries from '", file_name, "' ...")
    if(!is.null(query)){
      moveFile <- FALSE
      inGeom <- read_sf(dsn = thisInput,
                        query = paste0("select * from ", layerName, " ", query),
                        stringsAsFactors = FALSE)
    } else {
      inGeom <- read_sf(dsn = thisInput,
                        layer = layerName,
                        stringsAsFactors = FALSE)
    }

    for(k in seq_along(unitCols)){
      names(inGeom)[[which(names(inGeom) == oldClass[k])]] <- unitCols[k]
    }

    if(!topClass %in% names(inGeom)){
      inGeom <- inGeom %>%
        add_column(tibble(!!topClass := theUnits), .before = unitCols[1])
      unitCols <- c(topClass, unitCols)
      allCols <- get_class(ontology = gazPath) %>%
        pull(label)
      allCols <- allCols[which(allCols %in% head(unitCols, 1)) : which(allCols %in% tail(unitCols, 1))]
    } else {
      allCols <- unitCols
    }

    message("    harmonizing territory names ...")
    harmGeom <- matchOntology(table = inGeom,
                              columns = unitCols,
                              dataseries = dSeries,
                              ontology = gazPath,
                              verbose = verbose,
                              beep = beep,
                              all_cols = TRUE)

    if(is.null(theUnits)){
      theUnits <- unique(eval(expr = parse(text = unitCols[1]), envir = harmGeom)) %>%
        as.character()
    }

    if(length(theUnits) == 0){
      moveFile <- FALSE
      message("    ! New geometries not part of subset !")
    } else {

      # then we loop through all nations
      for(j in seq_along(theUnits)){

        tempUnit <- theUnits[j]

        if(is.na(tempUnit)) next
        message(paste0(" -> processing '", tempUnit, "' ..."))

        if(length(theUnits) != 1){
          # create a geom specifically for the recent top territory
          newGeom <- harmGeom %>%
            filter_at(vars(all_of(unitCols[1])), all_vars(. %in% tempUnit)) %>%
            select(all_of(allCols), id, match, external)
          assertChoice(x = allCols[1], choices = names(newGeom), .var.name = "names(nation_column)")
        } else {
          newGeom <- harmGeom %>%
            select(all_of(allCols), id, match, external)
        }

        # dissolve ----
        # in case the object consists of several POLYGONs per unique name, dissolve
        # them into a single MULTIPOLYGON
        if(unique(st_geometry_type(newGeom)) == "POLYGON"){
          uniqueUnits <- newGeom %>%
            as_tibble() %>%
            select(!!unitCols) %>%
            unique()

          if(all(!is.na(uniqueUnits))){
            if(dim(newGeom)[1] > dim(uniqueUnits)[1]){
              message("    Dissolving multiple polygons into a single multipolygon")

              newGeom <- newGeom %>%
                group_by(across(c(all_of(unitCols), "id"))) %>%
                summarise() %>%
                ungroup()
            }
          } else{
            message("  ! The geometry contains only POLYGON features but no unique names to summarise them.")
          }
        }

        # in case the object is not fully valid (e.g., degenerate edges), make
        # it valid
        if(!all(st_is_valid(newGeom))){
          newGeom <- newGeom <- st_make_valid(x = newGeom)
        }

        # file exists? ----
        # determine whether a geometry with the nation as name already exists and
        # whether that contains the correct layer ...
        fileExists <- testFileExists(x = paste0(intPaths, "/adb_geometries/stage3/", tempUnit, ".gpkg"))
        if(fileExists){
          targetLayers <- st_layers(dsn = paste0(intPaths, "/adb_geometries/stage3/", tempUnit, ".gpkg"))
          if(!grepl(pattern = classLabel, x = paste0(targetLayers$name, collapse = "|"))){
            fileExists <- FALSE
          }
        }

        # ... if yes, read it in, otherwise create it
        if(fileExists){

          topLayer <- targetLayers$name[which.min(targetLayers$features)]

          # read target geoms ----
          message("    Reading target geometries")
          targetGeom <- read_sf(dsn = paste0(intPaths, "/adb_geometries/stage3/", tempUnit, ".gpkg"),
                                layer = classLabel,
                                stringsAsFactors = FALSE)

          if(st_crs(targetGeom)$input == "Undefined Cartesian SRS"){
            st_crs(targetGeom) <- NA_crs_
          }

          # reproject new geom ----
          if(st_crs(newGeom) != st_crs(targetGeom)){
            message("    Reprojecting new geometries")
            newGeom <- st_transform(x = newGeom, crs = st_crs(targetGeom))
          }

          # test whether/which of the new features are already (spatially) in the target
          # geom and stop if all of them are there already.
          message("    Checking for exact spatial matches")
          equals <- unlist(st_equals(newGeom, targetGeom))
          if(length(equals) == dim(newGeom)[1]){
            message("  ! --> all features of the new geometry are already part of the target geometry !")
            next
          }

          newGeom <- newGeom %>%
            mutate(sourceFID = seq_along(geom))

          # determine geoms that are already ok ... ----
          message("    Joining target and source geometries")
          outGeom <- newGeom %>%
            filter(!is.na(id)) %>%
            unite(col = "gazName", all_of(allCols), sep = ".") %>%
            mutate(geoID = newGID,
                   gazClass = tail(allCols, 1)) %>%
            select(gazID = id, gazName, gazClass, match, external, geoID)

          stage2Geom <- newGeom %>%
            filter(is.na(id))

          if(dim(stage2Geom)[1] != 0){

            message("    -> Simplifying geometries")
            targetGeomSimple <- targetGeom %>%
              ms_simplify(keep = 0.1, keep_shapes = TRUE) %>%
              mutate(target_area = as.numeric(st_area(.))) %>%
              bind_cols(target_geom = st_geometry(targetGeom))

            if(priority == "ontology"){
              # in this case, names are matched first and for the remaining territories a spatial match is used
              stage3GeomSimple <- targetGeomSimple %>%
                filter(!gazID %in% outGeom$gazID) %>%
                select(-match, -external)
            } else if(priority %in% "spatial"){
              # in this case, names are not matched at all and instead matches are determined by spatial overlap entirely
              outGeom <- outGeom %>%
                slice(-c(1:dim(outGeom)[1]))
              stage2Geom <- newGeom
              stage3GeomSimple <- targetGeomSimple %>%
                select(-match, -external)
            }# else if(priority == "both"){
            # in this case, both methods are employed. Conflicts between the methods need to be resolved or documented
            #}

            stage2GeomSimple <- stage2Geom %>%
              ms_simplify(keep = 0.1, keep_shapes = TRUE) %>%
              mutate(source_area = as.numeric(st_area(.))) %>%
              bind_cols(stage2_geom = st_geometry(stage2Geom))

            # ... and then carrying out an intersection
            message("    -> Calculating intersection")
            overlapGeom <- suppressWarnings(
              stage3GeomSimple %>%
                st_intersection(y = stage2GeomSimple) %>%
                mutate(intersect_area = as.numeric(st_area(.)),
                       target_prop = round(intersect_area/target_area*100, 5),
                       source_prop = round(intersect_area/source_area*100, 5)) %>%
                group_by(gazID) %>%
                arrange(target_prop) %>%
                mutate(nr = n(),
                       target_prop_sum = cumsum(target_prop),
                       source_prop_sum = cumsum(source_prop),
                       valid = if_else(target_prop_sum > thresh,
                                       if_else(((target_prop_sum + source_prop_sum) / 2) > thresh, 1L, 0L),
                                       if_else(source_prop_sum > thresh,
                                               if_else(((target_prop_sum + source_prop_sum) / 2) > thresh, 2L, 0L),
                                               0L))) %>%
                ungroup() %>%
                arrange(desc(nr), gazID)
            )

            # check whether any geometries from the (simplfyed) newGeom have no overlap
            validFIDs <- overlapGeom %>%
              filter(valid != 0L) %>%
              pull(sourceFID) %>%
              unique()
            stage2Missing <- stage2GeomSimple %>%
              filter(!sourceFID %in% validFIDs)

            if(dim(stage2Missing)[1] != 0){
              message("       -> Correcting for missing ontology matches")
              overlapGeom <- suppressWarnings(
                targetGeomSimple %>%
                  select(-match, -external) %>%
                  st_intersection(y = stage2Missing) %>%
                  mutate(intersect_area = as.numeric(st_area(.)),
                         target_prop = round(intersect_area/target_area*100, 5),
                         source_prop = round(intersect_area/source_area*100, 5)) %>%
                  group_by(gazID) %>%
                  arrange(target_prop) %>%
                  mutate(nr = n(),
                         target_prop_sum = cumsum(target_prop),
                         source_prop_sum = cumsum(source_prop),
                         valid = if_else(target_prop_sum > thresh,
                                         if_else(((target_prop_sum + source_prop_sum) / 2) > thresh, 1L, 0L),
                                         if_else(source_prop_sum > thresh,
                                                 if_else(((target_prop_sum + source_prop_sum) / 2) > thresh, 2L, 0L),
                                                 0L))) %>%
                  ungroup() %>%
                  arrange(desc(nr), gazID) %>%
                  bind_rows(overlapGeom, .)
              )
            }

            # extract geometries that are valid ...
            message("    -> Spatial matches with more than ", 100-thresh, "% overlap")
            outGeom <- overlapGeom %>%
              filter(valid == 1L) %>%
              arrange(gazID) %>%
              mutate(match = paste0("overlap.", round(target_prop), ".", round(source_prop)),
                     geoID = newGID) %>%
              as_tibble() %>%
              st_as_sf(sf_column_name = "stage2_geom") %>%
              select(gazID, gazName, gazClass, match, external, sourceFID, geoID, geom = stage2_geom) %>%
              bind_rows(outGeom, .)

            # ... where the reverse match is valid...
            message("    -> Overlap with more than one polygon")
            reverseSource <- overlapGeom %>%
              filter(valid == 2L) %>%
              pull(sourceFID)
            outGeom <- suppressWarnings(
              stage2GeomSimple %>%
                filter(sourceFID %in% reverseSource) %>%
                st_intersection(y = targetGeomSimple) %>%
                mutate(intersect_area = as.numeric(st_area(.)),,
                       target_prop = round(intersect_area/target_area*100, 5),
                       source_prop = round(intersect_area/source_area*100, 5),
                       match = paste0("reverse.", round(target_prop), ".", round(source_prop)),
                       geoID = newGID) %>%
                group_by(sourceFID) %>%
                arrange(desc(source_prop)) %>%
                mutate(source_cumsum = cumsum(source_prop),
                       valid = if_else(lag(source_cumsum) < (100 - thresh), TRUE, FALSE),
                       valid = if_else(is.na(valid), TRUE, valid)) %>%
                ungroup() %>%
                filter(valid) %>%
                as_tibble() %>%
                st_as_sf(sf_column_name = "stage2_geom") %>%
                select(gazID, gazName, gazClass, match, external, sourceFID, geoID, geom = stage2_geom) %>%
                bind_rows(outGeom, .)
            )

            # ... and handle those, that are not valid
            message("    -> Rematching ontologic mismatches")
            unmatchedSource <- overlapGeom %>%
              filter(valid == 0L) %>%
              filter(!external %in% outGeom$external) %>%
              pull(sourceFID)
            outGeom <- suppressWarnings(
              stage2GeomSimple %>%
                filter(sourceFID %in% unmatchedSource) %>%
                st_intersection(y = targetGeomSimple) %>%
                mutate(intersect_area = as.numeric(st_area(.)),
                       source_prop = round(intersect_area/source_area*100, 5),
                       target_prop = round(intersect_area/target_area*100, 5),
                       match = paste0("rematch.", round(target_prop), ".", round(source_prop)),
                       geoID = newGID) %>%
                group_by(sourceFID) %>%
                arrange(desc(source_prop)) %>%
                mutate(source_cumsum = cumsum(source_prop),
                       valid = if_else(lag(source_cumsum) < (100 - thresh), TRUE, FALSE),
                       valid = if_else(is.na(valid), TRUE, valid)) %>%
                ungroup() %>%
                filter(valid) %>%
                as_tibble() %>%
                st_as_sf(sf_column_name = "stage2_geom") %>%
                select(gazID, gazName, gazClass, match, external, sourceFID, geoID, geom = stage2_geom) %>%
                bind_rows(outGeom, .)
            )
            outGeom <- outGeom %>%
              select(-any_of("sourceFID"))

          } else {
            message("    -> All ontology matches are valid")
          }

          outGeom <- targetGeom %>%
            bind_rows(outGeom) %>%
            arrange(gazID, geoID)

          # message("    -> Updating ontology")
          # develop a routine that inserts the spatially determined
          # relationships between geometries into the ontology, so that next
          # time those routines would have to handle less load

        } else {

          message("    Creating new basis dataset for class ", classLabel, ".")

          outGeom <- suppressMessages(
            newGeom %>%
              unite(col = "gazName", all_of(unitCols), sep = ".") %>%
              mutate(gazClass = classLabel,
                     geoID = newGID) %>%
              select(gazID = id, gazName, gazClass, match, external, geoID)
            )

        }

        if(update){
          # in case the user wants to update, output the simple feature
          if(outType != "rds"){
            st_write(obj = outGeom,
                     dsn = paste0(intPaths, "/adb_geometries/stage3/", tempUnit, ".", outType),
                     layer = classLabel,
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

    outLut <- bind_rows(outLut, geom_meta)

  }

  gc()

  message()
  return(outLut)
}
