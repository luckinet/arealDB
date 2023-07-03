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
#'     \item \code{"ontology"}: where territories are matched by comparing their
#'     name with the ontology
#'     and those that do not match are intersected spatially,
#'     \item \code{"both"}: where territories are matched with the ontology and
#'     spatially, and conflicts are indicated
#'   }
#' @param beep [\code{integerish(1)}]\cr Number specifying what sound to be
#'   played to signal the user that a point of interaction is reached by the
#'   program, see \code{\link[beepr]{beep}}.
#' @param simplify [\code{logical(1)}]\cr whether or not to simplify geometries.
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
#'   get_source new_concept
#' @importFrom dplyr filter distinct select mutate rowwise filter_at vars
#'   all_vars pull group_by arrange summarise mutate_if rename n if_else ungroup
#'   across
#' @importFrom rlang sym exprs
#' @importFrom readr read_csv read_rds
#' @importFrom purrr map
#' @importFrom tools file_ext
#' @importFrom sf st_layers read_sf st_write st_join st_buffer st_equals st_sf
#'   st_transform st_crs st_crs<- st_geometry_type st_area st_intersection
#'   st_drivers NA_crs_ st_is_valid st_make_valid st_as_sf st_geometry st_intersects
#' @importFrom rmapshaper ms_simplify
#' @importFrom stringr str_split_1 str_to_title str_pad str_replace_all
#' @importFrom tibble as_tibble add_column
#' @importFrom dplyr bind_rows slice lag desc n_distinct left_join right_join first
#' @importFrom tidyr unite
#' @importFrom tidyselect starts_with all_of
#' @importFrom progress progress_bar
#' @importFrom utils tail head
#' @importFrom stats na.omit
#' @export

normGeometry <- function(input = NULL, pattern = NULL, query = NULL, thresh = 10,
                         outType = "gpkg", priority = "ontology", beep = NULL,
                         simplify = FALSE, update = FALSE, verbose = FALSE){

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))
  gazPath <- paste0(getOption(x = "gazetteer_path"))

  type <- str_split(tail(str_split(string = gazPath, pattern = "/")[[1]], 1), "[.]")[[1]][1]

  # get territorial context
  topClass <- paste0(getOption(x = "gazetteer_top"))
  topUnits <- get_concept(class = topClass, ontology = gazPath) %>%
    arrange(label)
  allClasses <- get_class(ontology = gazPath) %>%
    pull(label)

  if(is.null(input)){
    input <- list.files(path = paste0(intPaths, "/adb_geometries/stage2"), full.names = TRUE, pattern = pattern)
  } else {
    assertFileExists(x = input, access = "r")
  }

  # set internal objects
  moveFile <- TRUE

  # get tables
  inv_geometries <- read_csv(paste0(intPaths, "/inv_geometries.csv"), col_types = "iicccccDDcc")
  inv_dataseries <- read_csv(paste0(intPaths, "/inv_dataseries.csv"), col_types = "icccccc")

  # check validity of arguments
  assertIntegerish(x = thresh, any.missing = FALSE)
  assertLogical(x = update, len = 1)
  assertLogical(x = simplify, len = 1)
  assertNames(x = outType, subset.of = c(tolower(st_drivers()$name), "rds"))
  assertNames(x = colnames(inv_geometries),
              permutation.of = c("geoID", "datID", "source_file", "layer",
                                 "label", "orig_file", "orig_link", "download_date",
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
      gIDs <- inv_geometries$geoID[inv_geometries$datID == geom_meta$datID]

      # manage dataseries
      dSeries <- inv_dataseries$name[inv_dataseries$datID == geom_meta$datID]
      if(!dSeries %in% get_source(ontology = gazPath)$label){
        new_source(name = dSeries, date = Sys.Date(), ontology = gazPath)
      }

      # extract class and label ...
      targetClass <- map(.x = classLabel,
                         .f = function(x){
                           temp <- str_split(x, "\\|")[[1]]
                           map(str_split(temp, "="), head, 1)
                         }) %>%
        unlist() %>%
        get_class(label = ., ontology = gazPath)
      parentClass <- allClasses[which(allClasses %in% tail(targetClass$label, 1)) - 1]

      targetLabel <- map(.x = classLabel,
                        .f = function(x){
                          temp <- str_split(x, "\\|")[[1]]
                          map(str_split(temp, "="), tail, 1)
                        }) %>%
        unlist()

      # ... and add them to the ontology
      new_mapping(new = targetLabel, target = targetClass, source = dSeries,
                  match = "exact", certainty = 3, type = "class",
                  ontology = gazPath)

      unitCols <- targetClass$label

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

    # stop this iteration when 'inGeom' has zero rows
    if(dim(inGeom)[1] == 0){
      next
    }

    for(k in seq_along(unitCols)){
      names(inGeom)[[which(names(inGeom) == targetLabel[k])]] <- unitCols[k]
    }

    if(!topClass %in% names(inGeom)){
      inGeom <- inGeom %>%
        add_column(tibble(!!topClass := theUnits), .before = unitCols[1])
      unitCols <- c(topClass, unitCols)
      allCols <- allClasses[which(allClasses %in% head(unitCols, 1)) : which(allClasses %in% tail(unitCols, 1))]
    } else {
      allCols <- unitCols
    }

    # construct the geom for further processing
    if(priority == "ontology"){

      # current work-around to only translate geometries that have a full set of territorial columns available
      checkClasses <- allClasses[which(allClasses %in% head(unitCols, 1)) : which(allClasses %in% tail(unitCols, 1))]
      if(!testNames(x = unitCols, identical.to = checkClasses)){
        stop("some of the territory layers in this geometry are missing, please use the argument 'priority = spatial'.")
      }

      message("    harmonizing territory names ...")
      harmGeom <- matchOntology(table = inGeom,
                                columns = unitCols,
                                dataseries = dSeries,
                                ontology = gazPath,
                                verbose = verbose,
                                beep = beep)
      # include a warning here in case matches other than 'close' have been used, which would lead to problems matching them in the ontology, or, alternatively implement a system that would derive the correct IDs automatically, just like below for when a spatial match is chosen

    } else if(priority == "spatial"){

      harmGeom <- matchOntology(table = inGeom,
                                columns = unitCols[1],
                                dataseries = dSeries,
                                ontology = gazPath,
                                verbose = verbose,
                                beep = beep)

      harmGeom <- harmGeom %>%
        mutate(id = NA_character_,
               external = paste0(!!sym(tail(unitCols, 1)), "_-_-", row_number()))

      harmGeom <- harmGeom %>%
        select(all_of(unitCols), id, match, external, everything())

    }

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
          stage2Geom <- harmGeom %>%
            filter_at(vars(all_of(unitCols[1])), all_vars(. %in% tempUnit)) %>%
            select(all_of(allCols), id, match, external)
          assertChoice(x = allCols[1], choices = names(stage2Geom), .var.name = "names(nation_column)")
        } else {
          stage2Geom <- harmGeom %>%
            # select(all_of(allCols), id, match, external) %>%
            select(any_of(allCols), id, match, external)
        }

        # dissolve ----
        # in case the object consists of several POLYGONs per unique name, dissolve
        # them into a single MULTIPOLYGON
        if(unique(st_geometry_type(stage2Geom)) == "POLYGON"){
          uniqueUnits <- stage2Geom %>%
            as_tibble() %>%
            select(!!unitCols) %>%
            unique()

          if(all(!is.na(uniqueUnits))){
            if(dim(stage2Geom)[1] > dim(uniqueUnits)[1]){
              message("    Dissolving multiple polygons into a single multipolygon")

              stage2Geom <- stage2Geom %>%
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
        if(!all(st_is_valid(stage2Geom))){
          stage2Geom <- stage2Geom <- st_make_valid(x = stage2Geom)
        }

        # file exists? ----
        # determine whether a geometry with the nation as name already exists and
        # whether that contains the correct layer ...
        fileExists <- testFileExists(x = paste0(intPaths, "/adb_geometries/stage3/", tempUnit, ".gpkg"))
        if(fileExists){
          targetLayers <- st_layers(dsn = paste0(intPaths, "/adb_geometries/stage3/", tempUnit, ".gpkg"))
          if(!grepl(pattern = tail(targetClass$label, 1), x = paste0(targetLayers$name, collapse = "|"))){
            fileExists <- FALSE
          }
        }

        # ... if yes, read it in, otherwise create it
        if(fileExists | priority == "spatial"){

          # read target geoms ----
          message("    Reading target geometries")
          if(fileExists){

            topLayer <- targetLayers$name[which.min(targetLayers$features)]

            stage3Geom <- read_sf(dsn = paste0(intPaths, "/adb_geometries/stage3/", tempUnit, ".gpkg"),
                                  layer = tail(targetClass$label, 1),
                                  stringsAsFactors = FALSE)

            # determine the size of the sibling group
            stage3Geom <- stage3Geom %>%
              rowwise() %>%
              mutate(siblings = paste0(head(str_split_1(gazName, "[.]"), -1), collapse = ".")) %>%
              group_by(siblings) %>%
              mutate(siblings = n()) %>%
              ungroup()

            if(st_crs(stage3Geom)$input == "Undefined Cartesian SRS"){
              st_crs(stage3Geom) <- NA_crs_
            }

            # reproject new geom ----
            if(st_crs(stage2Geom) != st_crs(stage3Geom)){
              message("    Reprojecting new geometries")
              stage2Geom <- st_transform(x = stage2Geom, crs = st_crs(stage3Geom))
            }

            # test whether/which of the new features are already (spatially) in the target
            # geom and stop if all of them are there already.
            message("    Checking for exact spatial matches")
            equals <- unlist(st_equals(stage2Geom, stage3Geom))
            if(length(equals) == dim(stage2Geom)[1]){
              message("  ! --> all features of the new geometry are already part of the target geometry !")
              next
            }
          } else {
            message("  ! --> no previously defined geometry exists at stage 3 !")
            next
          }

          if(!topClass %in% tail(targetClass$label, 1)){
            stage3Parent <- read_sf(dsn = paste0(intPaths, "/adb_geometries/stage3/", tempUnit, ".gpkg"),
                                    layer = allClasses[which(allClasses %in% tail(targetClass$label, 1)) - 1],
                                    stringsAsFactors = FALSE) %>%
              filter(geoID %in% gIDs & gazClass == parentClass)
            if(simplify){
              stage3Parent <- stage3Parent %>%
                ms_simplify(keep = 0.5, method = "dp", keep_shapes = TRUE)
            }
            stage3Parent <- stage3Parent %>%
              st_make_valid() %>%
              mutate(s3_area = as.numeric(st_area(.))) %>%
              rowwise() %>%
              mutate(siblings = 0L) %>%
              select(-match, -external)
          }

          # determine geoms that are already ok ... ----
          message("    Joining target and source geometries")
          outGeom <- stage2Geom %>%
            filter(!is.na(id)) %>%
            # unite(col = "gazName", all_of(allCols), sep = ".") %>%
            unite(col = "gazName", any_of(allCols), sep = ".") %>%
            mutate(geoID = newGID,
                   gazClass = tail(allCols, 1),
                   match = paste0(match, " [xx<>xx_", id, "]")) %>%
            select(gazID = id, gazName, gazClass, match, external, geoID)

          stage2Geom <- stage2Geom %>%
            filter(is.na(id))

          if(dim(stage2Geom)[1] != 0){

            if(fileExists){
              message("    -> Simplifying geometries")
              stage3GeomSimple <- stage3Geom
              if(simplify){
                stage3GeomSimple <- stage3GeomSimple %>%
                  ms_simplify(keep = 0.5, method = "dp", keep_shapes = TRUE)
              }
              stage3GeomSimple <- stage3GeomSimple %>%
                st_make_valid() %>%
                mutate(s3_area = as.numeric(st_area(.))) %>%
                bind_cols(s3_geom = st_geometry(stage3Geom))

              if(priority == "ontology"){
                # in this case, names are matched first and for the remaining territories a spatial match is used
                stage3GeomSimple <- stage3GeomSimple %>%
                  filter(!gazID %in% outGeom$gazID) %>%
                  select(-match, -external)
              } else if(priority %in% "spatial"){
                # in this case, names are not matched at all and instead matches are determined by spatial overlap entirely
                outGeom <- outGeom %>%
                  slice(-c(1:dim(outGeom)[1]))
                stage2Geom <- stage2Geom
                stage3GeomSimple <- stage3GeomSimple %>%
                  select(-match, -external)
              }# else if(priority == "both"){
              # in this case, both methods are employed. Conflicts between the methods need to be resolved or documented
              #}

            }

            stage2GeomSimple <- stage2Geom
            if(simplify){
              stage2GeomSimple <- stage2GeomSimple %>%
                ms_simplify(keep = 0.5, method = "dp", keep_shapes = TRUE)
            }
            stage2GeomSimple <- stage2GeomSimple %>%
              st_make_valid() %>%
              mutate(s2_area = as.numeric(st_area(.))) %>%
              bind_cols(s2_geom = st_geometry(stage2Geom))

            if(fileExists){
              # ... and then carrying out an intersection
              message("    -> Calculating intersection")
              pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = dim(stage3GeomSimple)[1])
              geomIntersections <- st_intersects(x = stage3GeomSimple, y = stage2GeomSimple)
              overlapGeom <- suppressWarnings(
                map_dfr(1:dim(stage3GeomSimple)[1], function(ix){
                  pb$tick()
                  st_intersection(x = stage3GeomSimple[ix,], y = stage2GeomSimple[geomIntersections[[ix]],])
                })) %>%
                  st_make_valid() %>%
                  mutate(intersect_area = as.numeric(st_area(.)),
                         s3_prop = round(intersect_area/s3_area*100, 5),
                         s2_prop = round(intersect_area/s2_area*100, 5)) %>%
                  arrange(gazID)

              message("    -> Determining matches")
              tempGeom <- overlapGeom %>%
                mutate(outString = paste0(round(s2_prop), "<>", round(s3_prop)),
                       outMatch = if_else(s2_prop >= (100 - thresh),
                                          if_else(s3_prop >= (100 - thresh),
                                                  "close",
                                                  "narrower"),
                                          if_else(s2_prop < (100 - thresh) & s2_prop >= thresh,
                                                  "broader",
                                                  if_else(s2_prop < thresh,
                                                          if_else(s3_prop >= (100 - thresh),
                                                                  "broader",
                                                                  if_else(s3_prop >= thresh,
                                                                          "reassign",
                                                                          "ignore")),
                                                          "ignore")))) %>%
                filter(outMatch != "ignore") %>%
                group_by(gazName) %>%
                mutate(s3_overlap = n()) %>%
                arrange(desc(s3_prop)) %>%
                filter(outMatch != "reassign" | row_number() == 1L) %>%
                arrange(gazID) %>%
                ungroup() %>%
                group_by(external) %>%
                mutate(s2_overlap = n()) %>%
                ungroup() %>%
                mutate(outMatch = if_else((outMatch == "narrower" & s2_overlap == 1L & s3_overlap == 1L) | outMatch == "close",
                                          "close",
                                          if_else(outMatch == "reassign", "broader", outMatch)),
                       match = paste0(outMatch, " [", outString, "_", gazID, "]"),
                       new_name = if_else((s2_overlap != 1L | s3_overlap != 1L), TRUE, FALSE))

              message("    -> Building new IDs")
              tempGeom <- tempGeom %>%
                rowwise() %>%
                mutate(gazID = if_else((s2_overlap != 1L | s3_overlap != 1L), paste0(head(str_split_1(gazID, "[.]"), -1), collapse = "."), gazID),
                       gazName = if_else((s2_overlap != 1L | s3_overlap != 1L), paste0(head(str_split_1(gazName, "[.]"), -1), collapse = "."), gazName)) %>%
                group_by(external, s2_geom, gazID, gazName, new_name, siblings) %>%
                summarise(match = paste0(match, collapse = " | ")) %>%
                ungroup()

            } else {
              tempGeom <- stage2Geom %>%
                rename(s2_geom = geom) %>%
                mutate(new_name = TRUE,
                       match = "exact")
            }

            # in case new geometries overlap at the parent administrative level, get these information in there and assign the ID of that parent, where the largest overlap exists
            if(dim(tempGeom)[1] != length(unique(tempGeom$external)) | (priority == "spatial" & !fileExists)){

              message("    -> Adapting IDs to parent level")
              pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = dim(stage3Parent)[1])
              geomIntersections <- st_intersects(x = stage3Parent, y = stage2GeomSimple)
              parentOverlap <-  suppressWarnings(
                map_dfr(1:dim(stage3Parent)[1], function(ix){
                  pb$tick()
                  st_intersection(x = stage3Parent[ix,], y = stage2GeomSimple[geomIntersections[[ix]],])
                })) %>%
                mutate(intersect_area = as.numeric(st_area(.)),
                       s3_prop = round(intersect_area/s3_area*100, 5),
                       s2_prop = round(intersect_area/s2_area*100, 5)) %>%
                group_by(external) %>%
                arrange(s3_prop) %>%
                filter(row_number() == 1) %>%
                select(gazID, gazName, external) %>%
                # select(gazID, gazName, external, siblings) %>%
                st_drop_geometry()

              tempGeom_clean <- tempGeom %>%
                group_by(external) %>%
                filter(n() == 1)

              tempGeom_dups <- tempGeom %>%
                group_by(external) %>%
                filter(n() > 1)

              if(fileExists){
                tempGeom_dups <- tempGeom_dups %>%
                  rename(gazID2 = gazID) %>%
                  select(-gazName)
              }
              tempGeom <- tempGeom_dups %>%
                left_join(parentOverlap, by = "external") %>%
                group_by(external) %>%
                separate_rows(match, sep = " \\| ") %>%
                mutate(siblings = if_else(gazID2 == gazID, siblings, NA_integer_),
                       siblings = first(na.omit(siblings))) %>%
                ungroup() %>%
                group_by(external, s2_geom, new_name, gazID, gazName, siblings) %>%
                summarise(match = paste0(match, collapse = " | ")) %>%
                bind_rows(tempGeom_clean)

            }

            tempGeom <- tempGeom %>%
              separate_wider_delim(cols = external, delim = "_-_-", names = "external", too_many = "drop") %>%
              group_by(gazName) %>%
              mutate(rn = row_number()) %>%
              ungroup() %>%
              mutate(tempID = str_pad(string = rn + siblings, width = 3, pad = 0),
                     thisName = str_to_title(external),
                     gazID = if_else(!new_name, gazID, paste0(gazID, ".", tempID)),
                     gazName = if_else(!new_name, gazName, paste0(gazName, ".", thisName)),
                     geoID = newGID,
                     gazClass = tail(allCols, 1)) %>%
              select(gazID, gazName, gazClass, match, external, geoID, geom = s2_geom)

            if(!fileExists){
              tempGeom <- tempGeom %>%
                mutate(match = paste0("close [xx<>xx_", gazID, "]"))
            }

            outGeom <- outGeom %>%
              bind_rows(tempGeom)

          } else {
            message("    -> All ontology matches are valid")
          }

          # update ontology, but only if we are not handling the topmost class, because in that case it has been harmonised with the gazetteer already
          if(unique(outGeom$gazClass) != topClass){

            message("    -> Updating ontology")
            newOnto <- outGeom %>%
              st_drop_geometry() %>%
              rowwise() %>%
              mutate(parentID = paste0(head(str_split_1(gazID, "[.]"), -1), collapse = ".")) %>%
              separate_rows("match", sep = " \\| ") %>%
              mutate(match = str_replace_all(match, "\\[|\\]", "")) %>%
              separate(col = match, into = c("match", "amount"), sep = " ", fill = "right") %>%
              separate(col = amount, into = c("amount", "target"), sep = "_", fill = "right") %>%
              separate(col = amount, into = c("target_overlap", "source_overlap"), sep = "<>", fill = "right") %>%
              mutate(target_overlap = as.numeric(target_overlap),
                     source_overlap = as.numeric(source_overlap))

            # 1. include new hamonised concepts in the ontology, in case it deviates more than 'thresh'
            newConcepts <- newOnto %>%
              filter(target_overlap < (100-thresh) | source_overlap < (100-thresh))
            newConcepts <- newConcepts %>%
              select(id = parentID, gazClass, external) %>%
              left_join(get_concept(id = newConcepts$parentID, ontology = gazPath), by = "id") %>%
              select(external, gazClass, id, label, class) %>%
              distinct()

            if(dim(newConcepts)[1] != 0){

              new_concept(new = newConcepts$external,
                          broader = newConcepts %>% select(id, label, class),
                          class = newConcepts$gazClass,
                          ontology =  gazPath)

            }

            # 2. define mappings of the new concepts with harmonised concepts
            harmOnto <- get_concept(id = newOnto$target, ontology = gazPath)

            if(any(newOnto$match == "close")){
              newClose <- newOnto %>%
                filter(match == "close")
              new_mapping(new =  newClose$external,
                          target = harmOnto %>% filter(id %in% newClose$target) %>% select(id, label, class, has_broader),
                          source = dSeries, match = "close", certainty = 3, type = "concept", ontology = gazPath)
            }

            if(any(newOnto$match == "narrower")){
              newNarrower <- newOnto %>%
                filter(match == "narrower")
              oldNarrower <- harmOnto %>%
                filter(id %in% newNarrower$target) %>%
                select(id, label, class, has_broader) %>%
                left_join(newNarrower, by = c("id" = "target")) %>%
                select(id, label, class, has_broader)
              new_mapping(new = newNarrower$external,
                          target = oldNarrower,
                          source = dSeries, match = "narrower", certainty = 3, type = "concept", ontology = gazPath)
            }

            if(any(newOnto$match == "broader")){
              newBroader <- newOnto %>%
                filter(match == "broader")
              oldBroader <- harmOnto %>%
                filter(id %in% newBroader$target) %>%
                select(id, label, class, has_broader) %>%
                right_join(newBroader, ., by = c("target" = "id")) %>%
                select(id = target, label, class, has_broader)
              new_mapping(new = newBroader$external,
                          target = oldBroader,
                          source = dSeries, match = "broader", certainty = 3, type = "concept", ontology = gazPath)
            }
          }

          if(fileExists){

            outGeom <- stage3Geom %>%
              select(-siblings) %>%
              bind_rows(outGeom) %>%
              arrange(gazID, geoID)

          }

        } else {

          message("    Creating new basis dataset for class ", tail(targetClass$label, 1), ".")

          outGeom <- suppressMessages(
            stage2Geom %>%
              unite(col = "gazName", all_of(unitCols), sep = ".") %>%
              mutate(gazClass = tail(targetClass$label, 1),
                     geoID = newGID) %>%
              select(gazID = id, gazName, gazClass, match, external, geoID))

        }

        if(update){
          # in case the user wants to update, output the simple feature
          if(outType != "rds"){
            st_write(obj = outGeom,
                     dsn = paste0(intPaths, "/adb_geometries/stage3/", tempUnit, ".", outType),
                     layer = tail(targetClass$label, 1),
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
