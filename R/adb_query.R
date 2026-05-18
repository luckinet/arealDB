#' Extract database contents
#'
#' Retrieve a thematic variable for a territory together with the matching
#' geometry. If the underlying table has one concept column (i.e. a vocabulary
#' was used to harmonise an ID-variable), the result is pivoted on that
#' column's values; otherwise it is returned in long form.
#'
#' @param territory [`list(1)`][list]\cr the territory to extract, given as a
#'   single named list element where the name is a gazetteer class and the
#'   value is a canonical territory label, e.g.
#'   \code{list(ADM0 = "a_nation")}. All descendants of that territory are
#'   included.
#' @param concept [`list(1)`][list]\cr optional concept filter. Either
#'   \code{NULL} (default; all values of the table's concept column are
#'   returned), or a single named list element where the name is a concept
#'   column and the value(s) are labels to keep, e.g.
#'   \code{list(commodity = "barley")}. Required if the table contains more
#'   than one concept column, so the function can pick the right one.
#' @param variable [`character(1)`][character]\cr name of the observed
#'   variable to extract (e.g. \code{"harvested"}).
#' @param level [`character(1)`][character]\cr optional gazetteer class to
#'   restrict the result to (e.g. \code{"ADM1"}). Defaults to all levels
#'   present for the matching tables.
#' @param year [`integerish(.)`][integer]\cr optional vector of years to
#'   include. Defaults to all years present in the table.
#' @return A list with three elements:
#'   \describe{
#'     \item{\code{geom}}{the sf geometry of the territory at the deepest
#'       requested level.}
#'     \item{\code{tab}}{the variable extracted from the table. Long form
#'       (\code{gazID, year, <variable>}) when no concept column is present;
#'       otherwise wide on the concept values.}
#'     \item{\code{meta}}{per-territory metadata: number of source tables,
#'       number of distinct concept values (if applicable), year range.}
#'   }
#' @examples
#' if(dev.interactive()){
#' adb_example(path = paste0(tempdir(), "/newDB"))
#'
#' # filter to one concept value
#' adb_query(territory = list(ADM0 = "a_nation"),
#'           concept = list(commodity = "barley"),
#'           variable = "harvested")
#'
#' # all commodities (no concept filter)
#' adb_query(territory = list(ADM0 = "a_nation"),
#'           variable = "harvested")
#' }
#' @importFrom checkmate assertList assertCharacter assertIntegerish
#'   assertLogical assertSubset
#' @importFrom dplyr mutate select
#' @importFrom readr read_csv
#' @importFrom sf st_read
#' @importFrom arrow read_parquet
#' @importFrom tibble as_tibble
#' @export

adb_query <- function(territory = NULL, concept = NULL, variable = NULL,
                      level = NULL, year = NULL){

  assertList(x = territory, types = "character", any.missing = FALSE, null.ok = TRUE)
  assertList(x = concept, types = "character", any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = variable, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = level, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertIntegerish(x = year, min.len = 1, any.missing = FALSE, null.ok = TRUE)

  intPaths <- .adb_state$path
  inv <- readRDS(paste0(intPaths, "/inventory.rds"))

  gazName <- "gazetteer"
  load(paste0(intPaths, "/db_info.RData"))
  topClass <- db_info$level[1]

  inv_tables <- inv$tables
  inv_geoms <- inv$geometries
  inv_series <- inv$dataseries

  tables <- list.files(path = paste0(intPaths, "/tables/stage3"), full.names = TRUE, pattern = "\\.parquet$")
  geometries <- list.files(path = paste0(intPaths, "/geometries/stage3"), full.names = TRUE, pattern = "\\.gpkg$")

  # get all canonical terms and resolve the requested territory path
  # territory is a named list: outer entries narrow the parent context,
  # the final entry selects the operative set of canonical IDs.
  allItems <- .read_terms(gazName)
  .descendants <- function(items, ids){
    out <- ids
    frontier <- ids
    while(length(frontier) > 0){
      kids <- items$id[items$parent_id %in% frontier]
      kids <- setdiff(kids, out)
      if(length(kids) == 0) break
      out <- c(out, kids)
      frontier <- kids
    }
    out
  }
  # external mapping columns appear after id/label/class/parent_id/... in the
  # terms file (one column per registered dataseries source). Match labels
  # against the canonical label AND every mapping column, so users can pass
  # source-specific spellings.
  externalCols <- setdiff(names(allItems),
                          c("id", "label", "class", "parent_id", "description",
                            "valid_from", "valid_until", "successor_id"))

  currentIDs <- NULL
  for(k in seq_along(territory)){
    thisClass  <- names(territory)[k]
    thisLabels <- territory[[k]]
    inAnyCol <- allItems$label %in% thisLabels
    for(col in externalCols){
      inAnyCol <- inAnyCol | (allItems[[col]] %in% thisLabels)
    }
    candidates <- allItems |>
      filter(class == thisClass, inAnyCol)
    if(!is.null(currentIDs)){
      candidates <- candidates |> filter(parent_id %in% .descendants(allItems, currentIDs))
    }
    currentIDs <- candidates |> pull(id)
    if(length(currentIDs) == 0){
      stop("no canonical terms found for territory entry ",
           thisClass, " = ",
           paste(thisLabels, collapse = ", "))
    }
  }

  # collect all descendants of the operative IDs (for in-table filtering)
  outIDs <- .descendants(allItems, currentIDs)

  # find the topClass-level ancestors -- stage3 files are split at topClass
  .ancestors <- function(items, ids){
    out <- ids
    frontier <- ids
    while(length(frontier) > 0){
      parents <- items$parent_id[items$id %in% frontier]
      parents <- parents[!is.na(parents) & nzchar(parents)]
      parents <- setdiff(parents, out)
      if(length(parents) == 0) break
      out <- c(out, parents)
      frontier <- parents
    }
    out
  }
  allRelated <- union(outIDs, .ancestors(allItems, currentIDs))
  tableNames <- allItems |>
    filter(id %in% allRelated & class == topClass)

  metaNames <- c("sources", "years", "min_year", "max_year")

  for(i in seq_along(tableNames$label)){

    nation <- tableNames$label[i]

    table <- as_tibble(read_parquet(file = tables[str_detect(string = tables, pattern = paste0(nation, ".parquet"))], mmap = FALSE)) |>
      rename(any_of(c(gazID = "gazetteerID", gazName = "gazetteerName",
                      gazMatch = "gazetteerMatch", gazClass = "gazetteerClass")))

    assertSubset(x = variable, choices = colnames(table))

    # detect concept columns (thematic vocab Name-columns present in the table)
    conceptCols <- grep("Name$", names(table), value = TRUE)
    conceptCols <- setdiff(conceptCols, c("gazetteerName", "gazName"))

    if(is.null(concept)){
      if(length(conceptCols) > 1){
        stop("table for '", nation, "' has multiple concept columns (",
             paste(conceptCols, collapse = ", "),
             "); specify which via 'concept'.")
      }
      conceptCol <- if(length(conceptCols) == 1) conceptCols else NULL
      conceptVals <- NULL
    } else {
      # accept either "animal" or "animalName" from the user
      userKey <- names(concept)
      conceptCol <- if(userKey %in% colnames(table)) userKey else paste0(userKey, "Name")
      assertSubset(x = conceptCol, choices = colnames(table))
      theConcepts <- table |>
        distinct(!!sym(conceptCol)) |>
        pull(conceptCol)
      theConcepts <- theConcepts[!is.na(theConcepts)]
      conceptVals <- unlist(concept, use.names = FALSE)
      assertSubset(x = conceptVals, choices = c(theConcepts, metaNames),
                   .var.name = paste0('concept = list(', userKey, ' = "', conceptVals, '")'))
    }

    # build metadata
    tabIDs <- table |>
      distinct(tabID)

    temp_inv_tables <- inv_tables |>
      filter(tabID %in% tabIDs$tabID)

    if(is.null(level)){
      tempLvls <- temp_inv_tables |>
        distinct(level) |>
        pull()
    } else {
      assertSubset(x = level, choices = unique(temp_inv_tables$level))
      tempLvls <- level
    }

    if(is.null(year)){
      targetYear <- table |>
        distinct(year) |>
        pull(year)
      # targetYear <- get all years and provide a prompt to ask whether all should be printed or just a subset
    } else {
      assertSubset(x = as.character(year), choices = unique(table$year))
      targetYear <- as.character(year)
    }

    # load geometries
    geometry <- suppressMessages(
      read_sf(dsn = geometries[str_detect(string = geometries, pattern = paste0(nation, ".gpkg"))],
              layer = max(tempLvls))) |>
      filter(geoID %in% temp_inv_tables$geoID,
             gazID %in% outIDs)

    # build some meta data (restricted to the requested territories)
    tableFiltered <- table |>
      filter(!is.na(gazID), gazID %in% outIDs, year %in% targetYear)
    if(is.null(conceptCol)){
      meta <- tableFiltered |>
        group_by(gazID, geoID) |>
        summarise(n_source = n_distinct(tabID),
                  n_year = n_distinct(year),
                  min_year = min(year),
                  max_year = max(year)) |>
        ungroup()
    } else {
      meta <- tableFiltered |>
        group_by(gazID, geoID) |>
        summarise(n_source = n_distinct(tabID),
                  !!paste0("n_", conceptCol) := n_distinct(!!sym(conceptCol)),
                  n_year = n_distinct(year),
                  min_year = min(year),
                  max_year = max(year)) |>
        ungroup()
    }

    # summarise variables
    var <- table |>
      filter(!is.na(gazID)) |>
      filter(gazID %in% outIDs) |>
      filter(year %in% targetYear)
    if(!is.null(conceptVals)){
      var <- var |> filter(!!sym(conceptCol) %in% conceptVals)
    }
    if(is.null(conceptCol)){
      var <- var |>
        select(gazID, year, all_of(variable))
    } else {
      var <- var |>
        pivot_wider(id_cols = c(gazID, year), names_from = all_of(conceptCol),
                    values_from = all_of(variable), values_fn = mean)
    }

    out <- list(geom = geometry,
                tab = var,
                meta = meta)

    return(out)

    # plotSteps <- function(x){
    #
    #  thisVar <- var |>
    #    filter(year == x)
    #
    #   # put it together
    #  full <- geometry |>
        # filter(geoID %in% temp_inv_tables$geoID) |>
        # left_join(meta, by = c("gazID", "geoID")) |>
        # left_join(thisVar, by = "gazID")

    #   thisGeoID <- full |>
    #     as_tibble() |>
    #     distinct(geoID)
    #
    #   temp_inv_geoms <- inv_geoms |>
    #     filter(geoID %in% thisGeoID$geoID)
    #   temp_inv_series <- inv_series |>
    #     filter(datID %in% temp_inv_geoms$datID)
    #
    #   if(theConcept %in% metaNames){
    #     titleString <- theConcept
    #   } else {
    #     titleString <- paste0(theConcept, " in ", x)
    #   }
    #
    #   p <- ggplot(full) +
    #     geom_sf(aes(fill = !!sym(theConcept)), lwd = 0.05, color = "white") +
    #     scale_fill_gradientn(
    #       colours = c("#F1E4DBFF", "#D7C9BEFF", "#8B775FFF", "#9AA582FF", "#657359FF"),
    #       limits = c(1, max(var[theConcept], na.rm = TRUE)),
    #       name = variable,
    #       transform = "log10"
    #     ) +
    #     labs(
    #       title = titleString,
    #       subtitle = paste0(nation, " at admin level ", max(tempLvls)),
    #       caption = paste0("Source: ", temp_inv_series$description, " (", temp_inv_series$homepage, ")")
    #     ) +
    #     theme_map() +
    #     theme(
    #       legend.position = "top",
    #       legend.justification = 0.5,
    #       legend.key.width = unit(1.75, "cm"),
    #       legend.margin = margin(),
    #       plot.title = element_text(size = 20, hjust = 0.5),
    #       plot.subtitle = element_text(hjust = 0.5)
    #     )
    #   print(p)
    # }

    # if(animate){
    #
    #   saveGIF(for(j in seq_along(targetYear)){
    #     plotSteps(targetYear[j])
    #   }, interval = 1, movie.name = paste0(intPaths, "/", nation, "_", theConcept, "_", min(year), "_", max(year), ".gif"))
    #
    # } else {
    #
    #   for(j in seq_along(targetYear)){
    #     plotSteps(targetYear[j])
    #   }
    #
    # }

  }
}
