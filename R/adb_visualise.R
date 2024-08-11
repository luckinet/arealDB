#' Visualise database contents
#'
#' @param ... combination of column name in the ontology and value to filter
#'   that column by to build a tree of the concepts nested into it; see
#'   \code{\link[ontologics]{make_tree}}.
#' @param territory description
#' @param concept description
#' @param level description
#' @param year description
#' @param diagnose description
#' @return returns ...
#' @importFrom dplyr mutate select
#' @importFrom sf st_read
#' @export

adb_visualise <- function(..., territory = NULL, concept = NULL,
                          variable = NULL, level = NULL, year = NULL,
                          diagnose = FALSE){

  # territory <- list(al1 = "Brazil"); level = "al3"; year = 2010; diagnose = FALSE

  assertList(x = territory, types = "character", any.missing = FALSE, null.ok = TRUE)
  assertList(x = concept, types = "character", any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = variable, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = level, len = 1, any.missing = FALSE, null.ok = TRUE)
  assertIntegerish(x = year, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  assertLogical(x = diagnose, len = 1, all.missing = FALSE)

  adb_path <- getOption(x = "adb_path")
  inv <- readRDS(paste0(adb_path, "/_meta/inventory.rds"))

  gazPath <- paste0(getOption(x = "gazetteer_path"))
  gazClasses <- get_class(ontology = gazPath)
  topClass <- paste0(getOption(x = "gazetteer_top"))

  inv_tables <- inv$tables
  inv_geoms <- inv$geometries
  inv_series <- inv$dataseries

  tables <- list.files(path = paste0(adb_path, "/tables/stage3"), full.names = TRUE)
  geometries <- list.files(path = paste0(adb_path, "/geometries/stage3"), full.names = TRUE)

  # first, get all items down until the "topClass"
  allItems <- make_tree(class = topClass, reverse = TRUE, ontology = gazPath)
  top <- get_concept(class = names(territory), label = territory[[1]], ontology = gazPath) %>%
    pull(id)

  # then select those that are determined by "broadest"
  fin <- NULL
  outIDs <- top
  while(is.null(fin)){
    childID <- allItems %>%
      filter(has_broader %in% top) %>%
      pull(id)
    if(length(childID) != 0){
      top <- childID
      outIDs <- c(outIDs, childID)
    } else {
      fin <- TRUE
    }
  }

  tableNames <- allItems |>
    filter(id %in% outIDs & class == "al1")

  for(i in seq_along(tableNames$label)){

    nation <- tableNames$label[i]

    table <- readRDS(file = tables[str_detect(string = tables, pattern = paste0(nation, ".rds"))])

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
      # taregetYear <- get all years and provide a prompt to ask whether all should be printed or just a subset
    } else {
      assertSubset(x = year, choices = unique(table$year))
      taregetYear <- year
    }

    geometry <- st_read(dsn = geometries[str_detect(string = geometries, pattern = paste0(nation, ".gpkg"))],
                        layer = max(tempLvls))

    diag <- list()
    if(diagnose){

      # visually diagnose some missing data
      diag$gazID <- table |>
        filter(is.na(gazID)) |>
        distinct(gazMatch)
      diag$ontoID <- table |>
        filter(is.na(ontoID)) |>
        distinct(ontoMatch)

    } else {
      diag <- NULL
    }

    meta <- table |>
      group_by(gazID, geoID) |>
      summarise(sources = n_distinct(tabID),
                animals = n_distinct(animal),
                years = n_distinct(year),
                min_year = min(year),
                max_year = max(year)) |>
      ungroup()

    # summarise variables
    var <- table |>
      filter(!is.na(gazID)) |>
      filter(year == taregetYear) |>
      filter(str_detect(ontoMatch, "close")) |>
      pivot_wider(id_cols = c(gazID), names_from = animal, values_from = number_heads, values_fn = mean)

    # put it together
    full <- geometry |>
      filter(geoID %in% temp_inv_tables$geoID) |>
      left_join(meta, by = c("gazID", "geoID")) |>
      left_join(var, by = "gazID")

    thisGeoID <- full |>
      as_tibble() |>
      distinct(geoID)

    temp_inv_geoms <- inv_geoms |>
      filter(geoID %in% thisGeoID$geoID)
    temp_inv_series <- inv_series |>
      filter(datID %in% temp_inv_geoms$datID)




  }



  return(diag)

}