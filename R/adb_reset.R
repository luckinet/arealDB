#' Reset an areal database to its unfilled state
#'
#' @param what [`character(.)`][character]\cr what to reset; one or more of
#'   \code{"vocabularies"}, \code{"schemas"}, \code{"tables"},
#'   \code{"geometries"}, \code{"inventory"}, or \code{"all"} (the default).
#' @return no return value, called for its side effect of reorganising an areal
#'   database into a state where no reg* or norm* functions have been run
#' @importFrom checkmate assertSubset
#' @export

adb_reset <- function(what = "all"){

  choices <- c("vocabularies", "schemas", "tables", "geometries", "inventory", "all")
  assertSubset(x = what, choices = choices)
  if("all" %in% what){
    what <- c("vocabularies", "schemas", "tables", "geometries", "inventory")
  }

  intPaths <- .adb_state$path
  if(length(intPaths) == 0){
    stop("no areal database active!")
  }

  # load inventory once so per-component blocks can roll status back to "staged".
  # if the inventory is also being wiped, skip the flip - the file is going away.
  inventoryPath <- paste0(intPaths, "/inventory.rds")
  rollbackStatus <- !("inventory" %in% what) && file.exists(inventoryPath)
  if(rollbackStatus){
    inventory <- readRDS(inventoryPath)
  }

  if("vocabularies" %in% what){
    message(" -> removing vocabulary terms, levels and mappings")
    # stage3 holds terms/levels as parquet; mappings holds per-dataseries csvs
    patterns <- list(stage3 = "[.]parquet$", mappings = "[.](parquet|csv)$")
    for(sub in names(patterns)){
      f <- list.files(file.path(intPaths, "vocabularies", sub),
                      pattern = patterns[[sub]], full.names = TRUE)
      if(length(f) != 0) file.remove(f)
    }
    if(rollbackStatus && !is.null(inventory$vocabularies)){
      inventory$vocabularies$status[inventory$vocabularies$status == "normalised"] <- "staged"
    }
  }

  if("schemas" %in% what){
    message(" -> removing schemas")
    unlink(list.files(paste0(intPaths, "/tables/schemas/"), full.names = TRUE))
  }

  if("geometries" %in% what){
    message(" -> removing geometries")
    geom_stage3_full <- list.files(path = paste0(intPaths, "/geometries/stage3"), full.names = TRUE)
    if(length(geom_stage3_full) != 0){
      file.remove(geom_stage3_full)
    }
    if(rollbackStatus && !is.null(inventory$geometries)){
      inventory$geometries$status[inventory$geometries$status == "normalised"] <- "staged"
    }
  }

  if("tables" %in% what){
    message(" -> removing tables")
    tab_stage3_full <- list.files(path = paste0(intPaths, "/tables/stage3"), full.names = TRUE)
    if(length(tab_stage3_full) != 0){
      file.remove(tab_stage3_full)
    }
    if(rollbackStatus && !is.null(inventory$tables)){
      inventory$tables$status[inventory$tables$status == "normalised"] <- "staged"
    }
  }

  if(rollbackStatus){
    saveRDS(object = inventory, file = inventoryPath)
  }

  if("inventory" %in% what){
    message(" -> removing inventory")
    unlink(inventoryPath)
  }

}
