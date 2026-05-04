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

  if("inventory" %in% what){
    message(" -> removing inventory")
    unlink(paste0(intPaths, "/inventory.rds"))
  }

  if("vocabularies" %in% what){
    message(" -> removing vocabulary terms, levels and mappings")
    for(sub in c("stage3", "mappings")){
      f <- list.files(file.path(intPaths, "vocabularies", sub),
                      pattern = "[.]parquet$", full.names = TRUE)
      if(length(f) != 0) file.remove(f)
    }
  }

  if("schemas" %in% what){
    message(" -> removing schemas")
    unlink(list.files(paste0(intPaths, "/tables/schemas/"), full.names = TRUE))
  }

  # move geometries from stage2/processed back to stage2 + clear stage3
  if("geometries" %in% what){
    message(" -> removing geometries")
    geom_stage2 <- list.files(path = paste0(intPaths, "/geometries/stage2/processed/"))
    geom_stage2_full <- list.files(path = paste0(intPaths, "/geometries/stage2/processed/"), full.names = TRUE)
    if(length(geom_stage2_full) != 0){
      file.copy(from = geom_stage2_full, paste0(intPaths, "/geometries/stage2/", geom_stage2))
      file.remove(geom_stage2_full)
    }

    geom_stage3_full <- list.files(path = paste0(intPaths, "/geometries/stage3"), full.names = TRUE)
    if(length(geom_stage3_full) != 0){
      file.remove(geom_stage3_full)
    }
  }

  # move tables from stage2/processed back to stage2 + clear stage3
  if("tables" %in% what){
    message(" -> removing tables")
    tab_stage2 <- list.files(path = paste0(intPaths, "/tables/stage2/processed/"))
    tab_stage2_full <- list.files(path = paste0(intPaths, "/tables/stage2/processed/"), full.names = TRUE)
    if(length(tab_stage2_full) != 0){
      file.copy(from = tab_stage2_full, paste0(intPaths, "/tables/stage2/", tab_stage2))
      file.remove(tab_stage2_full)
    }

    tab_stage3_full <- list.files(path = paste0(intPaths, "/tables/stage3"), full.names = TRUE)
    if(length(tab_stage3_full) != 0){
      file.remove(tab_stage3_full)
    }
  }

}
