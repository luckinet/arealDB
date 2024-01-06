#' Reset an areal database to its unfilled state
#'
#' @param onto [\code{logical(1)}]\cr whether or not to reset the ontology.
#' @param gaz [\code{logical(1)}]\cr whether or not to reset the gazetteer
#' @param schemas [\code{logical(1)}]\cr whether or not to reset schemas.
#' @param tables [\code{logical(1)}]\cr whether or not to reset the tables.
#' @param geometries [\code{logical(1)}]\cr whether or not to reset the
#'   geometries.
#' @return no return value, called for its side effect of reorganising an areal
#'   database into a state where no reg* or norm* functions have been run
#' @importFrom checkmate assertLogical
#' @export

adb_reset <- function(onto = TRUE, gaz = TRUE, schemas = TRUE,
                      tables = TRUE, geometries = TRUE){

  assertLogical(x = onto, len = 1, any.missing = FALSE)
  assertLogical(x = gaz, len = 1, any.missing = FALSE)

  # set internal paths
  intPaths <- paste0(getOption(x = "adb_path"))

  if(length(intPaths) == 0){
    stop("no areal database active!")
  }

  # remove metadata
  unlink(paste0(intPaths, "/meta/inventory.rds"))
  if(gaz) unlink(paste0(intPaths, "/meta/lucki_gazetteer.rds"))
  if(onto) unlink(paste0(intPaths, "/meta/lucki_onto.rds"))
  if(schemas) unlink(list.files(paste0(intPaths, "/meta/schemas/"), full.names = TRUE))

  # move geometries from stage2/processed, to stage2
  if(geometries){

    geom_stage2 <- list.files(path = paste0(intPaths, "/geometries/stage2/processed/"))
    geom_stage2_full <- list.files(path = paste0(intPaths, "/geometries/stage2/processed/"), full.names = TRUE)
    if(length(geom_stage2_full) != 0){
      file.copy(from = geom_stage2_full, paste0(intPaths, "/geometries/stage2/", geom_stage2))
      file.remove(geom_stage2_full)
    }

    # delete files from stage3
    geom_stage3_full <- list.files(path = paste0(intPaths, "/geometries/stage3"), full.names = TRUE)
    if(length(geom_stage3_full) != 0){
      file.remove(geom_stage3_full)
    }

  }

  # move tables from stage2/processed, to stage2
  if(tables){

    tab_stage2 <- list.files(path = paste0(intPaths, "/tables/stage2/processed/"))
    tab_stage2_full <- list.files(path = paste0(intPaths, "/tables/stage2/processed/"), full.names = TRUE)
    if(length(tab_stage2_full) != 0){
      file.copy(from = tab_stage2_full, paste0(intPaths, "/tables/stage2/", tab_stage2))
      file.remove(tab_stage2_full)
    }

    # delete files from stage3
    tab_stage3_full <- list.files(path = paste0(intPaths, "/tables/stage3"), full.names = TRUE)
    if(length(tab_stage3_full) != 0){
      file.remove(tab_stage3_full)
    }

  }

}