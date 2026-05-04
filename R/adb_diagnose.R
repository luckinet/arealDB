#' Diagnose database contents
#'
#' Compares the stage2 and stage3 directories to report which dataseries have
#' been registered, which have been normalised, and which are still pending.
#' Also reports any stage3 files that contain a high proportion of unmatched
#' territories (\code{NA} in \code{gazID}).
#' @param verbose logical; if \code{TRUE}, print a full per-nation summary
#'   (default \code{FALSE}).
#' @return invisibly returns a list with elements \code{$status} (a tibble of
#'   registered vs normalised counts per dataseries) and \code{$unmatched} (a
#'   tibble of nations with NA gazID proportion above 0).
#' @importFrom checkmate assertLogical
#' @importFrom dplyr mutate filter select arrange bind_rows
#' @importFrom tibble tibble
#' @export

adb_diagnose <- function(verbose = FALSE){

  assertLogical(x = verbose, len = 1, any.missing = FALSE)

  intPaths  <- .adb_state$path
  inventory <- readRDS(paste0(intPaths, "/inventory.rds"))

  inv_tables    <- inventory$tables
  inv_geoms     <- inventory$geometries
  inv_series    <- inventory$dataseries

  stage2_tables <- list.files(paste0(intPaths, "/tables/stage2"),    pattern = "\\.csv$")
  stage2_geoms  <- list.files(paste0(intPaths, "/geometries/stage2"), pattern = "\\.gpkg$")
  stage3_tables <- list.files(paste0(intPaths, "/tables/stage3"),    pattern = "\\.rds$",  full.names = TRUE)
  stage3_geoms  <- list.files(paste0(intPaths, "/geometries/stage3"), pattern = "\\.gpkg$", full.names = TRUE)

  # registered vs stage2 vs stage3
  status <- tibble(
    stage      = c("registered tables", "stage2 tables", "stage3 tables",
                   "registered geometries", "stage2 geometries", "stage3 geometries"),
    count      = c(nrow(inv_tables), length(stage2_tables), length(stage3_tables),
                   nrow(inv_geoms),  length(stage2_geoms),  length(stage3_geoms))
  )

  message("=== arealDB diagnostic ===")
  message(sprintf("  registered tables:    %d", nrow(inv_tables)))
  message(sprintf("  stage2 tables:        %d  (awaiting normalisation)", length(stage2_tables)))
  message(sprintf("  stage3 tables:        %d  (normalised)", length(stage3_tables)))
  message(sprintf("  registered geoms:     %d", nrow(inv_geoms)))
  message(sprintf("  stage2 geoms:         %d  (awaiting normalisation)", length(stage2_geoms)))
  message(sprintf("  stage3 geoms:         %d  (normalised)", length(stage3_geoms)))

  # check NA gazID proportion in each stage3 table
  unmatched <- bind_rows(lapply(stage3_tables, function(f){
    tbl <- tryCatch(readRDS(f), error = function(e) NULL)
    if(is.null(tbl) || !"gazID" %in% names(tbl)) return(NULL)
    n_na  <- sum(is.na(tbl$gazID))
    n_tot <- nrow(tbl)
    if(n_na == 0) return(NULL)
    tibble(nation = sub("\\.rds$", "", basename(f)),
           n_rows = n_tot, n_unmatched = n_na,
           pct_unmatched = round(n_na / n_tot * 100, 1))
  }))

  if(nrow(unmatched) > 0){
    message(sprintf("\n  nations with unmatched territories (%d):", nrow(unmatched)))
    if(verbose){
      print(arrange(unmatched, -pct_unmatched))
    } else {
      message(sprintf("  (use verbose = TRUE to see per-nation breakdown)"))
    }
  } else {
    message("\n  all stage3 territories matched.")
  }

  invisible(list(status = status, unmatched = unmatched))
}
