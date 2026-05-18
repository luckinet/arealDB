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
#' @importFrom tibble tibble as_tibble
#' @importFrom arrow read_parquet
#' @export

adb_diagnose <- function(verbose = FALSE){

  assertLogical(x = verbose, len = 1, any.missing = FALSE)

  intPaths  <- .adb_state$path
  inventory <- readRDS(paste0(intPaths, "/inventory.rds"))

  inv_tables    <- inventory$tables
  inv_geoms     <- inventory$geometries
  inv_series    <- inventory$dataseries

  stage3_tables <- list.files(paste0(intPaths, "/tables/stage3"),    pattern = "\\.parquet$",  full.names = TRUE)
  stage3_geoms  <- list.files(paste0(intPaths, "/geometries/stage3"), pattern = "\\.gpkg$", full.names = TRUE)

  tables_pending <- sum(inv_tables$status != "normalised")
  tables_done    <- sum(inv_tables$status == "normalised")
  geoms_pending  <- sum(inv_geoms$status  != "normalised")
  geoms_done     <- sum(inv_geoms$status  == "normalised")

  status <- tibble(
    stage = c("registered tables", "pending tables", "normalised tables",
              "registered geometries", "pending geometries", "normalised geometries"),
    count = c(nrow(inv_tables), tables_pending, tables_done,
              nrow(inv_geoms),  geoms_pending,  geoms_done)
  )

  message("=== arealDB diagnostic ===")
  message(sprintf("  registered tables:     %d", nrow(inv_tables)))
  message(sprintf("    pending:             %d", tables_pending))
  message(sprintf("    normalised:          %d", tables_done))
  message(sprintf("  registered geoms:      %d", nrow(inv_geoms)))
  message(sprintf("    pending:             %d", geoms_pending))
  message(sprintf("    normalised:          %d", geoms_done))

  # check NA gazID proportion in each stage3 table
  unmatched <- bind_rows(lapply(stage3_tables, function(f){
    tbl <- tryCatch(as_tibble(read_parquet(f, mmap = FALSE)), error = function(e) NULL)
    if(is.null(tbl)) return(NULL)
    gazCol <- if("gazetteerID" %in% names(tbl)) "gazetteerID" else if("gazID" %in% names(tbl)) "gazID" else NULL
    if(is.null(gazCol)) return(NULL)
    n_na  <- sum(is.na(tbl[[gazCol]]))
    n_tot <- nrow(tbl)
    if(n_na == 0) return(NULL)
    tibble(nation = sub("\\.parquet$", "", basename(f)),
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
