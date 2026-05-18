#' Summarise database metadata
#'
#' Returns a summary of what is currently in the database: nations covered,
#' variables available, thematic concepts, and time range.
#' @return a list with elements \code{$nations}, \code{$variables},
#'   \code{$concepts}, \code{$years}.
#' @importFrom dplyr distinct pull bind_rows
#' @importFrom tibble tibble as_tibble
#' @importFrom arrow read_parquet
#' @export

adb_metadata <- function(){

  intPaths <- .adb_state$path
  stage3   <- list.files(paste0(intPaths, "/tables/stage3"), pattern = "\\.parquet$", full.names = TRUE)

  if(length(stage3) == 0){
    message("No stage3 tables found -- run normTable() first.")
    return(invisible(NULL))
  }

  nations   <- character()
  variables <- character()
  concepts  <- character()
  years     <- character()

  for(f in stage3){
    tbl <- tryCatch(as_tibble(read_parquet(f, mmap = FALSE)), error = function(e) NULL)
    if(is.null(tbl)) next
    nations   <- c(nations,   sub("\\.parquet$", "", basename(f)))
    metaCols <- c("tabID", "geoID", "year")
    matchCols <- grep("(ID|Name|Match|Class)$", names(tbl), value = TRUE)
    variables <- c(variables, setdiff(names(tbl), c(metaCols, matchCols)))
    if("year" %in% names(tbl)) years <- c(years, unique(tbl$year))
    nameCols <- grep("Name$", names(tbl), value = TRUE)
    nameCols <- setdiff(nameCols, "gazetteerName")
    for(nc in nameCols) concepts <- c(concepts, unique(tbl[[nc]]))
  }

  out <- list(
    nations   = sort(unique(nations)),
    variables = sort(unique(variables)),
    concepts  = sort(unique(na.omit(concepts))),
    years     = sort(unique(years))
  )

  message(sprintf("  Nations : %d", length(out$nations)))
  message(sprintf("Variables : %s", paste(out$variables, collapse = ", ")))
  message(sprintf("    Years : %s - %s", min(out$years, na.rm = TRUE), max(out$years, na.rm = TRUE)))
  message(sprintf(" Concepts : %d unique", length(out$concepts)))

  invisible(out)
}
