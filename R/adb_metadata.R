#' Summarise database metadata
#'
#' Returns a summary of what is currently in the database: nations covered,
#' variables available, thematic concepts, and time range.
#' @return a list with elements \code{$nations}, \code{$variables},
#'   \code{$concepts}, \code{$years}.
#' @importFrom dplyr distinct pull bind_rows
#' @importFrom tibble tibble
#' @export

adb_metadata <- function(){

  intPaths <- .adb_state$path
  stage3   <- list.files(paste0(intPaths, "/tables/stage3"), pattern = "\\.rds$", full.names = TRUE)

  if(length(stage3) == 0){
    message("No stage3 tables found -- run normTable() first.")
    return(invisible(NULL))
  }

  nations   <- character()
  variables <- character()
  concepts  <- character()
  years     <- character()

  for(f in stage3){
    tbl <- tryCatch(readRDS(f), error = function(e) NULL)
    if(is.null(tbl)) next
    nations   <- c(nations,   sub("\\.rds$", "", basename(f)))
    variables <- c(variables, setdiff(names(tbl), c("gazID", "gazMatch", "gazClass", "ontoID",
                                                      "ontoName", "ontoMatch", "ontoClass",
                                                      "tabID", "geoID", "year")))
    if("year"    %in% names(tbl)) years    <- c(years,    unique(tbl$year))
    if("ontoName" %in% names(tbl)) concepts <- c(concepts, unique(tbl$ontoName))
  }

  out <- list(
    nations   = sort(unique(nations)),
    variables = sort(unique(variables)),
    concepts  = sort(unique(na.omit(concepts))),
    years     = sort(unique(years))
  )

  message(sprintf("Nations:   %d", length(out$nations)))
  message(sprintf("Variables: %s", paste(out$variables, collapse = ", ")))
  message(sprintf("Years:     %s - %s", min(out$years, na.rm = TRUE), max(out$years, na.rm = TRUE)))
  message(sprintf("Concepts:  %d unique", length(out$concepts)))

  invisible(out)
}
