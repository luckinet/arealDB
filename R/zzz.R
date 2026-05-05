# define global variables for internal use
globalVariables(c(
  ".",
  ".data",
  "base_area",
  "base_prop",
  "base_siblings",
  "base_name",
  "base_overlap",
  "base_ID",
  "new_area",
  "new_prop",
  "targetID",
  "target_prop",
  "target_area",
  "amount",
  "col_name",
  "col_type",
  "description",
  "dup",
  "external",
  "externalID",
  "gazClass",
  "gazID",
  "gazID2",
  "gazMatch",
  "gazName",
  "geoID",
  "geom",
  "has_broader",
  "has_broader_match",
  "has_close_match",
  "has_exact_match",
  "has_narrower_match",
  "id",
  "intersect_area",
  "label",
  "len new_label",
  "new_name",
  "new_overlap",
  "outMatch",
  "outString",
  "parentID",
  "rn",
  "siblings",
  "source_overlap",
  "target_overlap",
  "tempID",
  "thisName",
  "unitCol",
  "new_label",
  "len",
  "dataseries",
  "newID",
  "harmLab",
  "sort_in",
  "label_harm",
  "label_new",
  "label.x",
  "label.y",
  "dist",
  "dist_0",
  "dist_1",
  "dist_2",
  "has_0_differences",
  "has_new_close_match",
  "db_info",
  "id_new",
  "rest",
  "parentName",
  "new_parent_label",
  "has_new_broader",
  "has_source",
  "tabID",
  "ontoID",
  "ontoMatch",
  "ontoName",
  "ontoClass",
  "datID",
  "thisGeoID",
  "keep",
  "simple",
  "orig",
  "source_label",
  "canonical_id",
  "note",
  "term_parent",
  "parent_id",
  "class",
  "orig_",
  "term_level",
  "parent_id.y",
  "source",
  "simplified",
  "match",
  "canonical_id_resolved",
  "suffix",
  "parent_class",
  "parent_label",
  "src_raw",
  "canonical_label",
  "pct_unmatched",
  "is_new",
  "is_new.x",
  "is_new.y",
  "canonical_id_na",
  "note_na",
  "parent_id.x",
  "col_character",
  "matched_id"
))

#' Package-private state for the active areal database.
#'
#' Slots:
#' \itemize{
#'   \item \code{path} — root directory of the active areal database, set by
#'     \code{\link{adb_init}}.
#'   \item \code{testing} — TRUE while \code{\link{adb_example}} runs, so
#'     interactive prompts in reg* functions are skipped.
#' }
.adb_state <- new.env(parent = emptyenv())
.adb_state$path    <- NULL
.adb_state$testing <- FALSE

.onLoad <- function(libname, pkgname) {
  # CRAN OMP THREAD LIMIT
  Sys.setenv("OMP_THREAD_LIMIT" = 1)
}

.onAttach <- function(libname, pkgname){
  .adb_state$testing <- FALSE
}