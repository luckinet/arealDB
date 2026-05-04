#' Restore the database from a backup
#'
#' @param version [`character(1)`][character]\cr a version tag for which to
#'   restore files.
#' @param date [`character(1)`][character]\cr a date for which to restore files.
#' @details This function searches for files that have the version and date tag,
#'   as it was defined in a previous run of \code{\link{adb_backup}}, to restore
#'   them to their original folders. This function overwrites by default, so use
#'   with care.
#' @return No return value, called for the side effect of restoring files that
#'   were previously stored in a backup.
#' @importFrom stringr str_replace
#' @export

adb_restore <- function(version = NULL, date = NULL){

  intPaths <- .adb_state$path

  versionTag <- paste0(version, "_", date)

  restore_dir <- function(backup_dir, target_dir, label){
    if(!dir.exists(backup_dir)){
      message("no ", label, " backup directory")
      return(invisible())
    }
    src <- list.files(path = backup_dir, pattern = versionTag, full.names = TRUE)
    if(length(src) == 0){
      message("no ", label, " found for tag '", versionTag, "'")
      return(invisible())
    }
    dst <- file.path(target_dir,
                     str_replace(basename(src),
                                 pattern = paste0("_", versionTag),
                                 replacement = ""))
    file.copy(from = src, to = dst, overwrite = TRUE)
  }

  restore_dir(file.path(intPaths, "backup", "_meta"),
              file.path(intPaths, "_meta"),
              "inventory")
  restore_dir(file.path(intPaths, "backup", "tables"),
              file.path(intPaths, "tables", "stage3"),
              "tables")
  restore_dir(file.path(intPaths, "backup", "geometries"),
              file.path(intPaths, "geometries", "stage3"),
              "geometries")
  restore_dir(file.path(intPaths, "backup", "vocabularies", "stage3"),
              file.path(intPaths, "vocabularies", "stage3"),
              "vocabulary terms/levels")
  restore_dir(file.path(intPaths, "backup", "vocabularies", "mappings"),
              file.path(intPaths, "vocabularies", "mappings"),
              "vocabulary mappings")

  invisible()
}
