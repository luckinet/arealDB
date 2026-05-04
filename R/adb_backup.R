#' Backup the current state of an areal database
#'
#' @details This function creates a tag composed of the version and the
#'   current date and appends it to all stage 3 files (tables and geometries),
#'   the inventory and every registered vocabulary's parquet files. The tagged
#'   copies are written to the backup folder of the current areal database.
#' @return No return value, called for the side effect of saving the inventory,
#'   the stage 3 files and the vocabulary files into the backup directory.
#' @importFrom checkmate testDirectoryExists
#' @importFrom progress progress_bar
#' @importFrom purrr map
#' @importFrom stringr str_split
#' @importFrom tools file_ext file_path_sans_ext
#' @export

adb_backup <- function(){

  intPaths <- .adb_state$path
  load(paste0(intPaths, "/db_info.RData"))

  version <- paste0(db_info$version, "_", format(Sys.Date(), "%Y%m%d"))

  # tag a filename: <stem>_<version>.<ext>
  tag <- function(filename){
    paste0(file_path_sans_ext(filename), "_", version, ".", file_ext(filename))
  }

  # ensure backup directory tree
  for(sub in c("", "_meta", "tables", "geometries",
               "vocabularies", "vocabularies/stage3", "vocabularies/mappings")){
    p <- file.path(intPaths, "backup", sub)
    if(!testDirectoryExists(x = p)) dir.create(p, recursive = TRUE)
  }

  message(" -> backing up inventory")
  file.copy(from = paste0(intPaths, "/inventory.rds"),
            to   = paste0(intPaths, "/backup/_meta/inventory_", version, ".rds"),
            overwrite = TRUE)

  copy_tagged <- function(src_dir, dst_dir, label){
    files <- list.files(path = src_dir, full.names = FALSE)
    if(length(files) == 0) return(invisible())
    message(" -> backing up ", label)
    pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)",
                           total = length(files))
    tagged <- vapply(files, function(f){ pb$tick(); tag(f) }, character(1))
    file.copy(from = file.path(src_dir, files),
              to   = file.path(dst_dir, tagged),
              overwrite = TRUE)
  }

  copy_tagged(file.path(intPaths, "tables", "stage3"),
              file.path(intPaths, "backup", "tables"),
              "tables")
  copy_tagged(file.path(intPaths, "geometries", "stage3"),
              file.path(intPaths, "backup", "geometries"),
              "geometries")
  copy_tagged(file.path(intPaths, "vocabularies", "stage3"),
              file.path(intPaths, "backup", "vocabularies", "stage3"),
              "vocabulary terms/levels")
  copy_tagged(file.path(intPaths, "vocabularies", "mappings"),
              file.path(intPaths, "backup", "vocabularies", "mappings"),
              "vocabulary mappings")

  invisible()
}
