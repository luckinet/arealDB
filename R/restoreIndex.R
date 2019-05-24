#' Restore an index
#'
#' @param pattern [\code{character(1)}]\cr name of the index that shall be
#'   restored.
#' @param print [\code{logical(1)}]\cr whether or not a summary of the backed up
#'   indices should be printed.
#' @param select [\code{character(1)}]\cr name of the table to which the table
#'   should be reverted. Alternatively \code{"oldest"} or \code{"newest"}.
#' @details Whenever an index is updated with \code{\link{updateIndex}}, a
#'   backup of it is stored. When restoring one of these backup files with
#'   \code{restoreIndex}, the changes of \code{updateIndex} will be rolled back
#'   to the status of the selected backup.
#' @examples
#'
#' \dontrun{
#' setPath(root = "/home/se87kuhe/Nextcloud/LUCKINet/data/")
#'
#' # first print the existing logs to chose from
#' restoreIndex(print = TRUE)
#'
#' # then restore one
#' restoreIndex(pattern = "inv_geometries", select = "newest")
#' }
#' @importFrom checkmate assertCharacter test_true assertLogical
#' @importFrom utils tail
#' @export

restoreIndex <- function(pattern = NULL, print = FALSE, select = NULL){

  # check validity of arguments
  assertCharacter(x = pattern, len = 1, null.ok = TRUE)
  if(is.null(pattern)) pattern <- ""
  assertLogical(x = print, len = 1)
  path <- getOption("cT_path")

  printIndex <- function(index){
    temp <- suppressMessages(read_csv(paste0(path, "/log/", index)))
    nameParts <- strsplit(x = index, "_")[[1]]
    nameParts <- nameParts[length(nameParts)]
    nameParts <- strsplit(nameParts, "[.]")[[1]][1]
    message(paste0("table ", i, ": ", dim(temp)[1], " x ", dim(temp)[2], " (timestamp: ", nameParts, ") -> showing last 6 rows"))
    toPrint <- tail(temp)
    print(toPrint)
    message()
  }

  backups <- list.files(path = paste0(path, "/log"))
  toRestoreOpt <- rev(backups[grepl(pattern = pattern, x = backups)])

  if(print){

    if(length(toRestoreOpt) > 1){
      theSeq <- seq_along(toRestoreOpt)
      maxIt <- 0
      while(maxIt <= length(theSeq)){
        maxIt <- maxIt + 3
        seqAlong <- (maxIt-2):maxIt
        if(maxIt > length(theSeq)){
          seqAlong <- (maxIt-2):length(theSeq)
        }

        for(i in seqAlong){
          theIndex <- toRestoreOpt[i]
          printIndex(index = theIndex)
        }

        if(maxIt > length(theSeq)){
          toRestore <- readline("please select the table number you want to restore: ")
        } else{
          toRestore <- readline("please select the table number you want to restore or type [older] for more entries: ")
        }

        if(test_true(suppressWarnings(!is.na(as.integer(toRestore))))){
          toRestore <- as.integer(toRestore)
          out <- toRestoreOpt[toRestore]
          maxIt <- length(theSeq)+1
        } else if(toRestore == "older"){
          if(maxIt > length(theSeq)){
            stop("no number selected, ending procedure.")
          } else{
            next
          }
        }
      }
    } else{
      i <- 1
      printIndex(index = toRestoreOpt)
      out <- toRestoreOpt
    }

  } else{
    assertCharacter(x = select, len = 1, any.missing = FALSE)
    if(select == "oldest"){
      out <- toRestoreOpt[which.min(order(toRestoreOpt))]
    } else if(select == "newest"){
      out <- toRestoreOpt[which.max(order(toRestoreOpt))]
    } else{
      out <- toRestoreOpt[grepl(pattern = select, x = toRestoreOpt)]
    }
  }

  restored <- suppressMessages(read_csv(paste0(path, "/log", "/", out)))
  outName <- strsplit(out, "_")[[1]]
  outName <- paste0(outName[-c(length(outName), length(outName)-1)], collapse = "_")
  if(!print){
    write_csv(x = restored,
              path = paste0(getOption("cT_path"), outName, ".csv"),
              na = "")
    message("'", outName, ".csv' has been restored.")
  }
}
