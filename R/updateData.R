#' Update the database
#'
#' Store a harmonised and reshaped table into the database.
#' @param table [\code{tibble(1)}]\cr the object that contains the standardised
#'   data table.
#' @param nations [\code{character(1)}]\cr the nation name, which is also the
#'   new file name.
#' @param file [\code{character(1)}]\cr full path of the stage2 file containing
#'   stats.
#' @importFrom checkmate assertTibble assertCharacter
#' @importFrom readr read_csv write_csv cols col_number
#' @importFrom dplyr full_join arrange
#' @export

updateData <- function(table = NULL, nations = NULL, file = NULL){

  # check validity of arguments
  assertTibble(x = table)
  assertCharacter(x = nations)
  assertFileExists(x = file, access = "rw")

  # if(length(nations) > 1){
  #   table <- table %>%
  #     left_join(countries[c("nation", "ahID")], by = "ahID")
  # }

  # get some paths
  targetDir <- paste0(getOption(x = "adb_path"), "/adb_tables/stage3/")
  archive <- paste0(getOption(x = "adb_path"), "/adb_tables/stage2/processed")

  message("\n--> Updating tables.")

  for(i in seq_along(nations)){

    # message("--> Updating table of '", nations[i], "'.")

    tempTable <- table %>%
      filter(al1_name == nations[i]) %>%
      select(-starts_with("al"))

    # append output to previous file
    if(file.exists(paste0(targetDir, nations[i], ".csv"))){
      tempData <- read_csv(file = paste0(targetDir, "/", nations[i], ".csv"), col_types = "dddcddd")
      out <- full_join(tempData, tempTable) %>%
        arrange(id) %>%
        mutate(id = seq_along(id))
    } else{
      out <- tempTable %>%
        mutate(id = seq_along(id))
    }

    # write file to 'stage3' and move to folder 'processed'
    write_csv(x = out, path = paste0(targetDir, "/", nations[i], ".csv"), na = "")
  }

  # then move the original file to the archive
  file.copy(from = file, to = archive)
  file.remove(file)

  # push to sql-database

}

