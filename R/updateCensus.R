#' Update the census data
#'
#' @param census [\code{tibble(1)}]\cr the table that contains the standardised
#'   census data.
#' @param nations [\code{character(1)}]\cr the nation name, which is also the
#'   new file name.
#' @param file [\code{character(1)}]\cr full path of the stage1 file containing
#'   census stats.
#' @importFrom checkmate assertTibble assertCharacter
#' @importFrom readr read_csv write_csv cols col_number
#' @importFrom dplyr full_join arrange
#' @export

updateCensus <- function(census = NULL, nations = NULL, file = NULL){

  # check validity of arguments
  assertTibble(x = census)
  assertCharacter(x = nations)
  assertFileExists(x = file, access = "rw")

  if(length(nations) > 1){
    census <- census %>%
      left_join(countries[c("nation", "ahID")], by = "ahID")
  }

  # get some paths
  targetDir <- paste0(getOption(x = "dmt_path"), "/census/stage2/")
  archive <- paste0(getOption(x = "dmt_path"), "/census/stage1/processed")

  for(i in seq_along(nations)){

    message("\n--> Updating census of '", nations[i], "'.")

    tempCensus <- census %>%
      filter(nation == nations[i]) %>%
      select(-nation)

    # append output to previous file
    if(file.exists(paste0(targetDir, nations[i], ".csv"))){
      tempData <- read_csv(file = paste0(targetDir, "/", nations[i], ".csv"),
                           col_types = cols(headcount = col_number(),
                                            planted_area = col_number(),
                                            harvested_area = col_number(),
                                            production = col_number(),
                                            yield = col_number()))
      out <- full_join(tempData, tempCensus) %>%
        arrange(id) %>%
        mutate(id = seq_along(id))
    } else{
      out <- tempCensus %>%
        mutate(id = seq_along(id))
    }

    # write file to 'stage2' and move to folder 'processed'
    write_csv(x = out, path = paste0(targetDir, "/", nations[i], ".csv"), na = "")
  }

  # then move the original file to the archive
  file.copy(from = file, to = archive)
  file.remove(file)

  # push to sql-database


}

