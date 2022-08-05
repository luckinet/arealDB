#' Extract concepts in stage 2 tables
#'
#' Tables at stage 2 are not yet harmonised and thus there is no unified way of
#' extracting information (this is only given at stage 3). In case these
#' information are required before the tables are harmonised, this function
#' allows to extract them.
#' @param variable [\code{character(.)}]\cr name of the variable for which to
#'   extract concepts.
#' @param dataseries [\code{integerish(1)}]\cr name of the dataseries from which
#'   to extract the variables' concepts.
#' @details This function pulls the schema description of all files of the given
#'   dataseries to harmonise the table and extact the \code{variable} values.
#' @return the unique values at stage 2 of the specified dataseries
#' @importFrom checkmate assertChoice
#' @importFrom dplyr pull
#' @export

get_variable <- function(variable = NULL, dataseries = NULL){

  # set internal objects
  intPaths <- paste0(getOption(x = "adb_path"))

  # get objects
  inv_tables <- read_csv(paste0(intPaths, "/inv_tables.csv"), col_types = "iiiccccDccccc")
  inv_dataseries <- read_csv(paste0(intPaths, "/inv_dataseries.csv"), col_types = "icccccc")

  # check validity of arguments
  assertChoice(x = dataseries, choices = inv_dataseries$name)

  datID <- ifelse(length(inv_tables$datID) == 0, 1,
                  inv_dataseries$datID[grep(pattern = dataseries, x = inv_dataseries$name)])
  sources <- inv_tables$source_file[inv_tables$datID %in% datID]
  schemas <- inv_tables$schema[inv_tables$datID %in% datID]

  message("--> extracting concepts of variable '", variable,"'...")

  if(length(sources) == 0){
    stop("no tables have been defined for this dataseries.")
  }

  out <- NULL
  for(i in seq_along(sources)){

    message("    from table '", sources[i], "' ...")

    algorithm <- readRDS(file = paste0(intPaths, "/meta/schemas/", schemas[i], ".rds"))
    thisTable <- read.csv(file = paste0(intPaths, "/adb_tables/stage2/", sources[i]),
                          header = FALSE,
                          strip.white = TRUE,
                          as.is = TRUE,
                          na.strings = algorithm@format$na,
                          encoding = "UTF-8") %>%
      as_tibble()

    temp <- thisTable %>%
      reorganise(schema = algorithm)

    assertChoice(x = variable, choices = names(temp))

    concepts <- pull(temp, variable)
    out <- c(out, concepts)

  }

  out <- unique(out)
  return(out)
}