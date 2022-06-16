#' Extract concepts in stage2 tables
#'
#' This function matches the values of a variable with an index and returns the
#' specified IDs.
#' @param variable [\code{character(.)}]\cr name of the variable for which to
#'   extract concepts.
#' @param dataseries [\code{integerish(1)}]\cr name of the dataseries from which
#'   to extract the variables' concepts.
#' @return The table provided in \code{input}, where the given variable is
#'   replaced by the column that is specified by the argument name.
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

  out <- NULL
  for(i in seq_along(sources)){

    message("    from table '", sources[i], "' ...")

    algorithm <- readRDS(file = paste0(intPaths, "/meta/schemas/", schemas[i], ".rds"))
    thisTable <- read.csv(file = paste0(intPaths, "/adb_tables/stage2/processed/", sources[i]),
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