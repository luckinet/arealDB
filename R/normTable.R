#' Normalise data tables
#'
#' Harmonise and integrate data tables into standardised format
#' @param input [\code{character(1)}]\cr path of the file to normalise. If this
#'   is left empty, all files at stage two as subset by \code{pattern} are
#'   chosen
#' @param ... [\code{list(.)}]\cr matching lists that capture the variables by
#'   which to match and the new column names containing the resulting ID; see
#'   Details.
#' @param source [\code{charcter(1)}]\cr the source from which translations of
#'   terms should be sought. By default the recent \code{"tabID"}, but when the
#'   same terms occur in several tables of a dataseries, chose \code{"datID"}.
#' @param pattern [\code{character(1)}]\cr an optional regular expression. Only
#'   dataset names which match the regular expression will be returned.
#' @param update [\code{logical(1)}]\cr whether or not the physical files should
#'   be updated (\code{TRUE}) or the function should merely return the new
#'   object (\code{FALSE}, default). This is helpful to check whether the
#'   metadata specification and the provided file(s) (translation and ID tables)
#'   are properly specified.
#' @param keepOrig [\code{logical(1)}]\cr to keep the original units and
#'   variable names in the output (\code{TRUE}) or to remove them (\code{FALSE},
#'   default). Useful for debugging.
#' @param outType [\code{logical(1)}]\cr the output file-type, currently
#'   implemented options are either \emph{*.csv} (more exchangeable for a
#'   workflow based on several programs) or \emph{*.rds} (smaller and less
#'   error-prone data-format but can only be read by R efficiently).
#' @param verbose [\code{logical(1)}]\cr be verbose about translating terms
#'   (default \code{FALSE}). Furthermore, you can use
#'   \code{\link{suppressMessages}} to make this function completely silent.
#' @details Arguments in \code{...} are so-called matching lists. This argument
#'   captures three kinds of information: \enumerate{\item the 'variable' that
#'   should be matched with a matching list, \item the 'targetColumn' in that
#'   matching list that should be included in the final table in the place of
#'   'variable' and \item the 'targetID' (column name) of that new variable.}
#'
#'   targetID = list(variable = targetColumn)
#'
#'   'variable' must be present as column in \code{input} and a table that is
#'   named "id_variable.csv" (where 'variable' is replaced by the variable name)
#'   must be available in the root directory of the project. This should have
#'   been created with \code{\link{setVariables}}.
#'
#'   To normalise data tables, this function proceeds as follows: \enumerate{
#'   \item Read in \code{input} and extract initial metadata from the file name.
#'   \item Employ the function \code{tabshiftr::\link{reorganise}} to reshape
#'   \code{input} according to the respective schema description. \item Match
#'   the territorial units in \code{input} via \code{\link{matchUnits}}. \item
#'   If \code{...} has been provided with variables to match, those are matched
#'   via \code{\link{matchVars}}. \item Harmonise territorial unit names. \item
#'   If \code{update = TRUE}, store the processed data table at stage three.}
#' @family normalisers
#' @return This function harmonises and integrates so far unprocessed data
#'   tables at stage two into stage three of the areal database. It produces for
#'   each nation in the registered data tables a comma-separated values file
#'   that includes all thematic areal data.
#' @examples
#' # build the example database
#' makeExampleDB(until = "normGeometry")
#'
#' # normalise all available data tables, harmonising commodities
#' # according to the FAO commodity list ...
#' normTable(faoID = list(commodities = "target"), update = TRUE)
#'
#' # ... and check the result
#' output <- readRDS(paste0(tempdir(), "/newDB/adb_tables/stage3/Estonia.rds"))
#' @importFrom checkmate assertNames assertFileExists assertLogical
#' @importFrom rlang exprs
#' @importFrom tabshiftr reorganise
#' @importFrom dplyr mutate select pull full_join
#' @importFrom magrittr %>%
#' @importFrom readr read_csv cols
#' @importFrom stringr str_split
#' @importFrom tidyselect everything
#' @importFrom utils read.csv
#' @export

normTable <- function(input = NULL, ..., source = "tabID", pattern = NULL,
                      update = FALSE, keepOrig = FALSE, outType = "rds",
                      verbose = FALSE){

  # set internal paths
  intPaths <- getOption(x = "adb_path")

  if(is.null(input)){
    input <- list.files(path = paste0(intPaths, "/adb_tables/stage2"), full.names = TRUE, pattern = pattern)
  } else {
    assertFileExists(x = input, access = "r")
  }

  # get objects
  inv_tables <- read_csv(paste0(intPaths, "/inv_tables.csv"), col_types = "iiiccccDccccc")
  vars <- exprs(..., .named = TRUE)

  if(update){
    if(keepOrig){
      warning("to ensure database consistency, non-standard columns are removed (keepOrig = FALSE).\n-> Set update = FALSE to keep all original columns.\n")
      keepOrig <- FALSE
    }
  }

  # make spatial subset
  spatSub <- c("nation", "un_member", "continent", "region", "subregion")
  if(any(names(vars) %in% spatSub)){
    subsets <- vars[names(vars) %in% spatSub]
    vars <- vars[!names(vars) %in% spatSub]
  } else {
    subsets <- NULL
  }

  # check validity of arguments
  assertNames(x = colnames(inv_tables), permutation.of = c("tabID", "geoID", "datID", "source_file",
                                                           "schema", "orig_file", "orig_link", "download_date",
                                                           "next_update", "update_frequency", "metadata_link",
                                                           "metadata_path", "notes"))
  assertLogical(x = update, len = 1)
  assertNames(x = outType, subset.of = c("csv", "rds"))
  assertChoice(x = source, choices = c("tabID", "datID"))
  assertList(x = vars)

  out <- NULL
  for(i in seq_along(input)){

    thisInput <- input[i]

    # scrutinize file-name (the fields, which are delimited by "_" carry important information)
    pathStr <- str_split(thisInput, "/")[[1]]
    file_name <- pathStr[length(pathStr)]
    fields <- str_split(file_name, "_")[[1]]

    if(!file_name %in% inv_tables$source_file){
      message("\n--- ", i, " / ", length(input), " skipping ", rep("-", times = getOption("width")-(nchar(i)+nchar(length(input))+4+nchar(file_name))), " ", file_name, " ---")
      next
    } else {
      message("\n--- ", i, " / ", length(input), " ", rep("-", times = getOption("width")-(nchar(i)+nchar(length(input))+13+nchar(file_name))), " ", file_name, " ---")
    }

    # get some variables
    lut <- inv_tables[grep(pattern = paste0("^", file_name, "$"), x = inv_tables$source_file),]
    if(file_name %in% lut$source_file){
      newGID <- lut$geoID
      thisSchema <- lut$schema
    } else{
      stop(paste0("  ! the file '", file_name, "' has not been registered yet."))
    }

    algorithm = readRDS(file = paste0(intPaths, "/adb_tables/meta/schemas/", thisSchema, ".rds"))
    if(!exists(x = "algorithm")){
      stop(paste0("please create the schema desciption '", algorithm, "' for the file '", file_name, "'.\n  --> See '?meta_default' for details"))
    }
    joinVars <- unlist(lapply(seq_along(algorithm@variables), function(x){
      if(algorithm@variables[[x]]$type == "id"){
        theName <- names(algorithm@variables)[[x]]
        if(!grepl(pattern = "al\\d", x = theName)){
          theName
        }
      }
    }))

    # get some variables
    tabID <- ifelse(length(inv_tables$tabID) == 0, 1,
                    inv_tables$tabID[grep(pattern = file_name, x = inv_tables$source_file)])
    geoID <- ifelse(length(inv_tables$geoID) == 0, 1,
                    inv_tables$geoID[grep(pattern = file_name, x = inv_tables$source_file)])
    joinVars <- c("tabID", "geoID", joinVars)

    # potentially subset nation values
    if(length(subsets) > 0){
      assertChoice(x = names(subsets), choices = c("nation", "un_member", "continent", "region", "subregion"))

      # unify also the nations with which to subset
      if(any(names(subsets) == "nation")){
        toUnify <- eval(subsets[[which(names(subsets) == "nation")]])
        unified <- translateTerms(terms = toUnify,
                                  index = "tt_nations",
                                  source = list("tabID" = tabID),
                                  verbose = verbose) %>%
          pull(target)
        subsets[[which(names(subsets) == "nation")]] <- unified
      }
      subNations <- countries %>%
        filter_at(vars(!!names(subsets)), any_vars(. %in% as.character(subsets[[1]]))) %>%
        pull(nation)


      if(nchar(fields[1]) != 0){
        theNation <- countries %>%
          as_tibble() %>%
          filter(nation == subNations) %>%
          select(iso_a3) %>%
          tolower()

        if(fields[1] != theNation){
          message("\n  ! the file is not part of the subset '", paste0(subNations, collapse = ", "), "'.")
          next
        }
      }

    } else {
      subNations <- NULL
    }

    # reorganise data
    message("\n--> reading new data table ...")
    thisTable <- read.csv(file = thisInput, header = FALSE, as.is = TRUE, na.strings = algorithm@format$na, encoding = "UTF-8") %>%
      as_tibble()
    message("    reorganising table with '", thisSchema, "' ...")
    temp <- thisTable %>%
      reorganise(schema = algorithm)

    # make al1 if it doesn't extist (is needed below for subsetting by nation)
    if(!"al1" %in% names(temp)){
      message("    reconstructing 'al1' ...")
      if(nchar(fields[1]) == 0){
        stop("  ! the data table '", file_name, "' seems to include several nations but the schema description doesn't contain the variable 'al1'.")
      } else {
        temp$al1 <- countries$unit[which(countries$iso_a3 == toupper(fields[1]))]
        temp <- temp %>% select(al1, everything())
      }
    }

    temp <- temp %>%
      filter_at(vars(starts_with("al")), all_vars(!is.na(.)))

    # translate nations in input
    message("    harmonising nation names ...")
    nations <- translateTerms(terms = unique(temp$al1),
                              index = "tt_nations",
                              source = list("tabID" = tabID),
                              limit = subNations,
                              verbose = verbose) %>%
      filter(!target %in% c("ignore", "missing")) %>%
      select(target, origin)

    temp <- left_join(temp, nations, by = c("al1" = "origin")) %>%
      select(-al1) %>%
      filter(!is.na(target)) %>%
      select(al1 = target, everything())
    if(!is.null(subNations)){
      temp <- temp %>%
        filter(al1 %in% subNations)

      # if there no more elements, jump to next 'input'
      if(dim(temp)[1] == 0){
        message("  ! the file is not part of the subset '", paste0(subNations, collapse = ", "), "'.")
        next
      }

      if(nchar(fields[1]) != 0){
        moveFile <- TRUE
      } else {
        moveFile <- FALSE
      }
    } else {
      moveFile <- TRUE
    }

    if(source == "tabID"){
      theSource <- list("tabID" = tabID)
    } else {
      theSource <- list("datID" = inv_tables$datID[inv_tables$tabID %in% tabID])
    }

    temp_units <- temp %>%
      mutate(tabID = tabID,
             geoID = geoID) %>%
      matchUnits(source = theSource)
    joinVars <- c("ahID", joinVars)

    # if a matching list for other variables is defined, match those
    if(length(vars) != 0){
      joinVars <- tibble(orig = joinVars) %>%
        mutate(new = if_else(orig %in% names(vars[[1]]), names(vars), orig)) %>%
        pull(new)
      message()
      temp_all <- temp_units %>%
        matchVars(source = theSource, !!!vars)
    } else {
      temp_all <- temp_units
    }

    temp <- temp_all %>%
      select(tabID, geoID, ahID, everything())

    # in case the user wants to update, update the data table
    if(length(vars) == 0) {
      outVars <- c("tabID", "geoID", "ahID", names(vars), names(algorithm@variables)[-c(grep("al", names(algorithm@variables)))])
    } else {
      varNames <- names(algorithm@variables)
      outVars <- c("tabID", "geoID", "ahID", names(vars), varNames[!varNames %in% c(grep("al", names(algorithm@variables), value = TRUE), names(vars[[1]]))])
    }
    if(update){

      theNations <- temp %>%
        filter(!is.na(ahID)) %>%
        pull(al1_name) %>%
        unique()

      for(j in seq_along(theNations)){

        tempOut <- temp %>%
          filter(al1_name == theNations[j])
        tempOut <- tempOut %>%
          select(all_of(outVars)) %>%
          filter_at(.vars = outVars, all_vars(!is.na(.)))

        # append output to previous file
        avail <- list.files(path = paste0(intPaths, "/adb_tables/stage3/"), pattern = paste0("^", theNations[j], "."))

        if(length(avail) == 1){

          if(outType == "csv"){
            prevData <- read_csv(file = paste0(intPaths, "/adb_tables/stage3/", theNations[j], ".csv"),
                                 col_types = cols(id = "i", tabID = "i", geoID = "i", ahID = "c", year = "c", .default = "d"))
          } else {
            prevData <- readRDS(file = paste0(intPaths, "/adb_tables/stage3/", theNations[j], ".rds"))
          }
          out <- tempOut %>%
            bind_rows(prevData, .) %>%
            select(-id) %>%
            distinct() %>%
            mutate(id = seq_along(year)) %>%
            select(id, everything())

        } else if(length(avail > 1)){
          # stop("the nation '", theNations[j], "' exists several times in the output folder '/adb_tablse/stage3/'.")
        } else {
          out <- tempOut %>%
            distinct() %>%
            mutate(id = seq_along(year)) %>%
            select(id, everything())
        }

        # write file to 'stage3' and move to folder 'processed'
        if(outType == "csv"){
          write_csv(x = out,
                    file = paste0(intPaths, "/adb_tables/stage3/", theNations[j], ".csv"),
                    na = "")
        } else if(outType == "rds"){
          saveRDS(object = out, file = paste0(intPaths, "/adb_tables/stage3/", theNations[j], ".rds"))
        }
      }

      if(moveFile){
        file.copy(from = thisInput, to = paste0(intPaths, "/adb_tables/stage2/processed"))
        file.remove(thisInput)
      }


    } else {

      if(!keepOrig){

        tempOut <- temp %>%
          select(all_of(outVars)) %>%
          filter_at(.vars = outVars, all_vars(!is.na(.)))

      } else {

        tempOut <- temp  %>%
          select(all_of(outVars), everything())

      }

      out <- bind_rows(out, tempOut)
    }

    gc()

  }

  if(!update){
    return(out)
  }

}
