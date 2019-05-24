#' Rectangrise a table
#'
#' This function takes the output from \code{\link{register}} and rearranges
#' columns and rows so that the resulting table has a perfectly rectangular
#' format.
#' @param input [\code{data.frame(1)}]\cr table to rectangularise.
#' @importFrom checkmate assertDataFrame
#' @importFrom dplyr filter_all any_vars bind_rows
#' @importFrom tidyr fill drop_na
#' @importFrom tidyselect everything
#' @importFrom magrittr %>%
#' @export

reorganise <- function(input = NULL){

  # check validity of arguments
  assertDataFrame(x = input)

  # check whether there is already a metadata object
  if(!exists(x = "meta_default", envir = baseenv())){
    stop("please first use 'register()' to specify the table properties.")
  } else{
    current <- get(x = "meta_default", envir = baseenv())
    default <- get(x = "meta_default", envir = baseenv())
  }

  # derive the full configuration for clusters
  clusters <- current$clusters
  nClusters <- max(lengths(clusters))

  # set cluster start if it is NULL
  if(is.null(clusters$top)){
    clusters$top <- 1
  }
  if(is.null(clusters$left)){
    clusters$left <- 1
  }

  ### set width and height if they are NULL
  if(is.null(clusters$width)){
    if(length(clusters$left) > 1){

    } else {
      clusters$width <- diff(c(clusters$left, dim(input)[2]+1))
    }
  }
  if(is.null(clusters$height)){
    if(length(clusters$top) > 1){
      clusters$height <- diff(c(clusters$top, dim(input)[1]+1))
    } else {
      clusters$height <- dim(input)[1] - min(clusters$top)
    }
  }
  ### make sure that all elements occur the same number of times
  clusters$top <- rep(x = clusters$top, length.out = nClusters)
  clusters$left <- rep(x = clusters$left, length.out = nClusters)
  clusters$width <- rep(x = clusters$width, length.out = nClusters)
  clusters$height <- rep(x = clusters$height, length.out = nClusters)

  # identifying variables
  idVars <- unlist(lapply(
    seq_along(current$variables), function(x){
      vals <- current$variables[[x]]
      if(vals$type == "id"){
        return(names(current$variables)[x])
      }
    }
  ))

  # values variables
  valVars <- unlist(lapply(
    seq_along(current$variables), function(x){
      vals <- current$variables[[x]]
      if(vals$type == "values"){
        return(names(current$variables)[x])
      }
    }
  ))
  # ... does it have an id?
  hasID <- unlist(lapply(
    seq_along(current$variables), function(x){
      vals <- current$variables[[x]]
      if(vals$type == "values"){
        return(!is.null(vals$id))
      }
    }
  ))

  # test whether we have a tidy table. This depends on how the different
  # variables are specified:
  # 1. all identifying variables are of 'form = "long"' and are restricted to
  # one column
  idTidy <- unlist(lapply(
    seq_along(current$variables), function(x){
      vals <- current$variables[[x]]
      if(vals$type == "id"){
        if(vals$form == "long" & length(vals$col) == 1){
          TRUE
        } else {
          FALSE
        }
      }
    }
  ))

  # 2. all values variables are restricted to one column and do have 'id = NULL'
  # so that they don't have to be split
  valsTidy <- unlist(lapply(
    seq_along(current$variables), function(x){
      vals <- current$variables[[x]]
      if(vals$type == "values"){
        if(is.null(vals$id) & length(vals$col) == 1){
          TRUE
        } else {
          FALSE
        }
      }
    }
  ))

  # determine variable names
  varNames <- c(idVars, valVars)
  varNames <- unlist(lapply(
    seq_along(current$variables), function(x){
      vals <- current$variables[[x]]
      if(!is.null(vals$name)){
        vals$name
      } else {
        varNames[x]
      }
    }
  ))

  # keys <- lapply(seq_along(keyNames), function(x){
  #   tibble(.rows = 0)
  # })

  tempValues <- list()

  # go through all clusters and process them ...
  for(i in 1:nClusters){
    data <- input[clusters$top[i]:(clusters$top[i]+clusters$height[i] - 1),
                  clusters$left[i]:(clusters$left[i]+clusters$width[i] - 1)]

    # remove rows that have NA in all columns
    data <- data %>%
      filter_all(any_vars(!is.na(.)))

    # if not all ids and all vals are tidy, rearrange the data, otherwise
    # continue filling gaps
    if(!(all(idTidy) & all(valsTidy))){

      # rearrange data

    } else {

      # fill gaps
      temp <- data %>%
        fill(everything()) %>%
        drop_na(which(varNames%in%valVars))

    }

    # append cluster to the overall output list
    tempValues <- c(tempValues, list(temp))

  }

  # row bind the values of all clusters
  out <- bind_rows(tempValues)

  # set column names
  colnames(out) <- varNames



    # cycle through all variables of interest
    # newNames <- NULL
    # oldNames <- varNames
    # for(j in seq_along(varNames)){
      # theVariable <- varNames[j]
      # var <- current$variables[[theVariable]]

      # # test whether the variable is registered at all.
      # if(all(c(is.null(var$row), is.null(var$col)))){
      #   stop(paste0("please register ", theVariable, "."))
      # }

      # # determine whether the variable is either 'key' or 'values'
      # isKey <- FALSE
      # isValues <- FALSE
      # if(all(!is.null(var$row), !is.null(current$variables$key$row))){
      #   if(var$row == current$variables$key$row){
      #     isKey <- TRUE
      #   }
      # }
      # if(all(!is.null(var$col), !is.null(current$variables$key$col))){
      #   if(var$col == current$variables$key$col){
      #     isKey <- TRUE
      #   }
      # }
      # if(all(!is.null(var$row), !is.null(current$variables$values$row))){
      #   if(var$row == current$variables$values$row){
      #     isValues <- TRUE
      #   }
      # }
      # if(all(!is.null(var$col), !is.null(current$variables$values$col))){
      #   if(var$col == current$variables$values$col){
      #     isValues <- TRUE
      #   }
      # }

      # # replace the variable name with 'value', if it's not 'values' itself
      # if(!is.null(var$value) & !isValues){
      #   oldNames[j] <- var$value
      #   theVariable <- var$value
      # }

      # # proceed with extracting this variable only if it's not already covered
      # # by 'values', which is true either when one of them is NULL, or when they
      # # both have a different value other than NULL
      # if(!is.null(var$row) & !theVariable %in% c("key", "values")){
      #   if(isValues){
      #     oldNames <- oldNames[-which(oldNames == theVariable)]
      #     if(!is.null(var$value)){
      #       newName <- setNames(object = var$value, nm = theVariable)
      #       newNames <- c(newNames, newName)
      #     }
      #     next
      #   }
      # }
      # if(!is.null(var$col) & !theVariable %in% c("key", "values")){
      #   if(isValues){
      #     oldNames <- oldNames[-which(oldNames == theVariable)]
      #     if(!is.null(var$value)){
      #       newName <- setNames(object = var$value, nm = theVariable)
      #       newNames <- c(newNames, newName)
      #     }
      #     next
      #   }
      # }

      # # get the values
      # if(is.null(var$row)){
      #   if(var$relPos){
      #     temp <- data[, var$col]
      #   } else {
      #     temp <- input[clusters$row[i]:clusters$height[i], var$col]
      #   }
      # } else if(is.null(var$col)){
      #   if(var$relPos){
      #     temp <- data[var$row, ]
      #   } else {
      #     temp <- input[var$row, clusters$col]
      #   }
      # } else{
      #   if(var$relPos){
      #     temp <- data[var$row, var$col]
      #   } else {
      #     temp <- input[var$row, var$col]
      #   }
      # }

      # # include copies of the current variable name when there is more than one
      # # output
      # if(dim(temp)[2] > 1){
      #   before <- oldNames[0:(which(oldNames == theVariable)-1)]
      #   after <- oldNames[(which(oldNames == theVariable)+1):length(oldNames)]
      #   self <- paste0(theVariable, 1:dim(temp)[2])
      #   oldNames <- c(before, self, after)
      # }
    # }

  # out <- tibble(.rows = dim(values[[1]])[1])
  # for(i in seq_along(values)){
  #   out <- bind_cols(out, values[[i]])
  # }
  # colnames(out) <- oldNames

  # if(current$body$format$type == "long"){
  #   out <- out %>%
  #     spread(key = key, value = values, convert = TRUE) %>%
  #     rename(!!newNames)
  # }

  return(out)
}