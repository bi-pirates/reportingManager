#' Pulls data from folder of data.frame convertable json files and rbinds it into a data.table
#'
#' \code{init_queries} Pulls 
#'
#' @return data.table
#' @export
jsonFolderToDF <- function(path){
  files <- list.files(path, full.names = TRUE)
  data <- lapply(as.list(files), function(x) tryCatch(jsonlite::fromJSON(x), error = function(e) NULL))
  if("data" %in% names(data[[1]])) {
    data <- lapply(data, function(x) x$data)
  }
  data <- data.table::rbindlist(data)
  return(data)
}

#' Pulls data from folder of data.frame convertable json files and rbinds it into a data.table
#'
#' \code{init_queries} Pulls 
#'
#' @return data.table
#' @export
saveRefreshData <- function(gatherDataFunct, label, refresh = FALSE){
  path <- sprintf("data/%s.rds", label)
  if(refresh){ # load data (from file // from json)
    data <- gatherDataFunct()
    data <- unique(data)
    saveRDS(data, file = path)
  } else {
    data <- readRDS(path)
  }
  return(data)
}