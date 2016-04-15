library(RSiteCatalyst)


getData <- function (queryType, accountObj, dateFrom, dateTo, metrics, elements = ""
                     , segment.id = "", search = c(), top = 5000, auth = FALSE, date.granularity = date.granularity) { # execute a single sc request, streamlined so that appropriate queue form is called
  if(auth) {
    RSiteCatalyst::SCAuth(accountObj$Username, accountObj$Secret)
  }
  
  
  metrics <- ifelse(is.na(metrics), "NA", strsplit(metrics, ",")[[1]])
  elements <- ifelse(is.na(elements), "NA", strsplit(elements, ",")[[1]])
  
  if(is.na(top)) {
    top <- 5000
  }
  if(is.na(search)) {
    search <- c()
  }
  if(queryType == "overtime"){
    data <- RSiteCatalyst::QueueOvertime(accountObj$Suite, as.character(dateFrom), as.character(dateTo)
                                         , metrics, date.granularity, segment.id = segment.id)
  } else if (queryType == "trended") {
    data <- RSiteCatalyst::QueueTrended(accountObj$Suite, as.character(dateFrom), as.character(dateTo)
                                        , metrics, elements, search = search, segment.id = segment.id
                                        , date.granularity, top = top)
  } else if (queryType == "ranked") {
    data <- RSiteCatalyst::QueueRanked(accountObj$Suite, as.character(dateFrom), as.character(dateTo)
                                       , metrics, elements, search = search, segment.id = segment.id, top = top)
  }
  
  data$label <- accountObj$label
  if("datetime" %in% colnames(data)){
    data$datetime <- as.Date(data$datetime)
  }
  
  if("name" %in% colnames(data)){
    setnames(data, "name", elements)
  }
  
  
  data <- data.table(data)
  return(data)
}

#' @export
fetchSC <- function(scQueries, accountObj, start_date, end_date, date.granularity = "day") { #make a series of site catalyst data requests
  RSiteCatalyst::SCAuth(accountObj$Username, accountObj$Secret)  
  
  data <- list()
  
  for (i in 1:nrow(scQueries) ) {
    data[[i]] <- with(scQueries[i], getData(queryType, accountObj
                                            , start_date, end_date, metrics, elements
                                            , top = top, search = search, date.granularity = date.granularity))
    Sys.sleep(1)
  }
  
  names(data) <- scQueries$queryName
  return(data)
}
