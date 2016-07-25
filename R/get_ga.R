#' @import data.table
NULL

#' @import googleAuthR
#' @import googleAnalyticsR

NULL

query_ga_single <- function(query, start_date, end_date, override_dates = TRUE){
  ga_token <- googleAuthR:::gar_auth()
  
  # Save query.name for diagnostic info
  query.name <- query$query.name
  queryInfo <- buildQuery(query, start_date, end_date, TRUE)
  query <- queryInfo$query
  
  query_response <- google_analytics(id = param_clean(query$table.id), start = query$start.date, 
                      end = query$end.date, metrics = param_clean(query$metrics),
                      dimensions = param_clean(query$dimensions), sort = query$sort, 
                      filters = query$filters, segment = query$segment,
                      samplingLevel = query$sampling,
                      max_results = query$max.results, multi_account_batching = FALSE, type = query$source)
  
  message("Data returned\n")
  query_response <- data.table::data.table(query_response)
  query_response <- melt_ga(query_response, queryInfo)
  message("Data reshaped\n")
  return(query_response)
}

query_ga_range <- function(query){
  ga_token <- googleAuthR:::gar_auth()
  
  query$date.range <- unlist(strsplit(query$date.range,","))
  
  # Save query.name for diagnostic info
  query.name <- query$query.name
  queryInfo <- buildQuery(query, override_dates = FALSE)
  query <- queryInfo$query
  
  query_response <- google_analytics_4(viewId = param_clean(query$table.id), date_range = query$date.range, 
                                       metrics = param_clean(query$metrics), dimensions = param_clean(query$dimensions),
                                       dim_filters = NULL, met_filters = NULL,
                                       filtersExpression = query$filters, order = order_type(param_clean(query$metrics[1]), "DESCENDING", "DELTA"), segments = query$segment,
                                       pivots = NULL, cohorts = NULL, max = query$max.results,
                                       samplingLevel = query$sampling, metricFormat = NULL,
                                       histogramBuckets = NULL)

  message("Data returned\n")
  query_response <- data.table::data.table(query_response)
  queryInfo$query$metrics <- c(paste0(queryInfo$query$metrics,".d1"), paste0(queryInfo$query$metrics,".d2"))
  query_response <- melt_ga(query_response, queryInfo)
  message("Data reshaped\n")
  return(query_response)
}

fill_date_metrics <- function(dimension, implied_dimension, query_dimensions) {
  # one dimension implies
  if(grepl(dimension, query_dimensions) & !grepl(implied_dimension, query_dimensions)) {
    query_dimensions <- paste(query_dimensions, ",ga:", implied_dimension, sep = "")
  }
  return(query_dimensions)
}

buildQuery <- function(query, start_date, end_date, override_dates){
if(!is.list(query)) {
  warning(sprintf("Class of object `query` is: %s, should be list!", class(query)))
}

# Fill in missing dates & override date field if necessary
if(is.null(query["start.date"]) | override_dates) {
  query["start.date"] <- start_date
}
if(is.null(query["end.date"]) | override_dates) {
  query["end.date"] <- end_date
}
# Remove NA / Irrelevant Fields
fields <- c("start.date", "end.date", "dimensions", "metrics"
            , "table.id", "sort", "filters", "segment", "max.results", "sampling", "source", "date.range")
labels <- query[!names(query) %in% fields]
query <- query[fields]
if(is.na(query$max.results)){
  query$max.results <- 1000
}else{
  query$max.results <- as.numeric(query$max.results) 
}
query <- query[!is.na(query)]
# Cleanup Date Metrics
date_dimensions <- data.table::data.table(dimension = c("month", "week", "day", "hour"), implied_dimension = c("year", "year", "date", "date"))

for( i in 1:nrow(date_dimensions) ) {
  query$dimensions <- fill_date_metrics(date_dimensions$dimension[i]
                                        , date_dimensions$implied_dimension[i], query$dimensions)
}


return(list(query = query, labels = labels))
}


#' Fetch clean, streamlined Google Analytics Data, from a data.table of queries
#'
#' \code{query_ga} returns multiple Google Analytics API responses in a single data.table
#'
#'
#' @param queries_table data.table of queries
#' @param start_date start date as character string in \code{"\%Y-\%m-\%d"} format
#' @param end_date end date as character string in \code{"\%Y-\%m-\%d"} format
#' @param config_path path where the config.json file is located
#' @param override_dates whether all dates should be overriden with those in \code{start_date} & \code{end_date}
#' @param include_query_data whether the full query data should be included in a column as a json object
#' @return A data.table filled with great, interesting googleAnalytics data!!
#'
#' @examples
#' \dontrun{
#' query_ga(queries_table, "2015-01-01", "2015-02-01", config_path = "config.json", override_dates = TRUE)
#' }
#' @export
query_ga <- function(queries_table, start_date, end_date, override_dates = TRUE, include_query_data = FALSE) {
  results <- list()

  for (i in 1:nrow(queries_table)) {
    if(is.na(queries_table[i,]$date.range)){
      results[[i]] <- query_ga_single(as.list(queries_table[i,])
                                      , start_date, end_date, override_dates = override_dates)  
    }else{
      results[[i]] <- query_ga_range(as.list(queries_table[i,]))
    } 
  }
  results <- data.table::rbindlist(results, fill = TRUE)
  if(!include_query_data) {
    results[, query := NULL]
  }
  return(results)
}



param_clean <- function(param) {sort(unlist(strsplit(gsub("ga:", "", param), ",", fixed=TRUE)))}

melt_ga <- function(data, queryInfo){
  data <- data.table::copy(data)
  time_frames <- c("year", "month", "week", "date", "hour")
  time_frames <- c("lifetime", time_frames[time_frames %in% colnames(data)])
  time_frames <- tail(time_frames, 1)
  data[, resolution := time_frames]
  data <- add_in_ga_date(data)
  data <- reshape2::melt(data, measure.vars = param_clean(queryInfo$query$metrics))
  if(!is.null(queryInfo$query$segment)) {
    data[, segment := queryInfo$query$segment]
  }
  data[, query.name := queryInfo$labels$query.name]
  data[, query.data := jsonlite::toJSON(queryInfo)]
  return(data)
}



add_in_ga_date<- function(data) {
  if(all(data$resolution == "lifetime")){
    data[, date := ""]
  } else if (all(data$resolution == "year")){
    data[, date := as.Date(paste(year, "01-01", sep="-"), format="%Y-%m-%d")]
  } else if(all(data$resolution == "month")){
    data[, date := as.Date(paste(year, month, "01", sep="-"), format="%Y-%m-%d")]
  } else if(all(data$resolution == "week")){
    data[, date := as.Date(paste(year, week, "01", sep="-"), format="%Y-%U-%w")]
  } else if(all(data$resolution == "date")){
    data[, date := as.Date(date, format="%Y%m%d")]
  } else if(all(data$resolution == "hour")){
    data[, date := as.Date(paste(date, hour, sep="-"), format="%Y%m%d-%H")]
  }

    data[, date := as.character(date)]  
  
  return(data)
}
