invalid <- function(x) { is.na(x) | is.null(x) | x == ""}

query_sc_single <- function(query, start_date, end_date, config_path, override_dates = TRUE) { 

 metrics_clean <- ifelse(invalid(query$metrics), "NA", strsplit(query$metrics, ","))[[1]]
 elements_clean <- ifelse(invalid(query$elements), "NA", strsplit(query$elements, ","))[[1]]
 search_clean <- ifelse(invalid(query$search), "", strsplit(query$search, ","))[[1]]

  if(override_dates){
    query$start_date <- start_date
    query$end_date <- end_date
  }
  
  query$start_date <- as.character(query$start_date)
  query$end_date <- as.character(query$end_date)
  
  if(invalid(query$date.granularity)) {
    query$date.granularity <- "day"
  }
  
  if(invalid(query$top)) {
    query$top <- 5000
  }
  
  if(invalid(query$segment.id)) {
    query$segment.id <- ""
  }
  
  if(query$queryType == "overtime") {
    data <- with(query, RSiteCatalyst::QueueOvertime(suite, start_date, end_date
                                         , metrics_clean, date.granularity = date.granularity, segment.id = segment.id))
  } else if (query$queryType == "trended") {
    data <- with(query, RSiteCatalyst::QueueTrended(suite, start_date, end_date
                                        , metrics_clean, elements_clean, search = search_clean, segment.id = segment.id
                                        , date.granularity = date.granularity, top = top))
  } else if (query$queryType == "ranked") {
    data <- with(query, RSiteCatalyst::QueueRanked(suite, start_date, end_date
                                       , metrics_clean, elements_clean, search = search_clean
                                       , segment.id = segment.id, top = top))
  }
  
  if("datetime" %in% colnames(data)){
    data$datetime <- as.Date(data$datetime)
  }
  
  data <- data.table(data)
  
  if(nrow(data) > 0) {
   if("name" %in% colnames(data) & length(query$elements) > 0) {
    setnames(data, "name", query$elements)
   }
   data[, query.name := query$queryName]
   data[, query := jsonlite::toJSON(query)]  
  }
  
  return(data)
}

query_sc_single_safe <- dplyr::failwith(data.table(), query_sc_single)

#' @export
query_sc <- function(queries_table, start_date, end_date, config_path = "config.json",
                     override_dates = TRUE, include_query_data = FALSE) {
  
  config_data <- jsonlite::fromJSON(config_path)
  sc_token <- with(config_data$site_catalyst, RSiteCatalyst::SCAuth(client_id, client_secret))
  
  results <- list()
  
  if(!"suite" %in% colnames(queries_table)){
    warning("Suite missing from scQueries sheet")  
  }
  
  for (i in 1:nrow(queries_table)) {
    results[[i]] <- query_sc_single_safe(as.list(queries_table[i,]),
                                    start_date, end_date, config_path,
                                    override_dates = override_dates)
    Sys.sleep(1)
  }
  results <- data.table::rbindlist(results, fill = TRUE)
  
  if(!include_query_data) {
    results[, query := NULL]
  }
  return(results)
}
