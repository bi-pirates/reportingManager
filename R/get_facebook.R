#' @import data.table
NULL

#' Authorize Facebook Insights access
#'
#' \code{auth_fbInsights} generates a token which is saved to \code{tokens/fbInsights_token} and used for accessing Facebook Insights
#'
#'
#' @param config_path path where the config.json file is located
#' @param token_path path to the token file
#' @param create_token whether a new token should be created (mandatory, if an existing token is not available in the dir tokens/)
#' @return A google drive token in the (newly created?) \code{tokens/fbInsights_token} directory.
#'
#' @examples
#' \dontrun{
#' auth_fbInsights("config.json", create_token=TRUE)
#' }
#' @export
auth_fbInsights <- function(config_path = "config.json", token_path = "tokens/fbInsights_token", create_token=TRUE){
  if(create_token){
    config_data <- jsonlite::fromJSON(config_path)

    a <- readline("Sign into the appropriate Facebook account, then hit <RETURN>.")
    scope <- "read_insights"
    myapp <- with(config_data$facebook_insights, httr::oauth_app("facebook", app_id, app_secret))
    facebook_token <- httr::oauth2.0_token(httr::oauth_endpoints("facebook"), myapp, scope = scope,
                                     type = "application/x-www-form-urlencoded")
    dir.create(path = "tokens", showWarnings = FALSE)
    save(facebook_token, file= token_path)
  }
  load(token_path)
  req <- httr::GET("https://graph.facebook.com/v2.3/me", httr::config(token = facebook_token))
  httr::stop_for_status(req)
}

query_facebook_single <- function(query, start_date, end_date
                              , config_path, token_path, override_dates){
    load(token_path)


    url <- with(query, sprintf("https://graph.facebook.com/%s/insights/%s", Page, Metric))

    # Fill in missing dates & override date field if necessary
    if(is.null(query["start.date"]) | override_dates) {
      query["start.date"] <- start_date
    }
    if(is.null(query["end.date"]) | override_dates) {
      query["end.date"] <- end_date
    }
    query["start.date"] <- as.character(max(lubridate::floor_date(as.Date(query$start.date) - lubridate::days(90), "month"), Sys.Date() - 90)) #Data only available 90 days back

    req <- httr::GET(url, httr::config(token = facebook_token), query=list(since=query$start.date, until=query$end_date, period=query$period))
    httr::stop_for_status(req)
    data <- httr::content(req)$data[[1]]
    data_points <- rbindlist(lapply(data$values, function(x) as.list(c(end_time = unlist(x$end_time), unlist(x$value)))), fill=TRUE) #date conversion should be added here
    data_points[, metric := data$title]
    data_points <- data.table(data_points)
    data_points[,date := as.character(as.Date(end_time, format = "%Y-%m-%dT"))]
    data_points[,end_time := NULL]
    data_points <- reshape2::melt(data_points, id.var = c("date", "metric"), variable.name = "interactionType")
    data_points[, query.data := jsonlite::toJSON(query)]
    return(data_points)
}


query_generic <- function(query_function, queries_table, start_date, end_date, config_path = "config.json", token_path = "tokens/fbInsights_token", override_dates = TRUE, include_query_data = FALSE) {
  results <- list()

  for (i in 1:nrow(queries_table)) {
    results[[i]] <- query_function(as.list(queries_table[i,])
                                    , start_date, end_date, config_path, token_path, override_dates)
  }
  results <- data.table::rbindlist(results, fill = TRUE)
  if(!include_query_data) {
    results[, query.data := NULL]
  }
  return(results)
}


#' Fetch clean, streamlined Facebook Insights Data, from a data.table of queries
#'
#' \code{query_ga} returns multiple Facebook Insights responses in a single data.table
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
#' query_facebook(queries_table, "2015-01-01", "2015-02-01", config_path = "config.json", override_dates = TRUE)
#' }
#' @export
query_facebook <- function(queries_table, start_date, end_date, config_path = "config.json", token_path = "tokens/fbInsights_token", override_dates = TRUE, include_query_data = FALSE) {
  query_generic(query_facebook_single, queries_table, start_date, end_date, config_path = "config.json", token_path = "tokens/fbInsights_token", override_dates = TRUE, include_query_data = FALSE)
}
