#' @import devtools
#' @import data.table
#' @import SocialMediaMineR
#' @import lubridate
#' @import magrittr
devtools::install_github("pablobarbera/Rfacebook" , subdir="Rfacebook")
#' @import Rfacebook

NULL

#' Authorize Facebook Insights access
#'
#' \code{auth_fbInsights} generates a token which is saved to \code{tokens/fbInsights_token} and used for accessing Facebook Insights
#'
#'
#' @param config_path path where the config.json file is located
#' @param token_path path to the token file
#' @param create_token whether a new token should be loadcreated (mandatory, if an existing token is not available in the dir tokens/)
#' @return A google drive token in the (newly created?) \code{tokens/fbInsights_token} directory.
#'
#' @examples
#' \dontrun{
#' auth_fbInsights("config.json", create_token=TRUE)
#' }
#' @export

auth_fbInsights <- function(config_path = "config.json", token_path = "tokens/fbInsights_token", create_token=TRUE, extended_permissions = TRUE ){
  if(create_token){
    config_data <- jsonlite::fromJSON(config_path)

    a <- readline("Sign into the appropriate Facebook account, then hit <RETURN>.")
    scope <- "read_insights"
    myapp <- with(config_data$facebook_insights, httr::oauth_app("facebook", app_id, app_secret))
    #facebook_token <- httr::oauth2.0_token(httr::oauth_endpoints("facebook"), myapp$key, scope = scope,
    #                                 type = "application/x-www-form-urlencoded")
    facebook_token <- fbOAuth(app_id = myapp$key, app_secret = myapp$secret, extended_permissions = extended_permissions)
    dir.create(path = "tokens", showWarnings = FALSE)
    save(facebook_token, file = token_path)
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
    #query["start.date"] <- as.character(max(lubridate::floor_date(as.Date(query$start.date) - lubridate::days(90), "month"), Sys.Date() - 90)) #Data only available 90 days back
    data <- as.data.table(getInsights(object_id = query$Page, token = facebook_token, metric = query$Metric, period = query$period, parms = paste0("&since=",start_date,"&until=",end_date)))

    # req <- httr::GET(url, httr::config(token = facebook_token), query=list(since=query$start.date, until=query$end_date, period=query$period))
    # httr::stop_for_status(req)
    # data <- httr::content(req)$data[[1]]
    #data_points <- rbindlist(lapply(data$values, function(x) as.list(c(end_time = unlist(x$end_time), unlist(x$value)))), fill=TRUE) #date conversion should be added here

    data_points <- data[, .(end_time, value)]
    data_points[, metric := data$title]
    # data_points <- data.table(data_points)
    data_points[, date := as.character(as.Date(end_time, format = "%Y-%m-%dT"))]
    data_points[, end_time := NULL]
    data_points <- reshape2::melt(data_points, id.var = c("date", "metric"), variable.name = "interactionType")
    data_points[, page.name := query["Page"]]
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
query_generic(query_facebook_single, queries_table, start_date, end_date, config_path = "config.json", token_path = "tokens/fbInsights_token", override_dates = TRUE, include_query_data = include_query_data)
}

#' \code{query_facebook_pages} returns Facebook Page Data
#' @param facebookPages list of Facebook Page IDs
#' @param start_date start date as character string in \code{"\%Y\/\%m\/\%d"} format
#' @param end_date end date as character string in \code{"\%Y\/\%m\/\%d"} format
#' @param token path where the config.json file is located
#' @return A data.table filled with great, interesting facebook page date!!
#'
#' @examples
#' \dontrun{
#' query_facebook_pages("AudiDE", "2016/01/01", "2016/05/01")
#' }
#' @export
query_facebook_pages <- function(facebookPages, start_date, end_date, token_path = "tokens/fbInsights_token"){

  pages <- list()
  likes <- list()
  load(token_path)

  for(i in 1:length(facebookPages)){
    pages[[i]] <- getPage(page = facebookPages[i], token = facebook_token, since = start_date, until = end_date, n = 1e4)
    likes[[i]] <- as.data.table(SocialMediaMineR::get_facebook(paste0("https://www.facebook.com/",facebookPages[i])))
    pages[[i]] <- cbind(pages[[i]], "likes_total" = likes[[i]]$like_count)
  }
  saveRDS(pages, file = "pages_rawData.rds")

  pages <- as.data.table(do.call(rbind, pages))
  pages[, date := as.POSIXct(created_time)]
  pages[, month := lubridate::month(date, label = TRUE)]
  pages[, quarter := lubridate::quarter(date, with_year = TRUE) %>% as.character]
  pages[, year := lubridate::year(date) %>% as.character]
  pages[, totalEngagement := (likes_count + comments_count + shares_count), by = id]
  pages[, averageER := sum(totalEngagement)/likes_total, by = .(from_name, month)]

  return(pages)
}

#' \code{query_facebook_posts} returns Facebook Post Data - query_facebook_pages needed to run before!
#' @param token path where the config.json file is located
#' @return A data.table filled with great, interesting facebook post data!
#'
#' @examples
#' \dontrun{
#' query_facebook_posts(metrics = c("post_impressions_organic","post_impressions_paid"))
#' }
#' @export
query_facebook_posts <- function(token_path = "tokens/fbInsights_token", metrics = c("post_impressions_unique") ){

  pages <- readRDS("pages_rawData.rds")

  posts <- list()
  postData <- list()
  insights <- list()
  load(token_path)

  if(!"post_impressions_unique" %in% metrics){
    metrics <- c(metrics, "post_impressions_unique")
  }

  # metrics = queries_all_post
  # x <- 1
  # i <- 1
  for(x in 1:length(pages)){
    for(i in 1:nrow(pages[[x]])){
      posts[[i]] <- getPost(post = pages[[x]]$id[i], token = facebook_token, n = 5)
      insights[[i]] <- getInsights(pages[[x]]$id[i], token = facebook_token, metrics, period = "lifetime")
      insights <- as.data.table(do.call(bind_rows, insights[[i]]))
      insights <- insights[is.na(variable), variable := name][,.(variable, value)]
      insights <- dcast(insights, .~ variable)
      insights$. <- NULL
      posts[[i]] <- cbind(posts[[i]]$post, insights)
      print(paste0(i," von ", nrow(pages[[x]])))
      insights <- list()
    }
    if(length(postData) == 0){
      postData <- c(posts)
      insights <- list()
    }else{
      postData <- c(postData, posts)
      posts <- list()
      insights <- list()
    }
  }

  postData <- as.data.table(do.call(bind_rows, postData))
  postData[, date := as.POSIXct(created_time)]
  postData[, month := lubridate::month(date, label = TRUE)]
  postData[, quarter := lubridate::quarter(date, with_year = TRUE) %>% as.character]
  postData[, year := lubridate::year(date) %>% as.character]
  postData[, totalEngagement := (likes_count + comments_count + shares_count), by = id]
  postData[, RER := totalEngagement/post_impressions_unique, by = id]
  postData[RER == "Inf", RER := 0]
  postData[, averageRER := mean(RER, na.rm = TRUE), by = .(from_name, month)]

  return(postData)
}

#' \code{query_facebook_competitor_posts} returns Facebook Post Data from not owned pages - query_facebook_pages needed to run before!
#' @param token path where the config.json file is located
#' @return A data.table filled with great, interesting facebook post data!
#'
#' @examples
#' \dontrun{
#' query_facebook_competitor_posts()
#' }
#' @export
query_facebook_competitor_posts <- function(token_path = "tokens/fbInsights_token"){

  pages <- readRDS("pages_rawData.rds")

  posts <- list()
  postData <- list()
  likes <- list()
  load(token_path)

  for(x in 1:length(pages)){
    for(i in 1:nrow(pages[[x]])){
      posts[[i]] <- getPost(post = pages[[x]]$id[i], token = facebook_token, n = 5)
      likes[[i]] <- pages[[x]]$likes_total[i]
      posts[[i]] <- cbind(posts[[i]]$post, "likes_total" = likes[[i]])
      print(paste0(i," von ", nrow(pages[[x]])))
    }
    if(length(postData) == 0){
      postData <- c(posts)
    }else{
      postData <- c(postData, posts)
      posts <- list()
    }
  }

  postData <- as.data.table(do.call(rbind, postData))
  postData[, date := as.POSIXct(created_time)]
  postData[, month := lubridate::month(date, label = TRUE)]
  postData[, quarter := lubridate::quarter(date, with_year = TRUE) %>% as.character]
  postData[, year := lubridate::year(date) %>% as.character]
  postData[, totalEngagement := (likes_count + comments_count + shares_count), by = id]
  postData[, ER := totalEngagement/likes_total, by = id]
  postData[ER == "Inf", ER := 0]

  return(postData)
}
