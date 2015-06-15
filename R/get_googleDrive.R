#' @import data.table
NULL

#' Authorize Google Drive access
#'
#' \code{auth_gdrive} generates a token which is saved to \code{tokens/gdrive_token} and used for accessing a google drive folder
#'
#'
#' @param config_path path where the config.json file is located
#' @param token_path path to the token file
#' @param create_token whether a new token should be created (mandatory, if an existing token is not available in the dir tokens/)
#' @return A google drive token in the (newly created?) \code{tokens/gdrive_token} directory.
#'
#' @examples
#' \dontrun{
#' auth_gdrive("config.json", create_token=TRUE)
#' }
#' @export
auth_gdrive <- function(config_path = "config.json", token_path = "tokens/gdrive_token", create_token=TRUE){
  if(create_token){
    config_data <- jsonlite::fromJSON(config_path)
    a <- readline(sprintf("Sign into %s@gmail.com google account, then hit <RETURN>."
                     , config_data$google_drive$account_username))
    scope <- c("https://www.googleapis.com/auth/drive.readonly"
               , "https://www.googleapis.com/auth/userinfo.profile")
    myapp <- with(config_data$google_drive, httr::oauth_app("gdrive", client_id, client_secret))
    gdrive_token <- httr::oauth2.0_token(httr::oauth_endpoints("google"), myapp, scope = scope)
    dir.create(path = "tokens", showWarnings = FALSE)
    save(gdrive_token, file = token_path)
  }
  load(token_path)
  req <- httr::GET("https://www.googleapis.com/oauth2/v1/userinfo",
             httr::config(token = gdrive_token))
  httr::stop_for_status(req)
  return(gdrive_token)
}

get_query_folder_contents <- function(config_gdrive){
  load("tokens/gdrive_token")
  url <- with(config_gdrive, sprintf("https://www.googleapis.com/drive/v2/files/%s/children", query_folder_id))
  req <- httr::GET(url, httr::config(token = gdrive_token))
  httr::stop_for_status(req)
  items <- lapply(httr::content(req)$items, function(x) x$id)
  return(items)
}

get_download_link <- function(file_id){ #returns file names & download links
  load("tokens/gdrive_token")
  url <- sprintf("https://www.googleapis.com/drive/v2/files/%s", file_id)
  req <- httr::GET(url, httr::config(token = gdrive_token))
  httr::stop_for_status(req)
  req <- httr::content(req)
  link <- as.list(req$exportLinks$`text/csv`)
  names(link) <- req$title
  return(link)
}

get_sheet <- function(link){
  load("tokens/gdrive_token")
  req <- httr::GET(link, httr::config(token = gdrive_token))
  httr::stop_for_status(req)
  req <- data.table::data.table(httr::content(req))
  req[req == ""] <- NA
  return(req)
}

#' Get Google Drive queries
#'
#' \code{get_gdrive_queries} returns a list of \code{data.table}s which have the query information required to run google analytics queries
#'
#' @param config_path path where the config.json file is located
#' @return list of \code{data.table}s which have the query information required to run google analytics queries
#'
#' @examples
#' \dontrun{
#' get_gdrive_queries("config.json")
#' }
#' @export
get_gdrive_queries <- function(config_path){
  config_data <- jsonlite::fromJSON(config_path)
  files <- get_query_folder_contents(config_data$google_drive)
  links <- sapply(files, get_download_link, USE.NAMES=TRUE)
  query_data <- sapply(links, get_sheet, USE.NAMES=TRUE)
  return(query_data)
}
