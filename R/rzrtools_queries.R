#' @import data.table
NULL

# jsonlite
# RMySQL
# data.table

#' Creates a queries folder as well as a template config.json doc
#'
#' \code{init_queries} Sets up directory for reading sql queries out of a queries directory
#'
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' init_queries()
#' }
#' @export
init_queries <- function() {
  dir.create("queries", showWarnings = FALSE)
  if(!file.exists("config.json")) {
    config <- '{sql_config: {
    "engine":"mysql"
    , "db_name": "**db_name"
    , "host": "**host"
    , "user": "**username"
    , "pwd": "**password"
  }
}
    '
    cat(config, file = "config.json")
    } else {
      message("File config.json found and not replaced.")
    }
  }

getQuery <- function(query_path) {
  query <- readLines(query_path, warn = FALSE)
  query <- query[!grepl("^--", query)]
  query <- gsub("^[[:space:]]+", "", query)
  query <- paste(query, collapse = " ")
  return(query)
}

#' Creates a list object with labeled sql queries
#'
#' \code{getQueries} Creates a list object with labeled sql queries
#'
#'
#' @return list(Query1 = "SELECT * FROM abc")
#'
#' @examples
#' \dontrun{
#' queries <- getQueries()
#' getDBdata(queries)
#' }
#' @export
getQueries <- function(path = "", queries_folder = "queries", query_filter = "all", package = NULL){
  if(path != "" & !is.null(package)) warning("Only one of path and package can be used.")
  if (!is.null(package)) {
    path <- system.file(package = package)  
  }

  if(path != "") {
    queries_path <- paste(path, queries_folder, sep = "/")  
  } else ( queries_path <- queries_folder )
  
  queries <- as.list(list.files(queries_path, full.names = TRUE, recursive = TRUE))
  queries <- queries[grepl(".sql", queries)]
  names(queries) <- gsub("(.*\\/)?(.*)\\.sql", "\\2", unlist(queries))
  queries <- as.list(sapply(queries, getQuery, USE.NAMES = TRUE))
  if (query_filter == "active") {
    if (!is.null(package)) {
      path <- system.file(package = package)
    } else {path <- "" }
    act_queries <- active_queries(path)
    queries <- queries[names(queries) %in% act_queries]
  }
  return(queries)
}

#' Get Active Queries from a project.
#' @export
active_queries <- function(path = ""){
  active_queries <- function(file){
    code <- paste(readLines(file), collapse = " ")
    code <- stringr::str_match_all(code, "db\\$([a-z_]+)")
    return(data.frame(code, stringsAsFactors = FALSE)[,2])
  }
  if(path == ""){
    files <- list.files(full.names = TRUE)
  } else {
    files <- list.files(path, full.names = TRUE)
  }
  
  data <- unique(unlist(sapply(as.list(files[grepl(".Rmd|.R", files)]), active_queries)))
  return(data)
}

#' Queries data base using MySQL config file & queries list object
#'
#' \code{getDBdata} Queries data base using MySQL config file & queries list object
#'
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' queries <- getQueries()
#' getDBdata(queries)
#' }
#' @export
getDBdata <- function(queries, config_path = "config.json"){
  config_data <- jsonlite::fromJSON(config_path)
  if(config_data$sql_config$engine == "mysql") {
    con <- with(config_data$sql_config, DBI::dbConnect(RMySQL::MySQL(), dbname = db_name
                                                       , host = host, user = user, password = pwd))
    DBI::dbGetQuery(con, "SET NAMES utf8")
  } else {
    warning("RDBMS besides mysql not supported at this time.")
  }
  rs <- lapply(queries, function(x) data.table::data.table(DBI::dbGetQuery(con, x)))
  DBI::dbDisconnect(con)
  return(rs)
}
