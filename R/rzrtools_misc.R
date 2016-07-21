#' Converts Text to Sentence Case
#'
#' \code{init_queries} Pulls 
#'
#' @return data.table
#' @export
toSentenceCase <- function(text){
  gsub("(^|[[:space:]])([[:alpha:]])"
       , "\\1\\U\\2", tolower(text)
       , perl=TRUE)
}

#' Converts percentage (numeric) to text
#' @export
fm_pct <- function(num){paste0(round(num, 2), "%")}

#' Execute Data Table Query
#' @export
executeDataTableQuery <- function(query){eval(parse(text = query))}