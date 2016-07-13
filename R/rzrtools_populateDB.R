library(DBI)
library(RMySQL)


populateDB <- function(dbname){
  data <- loadData()
  con <- dbConnect(RMySQL::MySQL(), dbname, username = "root")
  
  for(i in 1:length(data)){
    dbWriteTable(con, names(data)[[i]], data[[i]], overwrite=TRUE)  
  }
}
  
