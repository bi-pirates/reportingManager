#' Build config.json template
#'
#' \code{auth_gdrive} generates a template config file.
#'
#'
#' @param config_path path where the config.json file is located
#' @return null
#'
#' @examples
#' \dontrun{
#' createConfigTemplate()
#' }
#' @export
createConfigTemplate <- function(config_path = ""){
  txt = '{
      "google_analytics":{
        "account_username": "_____",
        "client_id": "______.apps.googleusercontent.com",
        "client_secret":"________"
      }, "google_drive": {
        "account_username": "____",
        "client_id": "______.apps.googleusercontent.com",
        "client_secret":"_____",
        "query_folder_id":"______"
      }, { "site_catalyst": {
        "client_id": "____",
        "client_secret": "____"
      }
    }'
  if(!file.exists(paste0(config_path, "config.json"))){
    cat(txt, file = paste0(config_path, "config.json"))  
  } else {
    warning(paste0(config_path, "config.json already exists!"))
  }  
}