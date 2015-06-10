# Install
1. `install.packages("devtools"")`
2. `devtools::install_github("jlewis91/RGoogleAnalytics", ref = "patch-1")`
2. `devtools::install_github("RazorfishGermany/reportingManager")`
3. Obtain / create `config.json`
```
 {
      "google_analytics":{
        "account_username": "_____",
        "client_id": "______.apps.googleusercontent.com",
        "client_secret":"________"
 }, "google_drive": {
        "account_username": "____",
        "client_id": "______.apps.googleusercontent.com",
        "client_secret":"_____",
        "query_folder_id":"______"
  }
}
```
# Sample Code:
```
library(reportingManagr)
config_path <- "config.json" # Location of config.json file
gdrive_auth(config_path) # Create google drive credential (will be saved to token/gdrive_token)

queries <- get_gdrive_queries(config_path) # Get google drive queries (from folder specified in config)
final_data <- query_ga(queries$`Blended Ice GA`, "2015-06-01", "2015-06-01", config_path) #Get data
```

# Query Template:
- The following google doc can be copied and used to build a list of queries:
https://docs.google.com/spreadsheets/d/14fsu5SEMDo74SxKdl7IS7BMU3QheINba2pJzkEAVRzE/edit?usp=sharing
