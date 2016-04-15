# Install
1. `install.packages("devtools")`
2. `devtools::install_github("jlewis91/RGoogleAnalytics", ref = "patch-1")`
2. `devtools::install_github("RazorfishGermany/reportingManager")`

# Sample Code:
```
library(reportingManager)
createConfigTemplate() # Generate `config.json` file template.
## Edit this file with the correct parameters.
auth_gdrive("config.json") # Create google drive credential (will be saved to token/gdrive_token)

queries <- get_gdrive_queries(config_path) # Get google drive queries (from folder specified in config)
googleData <- query_ga(queries$GoogleAnalytics, "2015-06-01", "2015-06-01", config_path) #Get data

facebookData <- query_facebook(queries$facebook_queries, "2015-06-05", "2015-06-08")

```

# Query Template:
- The following google docs can be copied and used to build a list of queries:
[Google Analytics](https://docs.google.com/spreadsheets/d/14fsu5SEMDo74SxKdl7IS7BMU3QheINba2pJzkEAVRzE/edit?usp=sharing)
[Facebook]
