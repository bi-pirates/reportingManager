# Install
1. `install.packages("devtools")`
2. `devtools::install_github("jlewis91/RGoogleAnalytics", ref = "patch-1")`
3. `devtools::install_github("RazorfishGermany/reportingManager")`

# Initializing Package
```
library(reportingManager)
createConfigTemplate() # Generate `config.json` file template.
## Edit this file with the correct parameters.
```

# Authentication
```
## Google Drive
auth_gdrive(<ConfigPath>) # Create google drive credential (will be saved to token/gdrive_token)

## Facebook
auth_fbInsights(config_path, extended_permissions = FALSE)

## Google Analytics
### token will be generated while running "query_ga" with current the config path

## Adobe Analytics
### token will be generated while running "query_sc" with the current config path
```

# Get Queries from G-Drive
```
queries <- get_gdrive_queries(<ConfigPath>) # Get google drive queries (from folder specified in config)
```

# Get API Data
```
## Facebook
### Page Engagement
pages <- query_facebook_pages(<PageID>, <StartDate>, <EndDate>) # Dates as character "%Y-%m-%d"

### Post Engagement (need to run query_facebook_pages() before!)
#### Owned Page
posts <- query_facebook_posts() # add metrics parameter for additional metrics on post level (default = "post_impressions_unique")

#### Competitor
competitor_posts <- query_facebook_competitor_posts()

### Insights
insights <- query_facebook(<Queries>, start_date = <StartDate>, end_date = <EndDate>, override_dates = TRUE, include_query_data = TRUE)

## Google
ga_data <- query_ga(<Queries>, <StartDate>, <EndDate>, <ConfigPath>, override_dates = TRUE)

## Adobe
### Trended Reports
trended <- query_sc(<Queries>, <StartDate>, <EndDate>, override_dates = FALSE, config_path = <ConfigPath>)

### Overtime Reports
overtime <- query_sc(<Queries>, <StartDate>, <EndDate>, override_dates = FALSE, config_path = <ConfigPath>)
```

# Query Template:
- The following google docs can be copied and used to build a list of queries:
[Google Analytics](https://docs.google.com/spreadsheets/d/14fsu5SEMDo74SxKdl7IS7BMU3QheINba2pJzkEAVRzE/edit?usp=sharing)
[Adobe Analytics](https://docs.google.com/spreadsheets/d/1ojVrj14seNKtWoT1HTD49abN0qFeZHGs53IWFymchDQ/edit?usp=sharing)
[Facebook](https://docs.google.com/spreadsheets/d/1Ktt7Xl1lwQPqrtFMBqY5UvfIfVKiDJbUwi3UqGqhv9c/edit?usp=sharing)