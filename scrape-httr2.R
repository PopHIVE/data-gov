library(httr2)
library(jsonlite)

# Search for term on Data Catalog
base_url <- "https://catalog.data.gov/api/3/action/package_search"

# Search term = "firearm"
req <- request(base_url) |> 
  req_url_query(q="firearm")

# Perform request and fetch response from server
resp <- req |> req_perform()
# Parse JSON
resp_json <- resp |> resp_body_json()

# Grab results
results <- resp_json[[3]]$results

# Sixth result (NCHS - VSRR Quarterly provisional estimates for selected indicators of mortality)
results[[6]]