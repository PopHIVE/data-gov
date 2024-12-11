library(httr2)
library(jsonlite)

# Search for term on Data Catalog
base_url <- "https://catalog.data.gov/api/3/action/package_search"

# Search term = "firearm"
req <- request(base_url) |> 
  # Query "firearm" with maximum 1000 rows to return
  req_url_query(q = "firearm", rows = 1000)

# Perform request and fetch response from server
resp <- req |> req_perform()
# Parse JSON
resp_json <- resp |> resp_body_json()

# Grab results of HTTP request
results <- resp_json[[3]]$results

# Grab URL for downloading CSV and download
for (result_index in length(results)) {
  resources <- results[[result_index]]$resources

  for (resources_index in length(resources)) {
      if (resources[[resources_index]]$format == "CSV") {
        download_url <- resources[[resources_index]]$url
        download.file(download_url, "TODO_name_of_dataset.csv")
      }  
  }
}  
