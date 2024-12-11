## ----------------------------------------------------------------
## Directions copied from Dr. Dan Weinberger's email 12/6/2024


## ----------------------------------------------------------------
## Step #1. Access Data.gov API

## Data.gov provides an API for accessing its metadata. This 
## metadata includes information about the datasets available on 
## Data.gov, including URLs to access these datasets.


## ----------------------------------------------------------------
## Step #2. Fetch Dataset Metadata

## You can query the Data.gov API to fetch metadata about datasets. 
## Here is an example process using R to get started:

# Install and load required R packages.
install.packages("httr")
install.packages("jsonlite")
library(httr)
library(jsonlite)


# Define a function to fetch datasets metadata.
fetch_datasets <- function(page_size = 1000) {
  base_url <- "https://catalog.data.gov/api/3/action/package_search"
  page <- 1
  total_all <- list()
  
  while (TRUE) {
    res <- GET(base_url, query = list(rows = page_size, 
                                      start = ((page - 1) * page_size)))
    res_content <- content(res, "text")
    res_json <- fromJSON(res_content)
    
    if (length(res_json$result$results) == 0) break
    
    total_all <- append(total_all, res_json$result$results)
    
    message(paste("Page", page, "fetched"))
    page <- page + 1
  }
  
  return(total_all)
}


# Fetch the datasets' metadata.
datasets_metadata <- fetch_datasets()



## ----------------------------------------------------------------
## Step #3. Download Datasets

## Once you have the metadata, you can extract the URLs of individual 
## datasets and download them.


# Extract resources from metadata.
dataset_urls <- sapply(datasets_metadata, function(x) {
  sapply(x$resources, function(res) res$url)
})
dataset_urls <- unlist(dataset_urls)


# Create a function to download datasets.
download_datasets <- function(urls, download_path="datasets/") {
  dir.create(download_path, showWarnings = FALSE)
  
  for (url in urls) {
    download_file <- paste0(download_path, basename(url))
    
    tryCatch({
      download.file(url, destfile = download_file, method="auto")
      message(paste0("Downloaded: ", url))
    }, error = function(e) {
      message(paste0("Failed to download: ", url))
    })
  }
}


# Download the datasets (be aware of the data size and your network 
# limitations).
download_datasets(dataset_urls)


## ----------------------------------------------------------------
## Notes:
  
## Data Volume: Be cautious, as the volume of data can be massive. Ensure 
## you have adequate storage and bandwidth.

## Automation Tools: Consider using tools like wget or curl if you plan to 
## handle huge amounts of data outside of R.

## Incremental Downloads: Establish incremental or paginated fetching to 
## manage resources effectively and avoid server throttle limits. Improved 
## code considerations:
  

## Handle HTTP request limitations:
## 
## Manage large volumes and ensure the download process is robust and 
## resilient to failures. Consider saving metadata for later references 
## to avoid duplicate downloads or handle errors gracefully.


## ----------------------------------------------------------------
## Conclusion:
  
## This approach outlines a way to begin programmatically fetching 
## and downloading datasets from Data.gov using APIs. Customize and 
## enhance the script as per resource constraints and specific requirements. 
## For thorough details and up-to-date API documentation, refer to

## https://data.gov/user-guide/


