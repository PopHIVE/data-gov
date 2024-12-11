library(httr)
library(jsonlite)

fetch_datasets <- function(page_size = 2) {
  base_url <- "https://catalog.data.gov/api/3/action/package_search"
  page <- 1
  total_all <- list()
  
  while (TRUE) {
    res <- GET(base_url, query = list(rows = page_size, start = ((page - 1) * page_size)))
    res_content <- content(res, "text")
    res_json <- fromJSON(res_content)
    
    if (length(res_json$result$results) == 0) break
    
    total_all <- append(total_all, res_json$result$results)
    
    message(paste("Page", page, "fetched"))
    page <- page + 1
  }
  
  return(total_all)
}

datasets_metadata <- fetch_datasets()

dataset_urls <- sapply(datasets_metadata, function(x) {
  sapply(x$resources, function(res) res$url)
})


dataset_urls <- unlist(dataset_urls)



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


download_datasets(dataset_urls)