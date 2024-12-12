path_zip_file <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/natality/Nat2023us.zip"
temp <- tempfile()

download.file(path_zip_file, "temp.zip")
system("unzip temp.zip")
