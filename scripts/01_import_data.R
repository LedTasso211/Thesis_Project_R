source("scripts/00_setup.R")

# Get all folders under in data folder
year_folders <- list.dirs("data", recursive = FALSE, full.names = TRUE)


all_data <- list()

# Loop through each year folder
for (folder in year_folders) {
  year <- gsub(" DB", "", basename(folder))  # Extract just the year
  
  # List all files in that year's folder
  sav_files <- list.files(folder, pattern = "\\.sav$", full.names = TRUE)
  
  # Read and name all datasets in this folder
  year_data <- lapply(sav_files, haven::read_sav)
  names(year_data) <- tools::file_path_sans_ext(basename(sav_files))
  
  # Store in the all_data list under the corresponding year
  all_data[[year]] <- year_data
}

rm(year_data, folder, sav_files, year, year_folders)


# Load CPI data for Georgia from World Bank Datasets 
cpi_data <- WDI(
  country = "GE",             
  indicator = "FP.CPI.TOTL",  # CPI indicator code
  start = 2004,
  end = 2024
)