# Importing libraries
library(tidyverse)
library(haven)
library(WDI)
library(readxl)
library(Hmisc)
library(scales)
library(labelled)
library(survey)
library(rifreg)
library(openxlsx)
library(parallel)
library(broom)

save_results_to_excel <- function(data, sheet_name, output_file) {
  library(openxlsx)
  
  if (file.exists(output_file)) {
    wb <- loadWorkbook(output_file)
  } else {
    wb <- createWorkbook()
  }
  
  if (sheet_name %in% names(wb)) {
    removeWorksheet(wb, sheet_name)
  }
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, x = data)
  saveWorkbook(wb, output_file, overwrite = TRUE)
}
