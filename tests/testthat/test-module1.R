
# ------ Parameters (EDIT) ------

dirpath = system.file('extdata', package = 'SummaryStats')
# Provide directory paths to dictionary, nlp and codified data (csv)
# nlp_path <- "/Users/kimberlygreco/Dropbox/CELEHS/testdata/fake_nlp.csv"
# codified_path <- "/Users/kimberlygreco/Dropbox/CELEHS/testdata/fake_codified.csv"
# dictionary_path <- "/Users/kimberlygreco/Dropbox/CELEHS/testdata/fake_dict.csv"

nlp_path <- file.path(dirpath, "fake_nlp.csv")
codified_path <- file.path(dirpath, "fake_codified.csv")
dictionary_path <- file.path(dirpath, "fake_dict.csv")

# Provide target PheCode and CUI 
target_code <- "PheCode:335"
target_cui <- "C0026769"

# If running in O2, set O2 to TRUE; if not, set O2 to FALSE and provide manual directory paths to ONCE dictionaries for codified and nlp data
O2 <- FALSE
# manual_ONCE_path_code <- "/Users/kimberlygreco/Dropbox/CELEHS/testdata/ONCE_multiple_sclerosis_codified.csv"
# manual_ONCE_path_nlp <- "/Users/kimberlygreco/Dropbox/CELEHS/testdata/ONCE_multiple_sclerosis_nlp.csv"

manual_ONCE_path_code <- file.path(dirpath, "ONCE_multiple_sclerosis_codified.csv")
manual_ONCE_path_nlp <- file.path(dirpath, "ONCE_multiple_sclerosis_nlp.csv")

# ------ Code (DO NOT EDIT) ------ 

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggtext)
library(knitr)
library(cowplot)
library(RColorBrewer)

# 1.a. Import nlp & codified data
nlp <- read.csv(nlp_path)
codified <- read.csv(codified_path)

head(nlp)
head(codified)

codified <- codified %>%
  mutate(
    patient_num = as.character(patient_num),  
    year = format(as.Date(start_date), "%Y"),
    feature_id = str_replace(feature_id, "CCS-PCS", "CCS")
  )

nlp <- nlp %>%
  mutate(
    patient_num = as.character(patient_num), 
    year = format(as.Date(start_date), "%Y"),
    feature_id = str_replace(feature_id, "CCS-PCS", "CCS")
  )

# 1.b. Data dictionary processing
dictionary <- read.csv(dictionary_path)

filtered_dictionary <- dictionary %>%
  filter(
    grepl(target_code, feature_id) |        
      grepl(target_cui, feature_id) |         
      grepl("^PheCode:250(\\.\\d+)?$", feature_id) | 
      grepl("^PheCode:411(\\.\\d+)?$", feature_id) |  
      grepl("^PheCode:296(\\.\\d+)?$", feature_id)  
  ) %>%
  dplyr::select(feature_id, description) %>%
  distinct(feature_id, .keep_all = TRUE) %>%
  mutate(description = case_when(
    str_detect(description, "forms of") ~ str_replace(description, "(.*)forms of.*", "\\1forms"), 
    TRUE ~ str_replace_all(description, "\\s*\\(.*?\\)|,.*", "")
  )) %>%
  mutate(description = tolower(description)) 

# 1.c. ONCE dictionary processing
# ONCE <- clean_ONCE_data(target_code, O2, manual_ONCE_path_code, manual_ONCE_path_nlp)







# ------ Test ------ 

# Export to NAMESPACE 
# devtools::document()

# Install package
# install.packages("devtools")
# devtools::install()

# test_clean_ONCE_data = function() {
#   
#   expect_true(TRUE)
# }
# test_that('clean_ONCE_data', test_clean_ONCE_data())

# devtools::document()
# devtools::install()

# library(testthat)
# library(SummaryStats)

test_that("clean_ONCE_data errors when manual paths are missing", {
  expect_error(clean_ONCE_data(target_code = "PheCode:335", FALSE))
})


