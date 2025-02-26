
# ------ Parameters (EDIT) ------

# Provide directory paths to dictionary, nlp and codified data (csv)
nlp_path <- "/n/data1/hsph/biostat/celehs/lab/kimgreco/AI Paper/QC script/nlp_sample_1000.csv"
codified_path <- "/n/data1/hsph/biostat/celehs/lab/kimgreco/AI Paper/QC script/codified_sample_1000.csv"
dictionary_path <- "/n/data1/hsph/biostat/celehs/lab/SHARE/UPMC/data_mapping/mapping_clean/UPMC_Codified_data_codebook_with_descriptions_2023-05-15.csv"

# Provide target PheCode and CUI 
target_code <- "PheCode:335"
target_cui <- "C0026769"

# If running in O2, set O2 to TRUE; if not, set O2 to FALSE and provide manual directory paths to ONCE dictionaries for codified and nlp data
O2 <- TRUE
# manual_ONCE_path_code <- "/n/data1/hsph/biostat/celehs/lab/sm731/ONCE_features/codified/PheCode335_cod_features.csv"
# manual_ONCE_path_nlp <- "/n/data1/hsph/biostat/celehs/lab/sm731/ONCE_features/NLP/PheCode335_nlp_features.csv"




# ------ Code (DO NOT EDIT) ------ 

# 1.a. Import nlp & codified data
nlp <- read.csv(nlp_path)
codified <- read.csv(codified_path)

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
ONCE <- clean_ONCE_data(target_code, O2, manual_ONCE_path_code, manual_ONCE_path_nlp)







# ------ Test ------ 

# install.packages("devtools")
# devtools::install()
# library(RSQLite)
# library(SummaryStats)

test_clean_ONCE_data = function() {
  
  expect_true(TRUE)
}
test_that('clean_ONCE_data', test_clean_ONCE_data())



