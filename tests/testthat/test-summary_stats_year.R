  # Simulated dictionary
  dictionary_mapping <- data.frame(
    Group_Code = c("RXNORM:123", "RXNORM:456"),
    Common_Ontology_Code = c("RXNORM:001", "RXNORM:002"),
    Common_Ontology_Description = c("Acetaminophen", "Ibuprofen"),
    Group_Description = c("Pain Relievers", "NSAIDs"),
    stringsAsFactors = FALSE
  )
  
  # Simulated EHR data with Year column
  df_ehr <- data.frame(
    Year = sample(1995:2020, 1000, replace = TRUE),
    Patient = sample(1:500, 1000, replace = TRUE),
    Parent_Code = sample(c("RXNORM:123", "RXNORM:456", "RXNORM:789"), 1000, replace = TRUE),
    Count = sample(1:10, 1000, replace = TRUE)
  )
  
  # Step 1: Save df_ehr to SQLite
  test_db_path <- tempfile()
  test_db <- dbConnect(SQLite(), test_db_path)
  dbWriteTable(test_db, "df_monthly", df_ehr, overwrite = TRUE)
  
  # Step 2: Generate intermediary SQLite with year column
  intermediary_test_db_path <- tempfile()
  generate_intermediary_sqlite(test_db, output_sqlite_path = intermediary_test_db_path, time_column = "Year")
  dbDisconnect(test_db)
  
  # Step 3: Extract data over years
  data_test <- extract_patient_counts_over_years(
    sqlite_file = intermediary_test_db_path,
    codes_of_interest = c("RXNORM:123", "RXNORM:456", "RXNORM:789"),
    dictionary_mapping = dictionary_mapping
  )
  
  # Step 4: Create the ggplot object from combined data
  plot_obj <- plot_patient_counts_over_time(
    data = data_test$combined,
    title = "Test Trend Plot",
    year_range = c(2000, 2020),
    auto_breaks = TRUE,
    log_scale = FALSE,
    save_plots = FALSE
  )
  
  # Step 5: Check that plot is created
  expect_true(ggplot2::is.ggplot(plot_obj))
  expect_true(all(c("Year", "Patient_Count", "Parent_Code", "Name") %in% colnames(data_test$combined)))

