#' Generate Intermediary SQLite Database
#'
#' @description This function aggregates patient data and stores it in an intermediary SQLite file.
#'
#' @param con A database connection object to an existing SQLite database.
#' @param output_sqlite_path Path to the output SQLite file where processed data will be stored.
#' 
#' @return Saves the aggregated data to an SQLite file.
#' @export
generate_intermediary_sqlite <- function(con, output_sqlite_path) {
  # Perform aggregation directly in SQL
  sql_query <- "
    SELECT 
      Patient, 
      Parent_Code, 
      SUM(Count) AS Count
    FROM 
      df_monthly
    GROUP BY 
      Patient, Parent_Code
  "
  # Execute the SQL query and save the results into an intermediary SQLite file
  aggregated_data <- dbGetQuery(con, sql_query)
  
  # Save the aggregated data to the SQLite file
  con_out <- dbConnect(SQLite(), dbname = output_sqlite_path)
  dbWriteTable(con_out, "processed_data", aggregated_data, overwrite = TRUE)
  dbDisconnect(con_out)
}


#' Extract Data for Visualization
#'
#' @description This function extracts and processes data from an SQLite database for visualization.
#'
#' @param sqlite_file Path to the SQLite file containing processed data.
#' @param prefix Prefix for medical codes (e.g., "RXNORM:", "LOINC:").
#' @param top_n Number of top occurrences to select (default = 20).
#' @param additional_vars A vector of additional variables to include (default = NULL).
#' @param wanted_items_df A data frame of specific variables to extract (default = NULL).
#' @param manual_replacement_bank A named list for renaming specific codes (default = NULL).
#' @param dict_prefix Dictionary prefix for mapping (default = `prefix`).
#' @param dictionary_mapping the dictionary used to map codes
#'
#' @return A list containing:
#'   - `Total_Counts`: A summary of total occurrences for each medical code.
#'   - `Patient_Counts`: The number of unique patients associated with each medical code.
#' @export
extract_data_for_visualization <- function(sqlite_file, prefix, top_n = 20, additional_vars = NULL, wanted_items_df = NULL, manual_replacement_bank = NULL, dict_prefix = prefix, dictionary_mapping) {
  con <- dbConnect(SQLite(), dbname = sqlite_file)
  
  if (!is.null(wanted_items_df)) {
    # 'want' case
    selected_items <- map_items(wanted_items_df, dictionary_mapping)  # Explicitly pass dictionary_mapping
    combined_data <- fetch_data_for_items(con, selected_items, batch_size = 1000)
    code_summary <- create_summary_data(combined_data, selected_items, "Total_Count")
    patient_summary <- create_summary_data(combined_data, selected_items, "Patient_Count")
  } else if (!is.null(additional_vars)) {
    # 'add' case
    top_n_codes <- fetch_top_n_codes(con, prefix, batch_size = 1000, top_n)
    code_summary <- process_additional_vars(con, top_n_codes$Total, additional_vars, prefix, prefix, dictionary_mapping, top_n)  # Pass dictionary_mapping
    patient_summary <- process_additional_vars(con, top_n_codes$Patient, additional_vars, prefix, prefix, dictionary_mapping, top_n)
  } else {
    # 'null' case
    top_n_codes <- fetch_top_n_codes(con, prefix, batch_size = 1000, top_n = top_n)
    code_summary <- map_descriptions(top_n_codes$Total, prefix, dictionary_mapping, dict_prefix)  # Pass dictionary_mapping
    patient_summary <- map_descriptions(top_n_codes$Patient, prefix, dictionary_mapping, dict_prefix)
  }
  
  # Apply manual replacements if provided
  if (!is.null(manual_replacement_bank)) {
    code_summary$Name <- recode(code_summary$Name, !!!manual_replacement_bank)
    patient_summary$Name <- recode(patient_summary$Name, !!!manual_replacement_bank)
  }
  
  dbDisconnect(con)
  
  return(list(Total_Counts = code_summary, Patient_Counts = patient_summary))
}



#' Plot Visualized Data
#'
#' @description This function generates and optionally saves histograms for patient and total counts.
#'
#' @param data A list containing two data frames: `Total_Counts` and `Patient_Counts`.
#' @param count_column Character. Specify whether to plot "Total_Count" or "Patient_Count". If NULL, both are plotted.
#' @param prefix Prefix for medical codes (e.g., "RXNORM:", "LOINC:").
#' @param description_label A label describing the dataset.
#' @param output_path Path to save the plots (default = `tempdir()`).
#' @param save_plots Logical. Whether to save plots as image files (default = FALSE).
#' @param log_scale Logical. Whether to apply a logarithmic scale to the y-axis (default = FALSE).
#'
#' @return A ggplot object or a list of ggplot objects.
#' @export
plot_visualized_data <- function(data, count_column = NULL, prefix, description_label, output_path = tempdir(), save_plots = FALSE, log_scale = FALSE) {
  x_label <- switch(
    prefix,
    'CCS-PCS:' = 'CCS (IDs)',
    'PheCode:' = 'PheCode Labels (IDs)',
    'LOINC:' = 'Lab (IDs)',
    'RXNORM:' = 'RXNORM Ingredient (IDs)'
  )
  
  plots <- list()
  
  if (!is.list(data)) {
    stop("data must be a list containing 'Total_Counts' and/or 'Patient_Counts'.")
  }
  
  # Handle both counts if count_column is not specified
  if (is.null(count_column)) {
    if ("Total_Counts" %in% names(data)) {
      plots$Total_Count_Plot <- create_histogram_plot(
        data = data$Total_Counts,
        count_column = "Total_Count",
        x_label = x_label,
        y_label = "Total Count",
        plot_title = paste(description_label, "(Total Counts)"),
        save_path = if (save_plots) file.path(output_path, paste0(description_label, "_Total_Count.png")) else NULL,
        save_plots = save_plots,
        log_scale = log_scale
      )
    }
    if ("Patient_Counts" %in% names(data)) {
      plots$Patient_Count_Plot <- create_histogram_plot(
        data = data$Patient_Counts,
        count_column = "Patient_Count",
        x_label = x_label,
        y_label = "Patient Count",
        plot_title = paste(description_label, "(Patient Counts)"),
        save_path = if (save_plots) file.path(output_path, paste0(description_label, "_Patient_Count.png")) else NULL,
        save_plots = save_plots,
        log_scale = FALSE
      )
    }
  } else {
    # Handle single count_column explicitly
    count_type <- ifelse(count_column == "Total_Count", "Total_Counts", "Patient_Counts")
    if (count_type %in% names(data)) {
      plots[[count_type]] <- create_histogram_plot(
        data = data[[count_type]],
        count_column = count_column,
        x_label = x_label,
        y_label = ifelse(count_column == "Total_Count", "Total Count", "Patient Count"),
        plot_title = paste(description_label, paste0("(", count_column, ")")),
        save_path = if (save_plots) file.path(output_path, paste0(description_label, "_", count_column, ".png")) else NULL,
        save_plots = save_plots,
        log_scale = ifelse(count_column == "Total_Count", log_scale, FALSE)
      )
    } else {
      stop("Invalid count_column or corresponding data is missing in the input list.")
    }
  }
  
  # Return a single plot if only one exists
  if (length(plots) == 1) {
    return(plots[[1]])
  }
  
  return(plots)
}