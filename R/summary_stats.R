#' Generate Intermediary SQLite Database
#'
#' @description 
#' Aggregates patient data from a source SQLite connection and writes it to an intermediary SQLite file. 
#' If a `time_column` is provided (e.g., "Month"), the function will extract the year using the first four characters 
#' and aggregate by `Patient`, `Parent_Code`, and `Year`. Otherwise, it aggregates by `Patient` and `Parent_Code` only.
#'
#' @param con A database connection object to an existing SQLite database (usually containing `df_monthly`).
#' @param output_sqlite_path A string indicating the file path to save the aggregated intermediary SQLite database.
#' @param time_column Optional. A string naming the column (e.g., "Month") from which to extract the year. The first four characters 
#' of each entry in this column must represent a 4-digit year (e.g., "2013", "2020-05", or "2017-12-03"). 
#'
#' @return Saves a new SQLite database file at the specified path containing a table named `processed_data`.
#' @export
generate_intermediary_sqlite <- function(con, output_sqlite_path, time_column = NULL) {
  if (!is.null(time_column)) {
    sql_query <- sprintf("
      SELECT 
        Patient, 
        Parent_Code, 
        SUBSTR(%s, 1, 4) AS Year,  
        SUM(Count) AS Count
      FROM df_monthly
      GROUP BY Patient, Parent_Code, Year
    ", time_column)
  } else {
    sql_query <- "
      SELECT 
        Patient, 
        Parent_Code, 
        SUM(Count) AS Count
      FROM df_monthly
      GROUP BY Patient, Parent_Code
    "
  }
  
  aggregated_data <- dbGetQuery(con, sql_query)
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



#' Extract Patient Counts Over Time
#'
#' @description Extracts patient counts for specified codes from a time-aggregated intermediary SQLite file. 
#' For each code selected, it returns the number of unique patients observed in each year.
#'
#' @param sqlite_file A string path to the intermediary SQLite database file generated from Step 1.
#' @param codes_of_interest A character vector of full codes (e.g., "RXNORM:1234", "PheCode:714.1") to extract.
#' @param dictionary_mapping A data frame that maps codes to descriptions. It must include `Group_Code`, `Group_Description`,
#' `Common_Ontology_Code`, and `Common_Ontology_Description`.
#'
#' @return A named list of data frames:
#' * Each element corresponds to one code, showing `Year`, `Parent_Code`, `Patient_Count`, and `Name`.
#' * The `"combined"` element contains the merged data for all codes.
#' 
#' @export
extract_patient_counts_over_years <- function(sqlite_file, codes_of_interest, dictionary_mapping) {
  con <- dbConnect(SQLite(), dbname = sqlite_file)
  
  dictionary_mapping[] <- lapply(dictionary_mapping, function(x) {
    if (isS4(x)) as.character(x) else x
  })
  
  query <- sprintf("
    SELECT Year, Parent_Code, COUNT(DISTINCT Patient) AS Patient_Count
    FROM processed_data
    WHERE Parent_Code IN ('%s')
    GROUP BY Year, Parent_Code
  ", paste(codes_of_interest, collapse = "', '"))
  
  patient_data <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  patient_data$Name <- sapply(patient_data$Parent_Code, get_description, dict = dictionary_mapping)
  
  final_data <- patient_data
  final_data$Name <- sapply(final_data$Parent_Code, get_description, dict = dictionary_mapping)
  
  data_list <- split(final_data, final_data$Parent_Code)
  data_list$combined <- final_data
  
  return(data_list)
}



#' Plot Patient Counts Over Time
#'
#' @description Generates a line plot showing patient counts per year for one or more codes. The input `data` should come from 
#' `extract_patient_counts_over_years()`. You can apply linear or log scale and filter to a specific year range.
#'
#' @param data A data frame from `output_data$combined` or a single code element from `extract_patient_counts_over_years()`.
#' @param title Title of the plot.
#' @param year_range Optional. A numeric vector of length 2 (e.g., `c(2000, 2020)`) to limit the x-axis range.
#' @param output_path Directory to save the plot if `save_plots = TRUE`. Defaults to `NULL`.
#' @param save_plots Logical; if `TRUE`, saves the plot to `output_path` as PNG.
#' @param auto_breaks Logical; if `TRUE`, enables non-uniform y-axis breaks based on patient count ranges.
#' @param log_scale Logical; if `TRUE`, applies log10 transformation to y-axis.
#'
#' @return A ggplot object.
#' @export
plot_patient_counts_over_time <- function(data, 
                                          title = "Patient Counts Over Time", 
                                          year_range = NULL, 
                                          output_path = NULL, 
                                          save_plots = FALSE,
                                          auto_breaks = FALSE,
                                          log_scale = FALSE) {
  data$Year <- as.numeric(as.character(data$Year))
  
  if (!is.null(year_range) && length(year_range) == 2) {
    data <- data[data$Year >= year_range[1] & data$Year <= year_range[2], ]
  }
  
  max_y <- max(data$Patient_Count, na.rm = TRUE)
  
  p <- ggplot(data, aes(x = Year, y = Patient_Count, color = Name)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 2.5) +
    labs(
      title = title,
      x = "Year",
      y = "Patient Count",
      color = "Code"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
      axis.text.y = element_text(size = 15),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      plot.title = element_text(size = 20, face = "bold"),
      plot.margin = unit(c(1, 1, 1, 4), "cm"),
      legend.position = "right",
      legend.justification = c(1, 1),
      legend.background = element_rect(fill = alpha('white', 0.8)),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      panel.grid.major.y = element_line(color = "gray85"),
      panel.grid.minor.y = element_blank()
    ) +
    scale_x_continuous(breaks = pretty(data$Year))
  
  if (auto_breaks) {
    p <- p + scale_y_continuous(
      breaks = custom_breaks(max_y),
      labels = scales::comma
    )
  }
  
  if (log_scale) {
    p <- p + scale_y_log10()
  }
  
  if (save_plots && !is.null(output_path)) {
    ggsave(filename = file.path(output_path, paste0(gsub(" ", "_", title), ".png")), 
           plot = p, width = 18, height = 8, dpi = 300)
  }
  
  return(p)
}
