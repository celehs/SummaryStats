#' Clean Raw Data for QC Processing
#'
#' Standardizes date formats and extracts patient/year/feature IDs from a raw dataset.
#' Used as a helper for `clean_data()`.
#'
#' @param df A data frame with at least `patient_num`, a date column, and a feature ID column.
#' @param date_col The column name containing dates. Must be either `YYYY` or `YYYY-MM-DD` format.
#' @param id_col The column name containing feature identifiers (e.g., "feature_id").
#'
#' @return A cleaned data frame with columns: `patient_num`, `year`, and the cleaned feature ID column.
#' @keywords internal
clean_raw_data <- function(df, date_col = "start_date", id_col = "feature_id") {
  df <- df %>%
    mutate({{ date_col }} := as.character(.data[[date_col]]))
  
  bad_dates <- df %>%
    filter(!str_detect(.data[[date_col]], "^\\d{4}$|^\\d{4}-\\d{2}-\\d{2}$"))
  
  if (nrow(bad_dates) > 0) {
    warning(nrow(bad_dates), " rows have invalid date format in `", date_col, "` and were set to NA.")
  }
  
  df <- df %>%
    mutate(
      patient_num = as.character(patient_num),
      year = dplyr::case_when(
        str_detect(.data[[date_col]], "^\\d{4}$") ~ .data[[date_col]],
        str_detect(.data[[date_col]], "^\\d{4}-\\d{2}-\\d{2}$") ~ as.character(
          lubridate::year(suppressWarnings(lubridate::ymd(.data[[date_col]])))
        ),
        TRUE ~ NA_character_
      ),
      {{ id_col }} := str_replace(.data[[id_col]], "CCS-PCS", "CCS")
    )
  
  df %>%
    select(patient_num, year, {{ id_col }})
}

#' Load and Clean Raw Codified/NLP Feature Files
#'
#' Applies `clean_raw_data()` to a list of CSV file paths. Returns a list of cleaned datasets.
#'
#' @param paths A named list of paths to CSV files.
#' @param date_col Column name containing date information (default: `"start_date"`).
#' @param id_col Column name for feature identifiers (default: `"feature_id"`).
#'
#' @return A named list of cleaned data frames, each with columns: `patient_num`, `year`, and `feature_id`.
#' @export
clean_data <- function(paths, date_col = "start_date", id_col = "feature_id") {
  input_data <- lapply(paths, read.csv)
  cleaned <- lapply(input_data, clean_raw_data, date_col = date_col, id_col = id_col)
  names(cleaned) <- names(paths)
  return(cleaned)
}
