
# ----------------------- MODULE 2 ----------------------- 

clean_ONCE_data <- function(target_code, O2, manual_ONCE_path_code, manual_ONCE_path_nlp) {
  
  # Format target_code
  formatted_code <- gsub(":", "", target_code)
  formatted_code <- gsub("\\.", "_", formatted_code)
  
  # Define paths based on O2 flag
  if (O2) {
    ONCE_path_code <- paste0("/n/data1/hsph/biostat/celehs/lab/sm731/ONCE_features/codified/", formatted_code, "_cod_features.csv")
    ONCE_path_nlp <- paste0("/n/data1/hsph/biostat/celehs/lab/sm731/ONCE_features/NLP/", formatted_code, "_nlp_features.csv")
  } else {
    if (is.null(manual_ONCE_path_code) || is.null(manual_ONCE_path_nlp)) {
      stop("Manual paths must be provided when O2 is FALSE.")
    }
    ONCE_path_code <- manual_ONCE_path_code
    ONCE_path_nlp <- manual_ONCE_path_nlp
  }
  
  # Read CSV files
  ONCE_dictionary_code <- read.csv(ONCE_path_code)
  ONCE_dictionary_nlp <- read.csv(ONCE_path_nlp)
  
  # Process Code Dictionary
  ONCE_dictionary_code <- ONCE_dictionary_code %>%
    dplyr::select(Variable, Description, target_similarity) %>%
    rename(feature_id = Variable, description = Description) %>%
    mutate(description = tolower(description))
  
  # Process NLP Dictionary
  ONCE_dictionary_nlp <- ONCE_dictionary_nlp %>%
    dplyr::select(cui, term, target_similarity) %>%
    rename(feature_id = cui, description = term) %>%
    mutate(description = tolower(description))
  
  return(list(code = ONCE_dictionary_code, nlp = ONCE_dictionary_nlp))
}

# ----------------------- MODULE 2 ----------------------- 

# Define a function to calculate patient counts by year
generate_patient_counts <- function(data, target_feature) {
  total_counts <- data %>%
    group_by(year) %>%
    summarise(total_patients = n_distinct(patient_num))
  
  target_counts <- data %>%
    filter(feature_id == target_feature) %>%
    group_by(year) %>%
    summarise(target_patients = n_distinct(patient_num))
  
  merged_counts <- total_counts %>%
    left_join(target_counts, by = "year") %>%
    rename(Year = year, `Total` = total_patients, `Target` = target_patients) %>%
    pivot_longer(cols = c(`Total`, `Target`), names_to = "Type", values_to = "Patients")
  
  return(merged_counts)
}

# Define a function to create line plot
generate_line_plot <- function(data, title) {
  ggplot(data, aes(x = Year, y = Patients, color = Type, group = Type)) +
    geom_line() +
    geom_point() +
    labs(title = title, x = "Year", y = "Patients per Year", color = "Sample") +
    set_palette +
    theme_global
}

# Define a function to create bar plot
generate_bar_plot <- function(summary_data, title) {
  ggplot(summary_data, aes(x = Feature, y = Total_Patients, fill = Feature)) +
    geom_bar(stat = "identity") +
    labs(title = title, y = "Total Patients Across all Years") +
    set_fill_palette +
    theme_bar
}

# ----------------------- MODULE 3 ----------------------- 

# Function to summarize PheCode data
generate_phecode_summary <- function(data) {
  total_patients <- data %>%
    group_by(year) %>%
    summarise(Total_Patients = n_distinct(patient_num), .groups = "drop")
  
  data %>%
    group_by(year, feature_id) %>%
    summarise(Count = n(), Patients = n_distinct(patient_num), .groups = "drop") %>%
    left_join(total_patients, by = "year") %>%
    mutate(Rate = Patients / Total_Patients)
}

# Function to retrieve PheCode description
get_phecode_description <- function(phecode_pattern, dictionary) {
  description <- dictionary %>%
    filter(feature_id == paste0("PheCode:", phecode_pattern)) %>%
    pull(description)
  ifelse(length(description) == 0, "description not found", description)
}

# Function to prepare legend labels
get_legend_labels <- function(phecode_pattern, dictionary) {
  dictionary %>%
    filter(str_detect(feature_id, paste0("^PheCode:", phecode_pattern)) & !str_detect(feature_id, "\\.\\d{2,}")) %>%
    mutate(feature_label = paste0(feature_id, " | ", description)) %>%
    arrange(as.numeric(str_remove(feature_id, "PheCode:"))) %>%
    pull(feature_label)
}

# Function to generate line plots for rates and counts
plot_trends <- function(data, phecode_pattern, dictionary, y_var, y_label, title_suffix) {
  phecode_description <- get_phecode_description(phecode_pattern, dictionary)
  legend_labels <- get_legend_labels(phecode_pattern, dictionary)
  
  ggplot(data %>% filter(str_detect(feature_id, paste0("^PheCode:", phecode_pattern)) & !str_detect(feature_id, "\\.\\d{2,}")),
         aes(x = year, y = !!sym(y_var), color = feature_id, group = feature_id)) +
    geom_line() +
    geom_point() +
    scale_color_brewer(palette = "Set2", labels = legend_labels) +
    labs(
      title = paste("Parent-Child PheCodes", title_suffix),
      subtitle = paste0("<b><i>PheCode: ", phecode_pattern, "</i></b> | <i>", phecode_description, "</i>"),
      x = "Year",
      y = y_label,
      color = "Code"
    ) +
    theme_global
}

# Function to generate bar plot for total unique patient counts across all years
plot_total_patients <- function(data, phecode_pattern, dictionary) {
  total_patients_per_phecode <- data %>%
    filter(str_detect(feature_id, paste0("^PheCode:", phecode_pattern)) & !str_detect(feature_id, "\\.\\d{2,}")) %>%
    group_by(feature_id) %>%
    summarise(Total_Patients = n_distinct(patient_num), .groups = "drop") %>%
    left_join(dictionary, by = "feature_id") %>%
    mutate(feature_label = paste0(feature_id, " | ", description))
  
  ggplot(total_patients_per_phecode, aes(x = feature_label, y = Total_Patients, fill = feature_label)) +
    geom_bar(stat = "identity") +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "", y = "Total Patients Across all Years") +
    theme_bar
}

# ----------------------- MODULE 4 ----------------------- 

# ----------------------- MODULE 5 ----------------------- 

# Function to extract top 5 variables for each category, ordered by target similarity
get_top_related_codes <- function(keyword, target_code) {
  ONCE$code %>%
    filter(str_detect(feature_id, keyword) & feature_id != target_code) %>%
    filter(feature_id %in% codified$feature_id) %>%  
    arrange(desc(target_similarity)) %>%
    head(5)
}

# Function to extract top 5 CUIs, ordered by target similarity
get_top_related_cuis <- function(target_cui) {
  ONCE$nlp %>%
    filter(feature_id != target_cui) %>%
    filter(feature_id %in% nlp$feature_id) %>%  
    arrange(desc(target_similarity)) %>%
    head(5)
}

plot_related_code_trends <- function(Type, target_code, target_cui) {
  type_dict <- list(
    "Diagnosis" = "PheCode",
    "Medication" = "RXNORM",
    "Lab" = "LOINC",
    "Procedure" = "CCS"
  )
  
  if (!(Type %in% c(names(type_dict), "CUI"))) return(list(NULL, paste("Invalid Type:", Type)))
  
  related_codes <- if (Type == "CUI") get_top_related_cuis(target_cui) else get_top_related_codes(type_dict[[Type]], target_code)
  
  if (nrow(related_codes) == 0) {
    return(list(NULL, paste("No data corresponding to related **", Type, " Codes** from ONCE dictionary.", sep = "")))
  }
  
  selected_vars <- c(if (Type == "CUI") target_cui else target_code, related_codes$feature_id)
  
  related_data <- if (Type == "CUI") nlp else codified
  related_data <- related_data %>%
    filter(feature_id %in% selected_vars) %>%
    group_by(year, feature_id) %>%
    summarise(Patients = n_distinct(patient_num), .groups = "drop")
  
  total_patients <- codified %>%
    group_by(year) %>%
    summarise(Total_Patients = n_distinct(patient_num), .groups = "drop")
  
  combined_data <- related_data %>%
    left_join(total_patients, by = "year") %>%
    mutate(Rate = Patients / Total_Patients)
  
  descriptions <- if (Type == "CUI") ONCE$nlp else ONCE$code
  descriptions <- descriptions %>%
    filter(feature_id %in% selected_vars) %>%
    dplyr::select(feature_id, description, target_similarity) %>%
    arrange(desc(target_similarity)) %>%
    mutate(description_label = factor(
      paste0(feature_id, " | ", description, " [", round(target_similarity, 3), "]"),
      levels = paste0(feature_id, " | ", description, " [", round(target_similarity, 3), "]")
    ))
  
  combined_data <- combined_data %>%
    left_join(descriptions, by = "feature_id") %>%
    mutate(description_label = factor(description_label, levels = levels(descriptions$description_label)))
  
  target_description <- descriptions$description[descriptions$feature_id == (if (Type == "CUI") target_cui else target_code)]
  target_description <- ifelse(length(target_description) > 0, target_description, "No Description Available")
  subtitle_text <- paste0("<i><b>Target ", Type, ":</b> ", if (Type == "CUI") target_cui else target_code, "</i> | <i>", target_description, "</i>")
  
  if (all(combined_data$feature_id == (if (Type == "CUI") target_cui else target_code))) {
    return(list(NULL, paste("No data on related *", Type, " Codes* in sample.", sep = "")))
  }
  
  unique_patients_by_code <- (if (Type == "CUI") nlp else codified) %>%
    filter(feature_id %in% selected_vars) %>%
    group_by(feature_id) %>%
    summarise(Total_Patients = n_distinct(patient_num), .groups = "drop") %>%
    left_join(descriptions, by = "feature_id") %>%
    mutate(description_label = factor(description_label, levels = levels(descriptions$description_label))) %>%
    arrange(desc(Total_Patients))
  
  num_colors <- n_distinct(combined_data$description_label)
  color_palette <- scale_color_manual(values = RColorBrewer::brewer.pal(n = min(8, num_colors), "Set2"))
  fill_palette <- scale_fill_manual(values = RColorBrewer::brewer.pal(n = min(8, num_colors), "Set2"))
  
  # Plot rate
  plot_rates <- ggplot(combined_data, aes(x = as.numeric(year), y = Rate, color = description_label, group = feature_id)) +
    geom_line() + 
    geom_point() +
    color_palette +  
    labs(title = paste("Rate Trends for Target and Related", Type, "Codes"),
         subtitle = subtitle_text,  
         x = "Year", y = "Rate", color = paste(Type, if (Type != "CUI") "Code" else "")) +
    scale_x_continuous(breaks = unique(as.numeric(combined_data$year))) +
    theme_global
  
  # Plot patient counts
  plot_counts <- ggplot(combined_data, aes(x = as.numeric(year), y = Patients, color = description_label, group = feature_id)) +
    geom_line() + 
    geom_point() +
    color_palette +  
    labs(title = paste("Patient Counts for Target and Related", Type, "Codes"),
         subtitle = subtitle_text,  
         x = "Year", y = "Patients per Year", color = paste(Type, if (Type != "CUI") "Code" else "")) +
    scale_x_continuous(breaks = unique(as.numeric(combined_data$year))) +
    theme_global
  
  # Summary bar plot
  plot_bar_counts <- ggplot(unique_patients_by_code, aes(y = Total_Patients, x = description_label, fill = description_label)) +
    geom_bar(stat = "identity") +
    fill_palette +  
    labs(y = "Total Patients Across all Years", x = "", title = "") +
    theme_bar
  
  legend <- cowplot::get_legend(plot_counts)
  
  combined_plot <- cowplot::plot_grid(
    plot_counts + theme(legend.position = "none"), 
    plot_bar_counts, 
    legend, 
    ncol = 3, rel_widths = c(2, .75, 2)
  )
  
  return(list(list(plot_rates, combined_plot), NULL))
}

