utils::globalVariables(c("theme_global", "theme_bar", "set_palette", "set_fill_palette"))

get_similarity_ordered_colors <- function(descriptions_df) {
  desc_ordered <- descriptions_df %>%
    arrange(desc(target_similarity)) %>%
    mutate(description_label = paste0(feature_id, " | ", description, " [", round(target_similarity, 3), "]"))
  
  labels <- desc_ordered$description_label
  names(labels) <- desc_ordered$feature_id
  
  colors <- setNames(RColorBrewer::brewer.pal(n = length(labels), name = "Set2"), labels)
  
  list(
    labels = labels,
    color_map = colors
  )
}

plot_related_code_trends_v2 <- function(Type, target_code, target_cui, codified_list, nlp_list, sample_labels, ONCE,
                                        type_dict = list("Diagnosis" = "PheCode", 
                                                         "Medication" = "RXNORM", 
                                                         "Lab" = c("LOINC","ShortName","Other Lab"), 
                                                         "Procedure" = "CCS")) {
  
  if (!(Type %in% c(names(type_dict), "CUI"))) return(list(NULL, paste("Invalid Type:", Type)))
  
  is_cui <- (Type == "CUI")
  source_list <- if (is_cui) nlp_list else codified_list
  keyword <- if (!is_cui) type_dict[[Type]] else NULL
  target_feature <- if (is_cui) target_cui else target_code
  dict <- if (is_cui) ONCE$nlp else ONCE$code
  
  related_features <- dict %>%
    filter((if (is_cui) TRUE else str_detect(feature_id, paste0(keyword, collapse = "|"))) & feature_id != target_feature) %>%
    filter(feature_id %in% unlist(lapply(source_list, \(df) df$feature_id))) %>%
    arrange(desc(target_similarity)) %>%
    head(5)
  
  if (nrow(related_features) == 0) return(list(NULL, paste("No related features found for", Type)))
  
  selected_features <- c(target_feature, related_features$feature_id)
  
  # Patient-year counts
  long_data <- purrr::imap_dfr(source_list, function(df, key) {
    df %>%
      filter(feature_id %in% selected_features) %>%
      group_by(year, feature_id) %>%
      summarise(Patients = n_distinct(patient_num), .groups = "drop") %>%
      mutate(Sample = sample_labels[[key]])
  })
  
  total_data <- purrr::imap_dfr(codified_list, function(df, key) {
    df %>%
      group_by(year) %>%
      summarise(Total_Patients = n_distinct(patient_num), .groups = "drop") %>%
      mutate(Sample = sample_labels[[key]])
  })
  
  combined_data <- long_data %>%
    left_join(total_data, by = c("year", "Sample")) %>%
    mutate(Rate = Patients / Total_Patients)
  
  descriptions <- dict %>%
    filter(feature_id %in% selected_features) %>%
    arrange(desc(target_similarity)) %>%
    mutate(description_label = paste0(feature_id, " | ", description, " [", round(target_similarity, 3), "]"))
  
  color_info <- get_similarity_ordered_colors(descriptions)
  
  combined_data <- combined_data %>%
    left_join(descriptions, by = "feature_id") %>%
    mutate(
      description_label = factor(description_label, levels = color_info$labels),
      is_target = ifelse(feature_id == target_feature, "Target", "Other")
    )
  
  bar_data <- expand.grid(
    feature_id = selected_features,
    Sample = as.character(unname(sample_labels)),
    stringsAsFactors = FALSE
  ) %>%
    left_join(descriptions, by = "feature_id") %>%
    mutate(description_label = factor(description_label, levels = color_info$labels))
  
  bar_counts <- purrr::imap_dfr(source_list, function(df, key) {
    df %>%
      filter(feature_id %in% selected_features) %>%
      distinct(patient_num, feature_id) %>%
      group_by(feature_id) %>%
      summarise(Total_Patients = n(), .groups = "drop") %>%
      mutate(Sample = sample_labels[[key]])
  })
  
  bar_data <- bar_data %>%
    left_join(bar_counts, by = c("feature_id", "Sample")) %>%
    mutate(Total_Patients = replace_na(Total_Patients, 0)) %>%
    filter(Total_Patients > 0)
  
  target_desc <- descriptions$description[descriptions$feature_id == target_feature]
  subtitle_text <- paste0("<i><b>Target ", ":</b> ", target_feature, "</i> | <i>", target_desc, "</i>")
  
  plot_rates <- ggplot(combined_data, aes(x = as.numeric(year), y = Rate,
                                          color = description_label, linetype = Sample,
                                          group = interaction(feature_id, Sample), size = is_target)) +
    geom_line() + geom_point(size = 3) +
    scale_size_manual(values = c("Target" = 2, "Other" = 1), guide = "none") +
    scale_linetype_manual(values = c("solid", "dashed")[seq_along(unique(combined_data$Sample))]) +
    scale_color_manual(values = color_info$color_map) +
    labs(title = paste("Rates for Related", Type, "Codes"),
         subtitle = subtitle_text,
         x = "", y = "Rate", color = Type, linetype = "Sample") +
    theme_global +
    scale_x_continuous(breaks = unique(as.numeric(combined_data$year)))
  
  plot_counts <- ggplot(combined_data, aes(x = as.numeric(year), y = Patients,
                                           color = description_label, linetype = Sample,
                                           group = interaction(feature_id, Sample), size = is_target)) +
    geom_line() + geom_point(size = 3) +
    scale_size_manual(values = c("Target" = 2, "Other" = 1), guide = "none") +
    scale_linetype_manual(values = c("solid", "dashed")[seq_along(unique(combined_data$Sample))]) +
    scale_color_manual(values = color_info$color_map) +
    labs(title = paste("Patient Counts for Related", Type, "Codes"),
         subtitle = subtitle_text,
         x = "", y = "Patients per Year", color = Type, linetype = "Sample") +
    theme_global +
    scale_x_continuous(breaks = unique(as.numeric(combined_data$year)))
  
  # --- UPDATED SECTION FOR 1 OR 2 SAMPLES ---
  sample_names <- unique(bar_data$Sample)
  
  if (length(sample_names) == 1) {
    plot_bar <- ggplot(bar_data, aes(x = description_label, y = Total_Patients, fill = description_label)) +
      geom_bar(stat = "identity", position = position_dodge(), width = 0.6, color = "black") +
      scale_fill_manual(values = color_info$color_map) +
      labs(y = "Total Patients Across All Years", x = "") +
      theme_bar +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = "none")
  } else if (length(sample_names) == 2) {
    plot_bar <- ggplot(bar_data, aes(fill = description_label)) +
      geom_bar(data = bar_data %>% filter(Sample == sample_names[2]),
               aes(x = as.numeric(description_label) + 0.1, y = Total_Patients),
               stat = "identity", position = position_identity(), width = 0.4,
               color = "black", linetype = "dashed") +
      geom_bar(data = bar_data %>% filter(Sample == sample_names[1]),
               aes(x = as.numeric(description_label) - 0.1, y = Total_Patients),
               stat = "identity", position = position_identity(), width = 0.4,
               color = "black") +
      scale_x_continuous(breaks = unique(as.numeric(bar_data$description_label)),
                         labels = levels(bar_data$description_label)) +
      scale_fill_manual(values = color_info$color_map) +
      labs(y = "Total Patients Across All Years", x = "") +
      theme_bar +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = "none")
  } else {
    stop("plot_related_code_trends_v2 supports only 1 or 2 samples.")
  }
  
  combined_plot <- cowplot::plot_grid(
    plot_counts + theme(legend.position = "none"),
    plot_bar,
    ncol = 2,
    rel_widths = c(2, 1)
  )
  
  return(list(list(plot_rates, combined_plot), NULL))
}

#' Visualize ONCE-Derived Related Features
#'
#' Identifies and plots ONCE-derived features most similar to the target code/CUI across domains.
#' 
#' @importFrom dplyr filter group_by mutate summarise left_join pull n_distinct case_when select rename distinct
#' @importFrom purrr imap_dfr map
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom stringr str_detect
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_bar labs scale_size_manual scale_linetype_manual scale_color_manual scale_fill_manual scale_x_continuous position_dodge position_identity theme
#' @importFrom cowplot plot_grid get_legend
#' @importFrom RColorBrewer brewer.pal
#' 
#' @param data_inputs Cleaned codified and NLP datasets.
#' @param target_code Target Phecode.
#' @param target_cui Target CUI.
#' @param sample_labels Named list labeling the samples.
#' @param O2 Logical flag for using O2 default paths.
#' @param manual_ONCE_path_code Optional codified dictionary path if O2 = FALSE.
#' @param manual_ONCE_path_nlp Optional NLP dictionary path if O2 = FALSE.
#' @param types Character vector of domains (e.g., "Diagnosis", "Lab").
#' @param type_dict Named list mapping domains to feature prefixes.
#'
#' @return A list of plots or messages for each domain.
#' @export
plot_related_features <- function(data_inputs, target_code, target_cui, sample_labels,
                                  O2 = FALSE,
                                  manual_ONCE_path_code = NULL,
                                  manual_ONCE_path_nlp = NULL,
                                  types = c("Diagnosis", "Medication", "Lab", "Procedure", "CUI"),
                                  type_dict = list("Diagnosis" = "PheCode", 
                                                   "Medication" = "RXNORM", 
                                                   "Lab" = c("LOINC","ShortName","Other Lab"), 
                                                   "Procedure" = "CCS")) {
  
  # Load ONCE dictionary
  ONCE <- clean_ONCE_data(target_code, O2, manual_ONCE_path_code, manual_ONCE_path_nlp)
  
  # Build codified and NLP lists
  codified_list <- list()
  nlp_list <- list()
  if (!is.null(data_inputs$codified1)) codified_list[["1"]] <- data_inputs$codified1
  if (!is.null(data_inputs$codified2)) codified_list[["2"]] <- data_inputs$codified2
  if (!is.null(data_inputs$nlp1)) nlp_list[["1"]] <- data_inputs$nlp1
  if (!is.null(data_inputs$nlp2)) nlp_list[["2"]] <- data_inputs$nlp2
  
  # Generate plots or messages for each selected type
  plot_results <- lapply(types, function(t) {
    result <- plot_related_code_trends_v2(
      Type = t,
      target_code = target_code,
      target_cui = target_cui,
      codified_list = codified_list,
      nlp_list = nlp_list,
      sample_labels = sample_labels,
      ONCE = ONCE,
      type_dict = type_dict
    )
    
    list(
      type = t,
      line_plot = if (!is.null(result[[1]])) result[[1]][[1]] else NULL,
      combined_plot = if (!is.null(result[[1]])) result[[1]][[2]] else NULL,
      message = if (is.null(result[[1]])) result[[2]] else NULL
    )
  })
  
  return(plot_results)
}

