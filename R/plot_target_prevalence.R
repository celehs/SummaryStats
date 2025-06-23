utils::globalVariables(c("theme_global", "theme_bar", "set_palette", "set_fill_palette"))

generate_all_patient_data <- function(data_inputs_subset, target_feature, sample_labels) {
  counts_list <- list()
  summary_list <- list()
  
  for (name in names(data_inputs_subset)) {
    sample_key <- ifelse(grepl("2", name), "2", "1")
    label <- sample_labels[[sample_key]]
    df <- data_inputs_subset[[name]]
    
    counts_list[[label]] <- generate_patient_counts(df, target_feature)
    summary_list[[label]] <- generate_total_patient_summary(df, target_feature, label)
  }
  
  list(counts = counts_list, summary = summary_list)
}

# 2.a. Patient counts per year for one dataset
generate_patient_counts <- function(data, target_feature) {
  total_counts <- data %>%
    group_by(year) %>%
    summarise(total_patients = n_distinct(patient_num), .groups = "drop")
  
  target_counts <- data %>%
    filter(feature_id == target_feature) %>%
    group_by(year) %>%
    summarise(target_patients = n_distinct(patient_num), .groups = "drop")
  
  total_counts %>%
    left_join(target_counts, by = "year") %>%
    rename(Year = year, `Total` = total_patients, `Target` = target_patients) %>%
    pivot_longer(cols = c("Total", "Target"), names_to = "Type", values_to = "Patients")
}

# 2.b. Plot patients by year (single or dual samples)
generate_line_plot <- function(counts_list, title) {
  combined_data <- dplyr::bind_rows(
    Map(function(df, label) mutate(df, Sample = label),
        counts_list,
        names(counts_list))
  )
  
  ggplot(combined_data, aes(
    x = Year,
    y = Patients,
    color = Type,
    group = interaction(Type, Sample),
    linetype = Sample
  )) +
    geom_line() +
    geom_point() +
    scale_linetype_manual(values = setNames(c("solid", "dashed")[seq_along(counts_list)], names(counts_list))) +
    labs(
      title = title,
      x = "",
      y = "Patients per Year",
      color = "Type",
      linetype = "Sample"
    ) +
    set_palette +
    theme_global
}

generate_line_plot <- function(counts_list, title) {
  combined_data <- dplyr::bind_rows(
    Map(function(df, label) mutate(df, Sample = label), counts_list, names(counts_list))
  )
  
  ggplot(combined_data, aes(
    x = Year,
    y = Patients,
    color = Type,
    group = interaction(Type, Sample),
    linetype = Sample
  )) +
    geom_line() +
    geom_point() +
    scale_linetype_manual(values = setNames(c("solid", "dashed"), names(counts_list))) +  # Dynamic line types
    labs(
      title = title,
      x = "",
      y = "Patients per Year",
      color = "Type",
      linetype = "Sample"
    ) +
    set_palette +
    theme_global
}

# 2.c. Total patient summary
generate_total_patient_summary <- function(data, target_feature, label) {
  data.frame(
    Feature = c("Total Patients", "Patients with Target"),
    Total_Patients = c(
      length(unique(data$patient_num)),
      length(unique(data$patient_num[data$feature_id == target_feature]))
    ),
    Dataset = label
  )
}

# 2.d. Stacked bar plot for 1 or 2 datasets
generate_bar_plot <- function(summary_list, title) {
  combined_data <- dplyr::bind_rows(summary_list)
  
  labels <- names(summary_list)
  first_label <- labels[1]
  second_label <- labels[2]
  
  ggplot(combined_data, aes(x = Feature, y = Total_Patients, fill = Feature)) +
    geom_bar(data = combined_data %>% filter(Dataset == second_label),
             stat = "identity", 
             position = position_identity(),
             width = 0.4,
             aes(x = as.numeric(as.factor(Feature)) + 0.1),
             color = "black",
             linetype = "dashed") +
    
    geom_bar(data = combined_data %>% filter(Dataset == first_label),
             stat = "identity", 
             position = position_identity(),
             width = 0.4,
             aes(x = as.numeric(as.factor(Feature)) - 0.1),
             color = "black") +
    
    scale_x_continuous(breaks = unique(as.numeric(as.factor(combined_data$Feature))), 
                       labels = unique(combined_data$Feature)) +
    labs(title = title, y = "Total Patients Across All Years", x = "Feature") +
    set_fill_palette +
    theme_bar
}

#' Visualize Target Feature Prevalence
#'
#' Generates line and bar plots showing annual trends and overall patient counts for a selected code and CUI.
#'
#' @importFrom dplyr bind_rows mutate filter group_by summarise rename left_join select n_distinct %>%
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_line geom_point scale_linetype_manual labs theme aes position_identity
#' 
#' @param data_inputs Named list of cleaned codified and NLP datasets.
#' @param target_code Codified feature ID of interest (e.g., "PheCode:335").
#' @param target_cui NLP-derived concept ID of interest (e.g., "C0026769").
#' @param sample_labels Named list labeling each sample (e.g., list("1" = "Site A", "2" = "Site B")).
#'
#' @return A list with three elements: `sample_sizes`, `nlp_plot`, and `codified_plot`.
#' @export
plot_target_prevalence <- function(data_inputs, target_code, target_cui, sample_labels) {
  
  # Total sample sizes
  sample_sizes <- lapply(names(data_inputs), function(name) {
    data <- data_inputs[[name]]
    data_type <- ifelse(grepl("nlp", name), "NLP", "Codified")
    sample_key <- ifelse(grepl("2", name), "2", "1")
    sample_label <- sample_labels[[sample_key]]
    data.frame(Sample = sample_label, Dataset = data_type,
               `Number of Patients` = length(unique(data$patient_num)))
  }) %>% dplyr::bind_rows()
  
  # NLP processing
  nlp_data_inputs <- data_inputs[grepl("nlp", names(data_inputs))]
  nlp_summaries <- generate_all_patient_data(nlp_data_inputs, target_cui, sample_labels)
  plot_nlp <- generate_line_plot(nlp_summaries$counts, "Sample Size by Year (NLP)")
  plot_bar_nlp <- generate_bar_plot(nlp_summaries$summary, "Total Patients vs. Target CUI")
  
  # Codified processing
  cod_data_inputs <- data_inputs[grepl("codified", names(data_inputs))]
  cod_summaries <- generate_all_patient_data(cod_data_inputs, target_code, sample_labels)
  plot_codified <- generate_line_plot(cod_summaries$counts, "Sample Size by Year (Codified)")
  plot_bar_codified <- generate_bar_plot(cod_summaries$summary, "Total Patients vs. Target Code")
  
  # Legends
  legend_nlp <- suppressWarnings(cowplot::get_legend(plot_nlp + theme(legend.position = "right")))
  legend_codified <- suppressWarnings(cowplot::get_legend(plot_codified + theme(legend.position = "right")))
  
  # Combine plots
  combined_plot_nlp <- cowplot::plot_grid(
    plot_nlp + theme(legend.position = "none"),
    plot_bar_nlp,
    legend_nlp,
    ncol = 3,
    rel_widths = c(3, 2, 1)
  )
  
  combined_plot_codified <- cowplot::plot_grid(
    plot_codified + theme(legend.position = "none"),
    plot_bar_codified,
    legend_codified,
    ncol = 3,
    rel_widths = c(3, 2, 1)
  )
  
  return(list(
    sample_sizes = sample_sizes,
    nlp_plot = combined_plot_nlp,
    codified_plot = combined_plot_codified
  ))
}