utils::globalVariables(c("theme_global", "theme_bar", "set_palette", "set_fill_palette"))

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

get_phecode_description <- function(phecode_pattern, dictionary) {
  description <- dictionary %>%
    filter(feature_id == phecode_pattern) %>%
    pull(description)
  ifelse(length(description) == 0, "description not found", description)
}

get_legend_labels <- function(included_codes, dictionary) {
  dictionary %>%
    filter(feature_id %in% included_codes) %>%
    mutate(feature_label = paste0(feature_id, " | ", description)) %>%
    arrange(as.numeric(str_remove_all(feature_id, "[^0-9]"))) %>%
    pull(feature_label)
}

get_ordered_code_info <- function(included_codes, dictionary) {
  ordered_codes <- included_codes[order(as.numeric(str_replace_all(included_codes, "[^0-9.]", "")))]
  
  code_info <- dictionary %>%
    filter(feature_id %in% ordered_codes) %>%
    mutate(feature_label = paste0(feature_id, " | ", description)) %>%
    arrange(factor(feature_id, levels = ordered_codes))
  
  labels <- code_info$feature_label
  names(labels) <- code_info$feature_id
  
  colors <- setNames(RColorBrewer::brewer.pal(n = length(ordered_codes), name = "Set2"), ordered_codes)
  
  list(
    ordered_codes = ordered_codes,
    legend_labels = labels,
    color_mapping = colors
  )
}

plot_trends_v2 <- function(data_list, phecode_pattern, dictionary, y_var, y_label, title_suffix, sample_labels, custom_children = NULL) {
  phecode_description <- get_phecode_description(phecode_pattern, dictionary)
  
  if (!is.null(custom_children) && phecode_pattern %in% names(custom_children)) {
    included_codes <- c(phecode_pattern, custom_children[[phecode_pattern]])
  } else {
    included_codes <- dictionary %>%
      filter(
        str_detect(feature_id, phecode_pattern) &
          !str_detect(feature_id, "\\.\\d{2,}")
      ) %>%
      pull(feature_id)
  }
  
  code_info <- get_ordered_code_info(included_codes, dictionary)
  
  combined <- purrr::imap_dfr(data_list, function(df, key) {
    df %>% mutate(Sample = sample_labels[[key]])
  }) %>%
    filter(feature_id %in% code_info$ordered_codes) %>%
    mutate(
      is_parent = ifelse(feature_id == phecode_pattern, "Parent", "Child"),
      feature_id = factor(feature_id, levels = code_info$ordered_codes)
    )
  
  ggplot() +
    geom_line(data = combined,
              aes(x = year, y = !!sym(y_var), color = feature_id,
                  group = interaction(feature_id, Sample),
                  linetype = Sample,
                  size = is_parent)) +
    geom_point(data = combined,
               aes(x = year, y = !!sym(y_var), color = feature_id,
                   group = interaction(feature_id, Sample))) +
    scale_linetype_manual(values = setNames(c("solid", "dashed")[seq_along(data_list)], unname(sample_labels[seq_along(data_list)]))) +
    scale_color_manual(values = code_info$color_mapping, labels = code_info$legend_labels, name = "Code") +
    scale_size_manual(values = c("Parent" = 2, "Child" = 1), guide = "none") +
    labs(
      title = paste(title_suffix, "for Parent-Child Phecodes"),
      subtitle = paste0("<b><i>", phecode_pattern, "</i></b> | <i>", phecode_description, "</i>"),
      x = "",
      y = y_label
    ) +
    theme_global
}

plot_total_patients_v2 <- function(data_list, phecode_pattern, dictionary, sample_labels, custom_children = NULL) {
  # Step 1: Filter custom_children to only codes present in the data
  all_feature_ids <- unique(unlist(lapply(data_list, function(df) unique(df$feature_id))))
  if (!is.null(custom_children) && phecode_pattern %in% names(custom_children)) {
    valid_children <- intersect(custom_children[[phecode_pattern]], all_feature_ids)
    included_codes <- c(phecode_pattern, valid_children)
  } else {
    included_codes <- dictionary %>%
      filter(
        str_detect(feature_id, phecode_pattern) &
          !str_detect(feature_id, "\\.\\d{2,}")
      ) %>%
      pull(feature_id)
  }
  
  code_info <- get_ordered_code_info(included_codes, dictionary)
  
  # Step 2: Summarize total patients per code across all years
  all_data <- purrr::imap_dfr(data_list, function(data, key) {
    label <- sample_labels[[key]]
    data %>%
      filter(feature_id %in% code_info$ordered_codes) %>%
      group_by(feature_id) %>%
      summarise(Total_Patients = n_distinct(patient_num), .groups = "drop") %>%
      left_join(dictionary, by = "feature_id") %>%
      mutate(Dataset = label)
  })
  
  # Step 3: Clean up factor levels and axis setup
  codes_present <- unique(all_data$feature_id)
  ordered_codes <- code_info$ordered_codes[code_info$ordered_codes %in% codes_present]
  all_data$feature_id <- factor(all_data$feature_id, levels = ordered_codes)
  all_data <- all_data %>% mutate(x_pos = as.numeric(feature_id))
  x_levels <- levels(all_data$feature_id)
  x_breaks <- seq_along(x_levels)
  x_labels <- unname(code_info$legend_labels[x_levels])
  color_mapping <- code_info$color_mapping[x_levels]
  
  # Step 4: Get dataset names
  dataset_names <- unique(all_data$Dataset)
  if (length(dataset_names) != 2) {
    stop("plot_total_patients_v2 requires exactly 2 datasets.")
  }
  
  # Step 5: Plot
  ggplot(all_data, aes(x = x_pos, y = Total_Patients, fill = feature_id)) +
    geom_bar(data = all_data %>% filter(Dataset == dataset_names[2]),
             stat = "identity", position = position_identity(),
             width = 0.4, aes(x = x_pos + 0.1),
             linetype = "dashed", color = "black") +
    geom_bar(data = all_data %>% filter(Dataset == dataset_names[1]),
             stat = "identity", position = position_identity(),
             width = 0.4, aes(x = x_pos - 0.1),
             color = "black") +
    scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels
    ) +
    scale_fill_manual(
      values = color_mapping,
      labels = x_labels,
      name = "Code"
    ) +
    labs(x = "", y = "Total Patients Across All Years") +
    theme_bar
}

plot_total_patients_v2 <- function(data_list, phecode_pattern, dictionary, sample_labels, custom_children = NULL) {
  # Step 1: Determine which codes to include
  all_feature_ids <- unique(unlist(lapply(data_list, \(df) unique(df$feature_id))))
  if (!is.null(custom_children) && phecode_pattern %in% names(custom_children)) {
    valid_children <- intersect(custom_children[[phecode_pattern]], all_feature_ids)
    included_codes <- c(phecode_pattern, valid_children)
  } else {
    included_codes <- dictionary %>%
      filter(
        str_detect(feature_id, phecode_pattern) &
          !str_detect(feature_id, "\\.\\d{2,}")
      ) %>%
      pull(feature_id)
  }
  
  code_info <- get_ordered_code_info(included_codes, dictionary)
  
  # Step 2: Summarize total patients per code across all years
  all_data <- purrr::imap_dfr(data_list, function(data, key) {
    label <- sample_labels[[key]]
    data %>%
      filter(feature_id %in% code_info$ordered_codes) %>%
      group_by(feature_id) %>%
      summarise(Total_Patients = n_distinct(patient_num), .groups = "drop") %>%
      left_join(dictionary, by = "feature_id") %>%
      mutate(Dataset = label)
  })
  
  # Step 3: Set up plot variables
  codes_present <- unique(all_data$feature_id)
  ordered_codes <- code_info$ordered_codes[code_info$ordered_codes %in% codes_present]
  all_data$feature_id <- factor(all_data$feature_id, levels = ordered_codes)
  all_data <- all_data %>% mutate(x_pos = as.numeric(feature_id))
  x_levels <- levels(all_data$feature_id)
  x_breaks <- seq_along(x_levels)
  x_labels <- unname(code_info$legend_labels[x_levels])
  color_mapping <- code_info$color_mapping[x_levels]
  
  # Step 4: Plot depending on number of datasets
  dataset_names <- unique(all_data$Dataset)
  
  if (length(dataset_names) == 1) {
    # One dataset — simple bar plot
    ggplot(all_data, aes(x = feature_id, y = Total_Patients, fill = feature_id)) +
      geom_bar(stat = "identity", position = position_dodge(), color = "black", width = 0.6) +
      scale_fill_manual(
        values = color_mapping,
        labels = x_labels,
        name = "Code"
      ) +
      labs(x = "", y = "Total Patients Across All Years") +
      theme_bar
  } else if (length(dataset_names) == 2) {
    # Two datasets — offset bars with dashed outline
    ggplot(all_data, aes(x = x_pos, y = Total_Patients, fill = feature_id)) +
      geom_bar(data = all_data %>% filter(Dataset == dataset_names[2]),
               stat = "identity", position = position_identity(),
               width = 0.4, aes(x = x_pos + 0.1),
               linetype = "dashed", color = "black") +
      geom_bar(data = all_data %>% filter(Dataset == dataset_names[1]),
               stat = "identity", position = position_identity(),
               width = 0.4, aes(x = x_pos - 0.1),
               color = "black") +
      scale_x_continuous(
        breaks = x_breaks,
        labels = x_labels
      ) +
      scale_fill_manual(
        values = color_mapping,
        labels = x_labels,
        name = "Code"
      ) +
      labs(x = "", y = "Total Patients Across All Years") +
      theme_bar
  } else {
    stop("plot_total_patients_v2 supports only 1 or 2 datasets.")
  }
}


#' Analyze Parent-Child Code Hierarchies
#'
#' Evaluates hierarchical consistency for selected Phecodes by comparing trends for parent and child codes.
#' 
#' @importFrom dplyr n group_by summarise filter mutate select rename left_join case_when bind_rows n_distinct
#' @importFrom stringr str_detect str_replace str_replace_all str_remove_all
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_linetype_manual scale_color_manual scale_size_manual labs theme scale_fill_manual scale_x_continuous position_dodge
#' @importFrom tidyr pivot_longer
#' @importFrom purrr imap_dfr
#' @importFrom RColorBrewer brewer.pal
#' @importFrom rlang sym
#' 
#' @param data_inputs Named list of cleaned codified datasets.
#' @param dictionary Cleaned feature dictionary.
#' @param sample_labels Named list labeling each sample.
#' @param phecodes Character vector of parent Phecodes to analyze.
#' @param custom_children Optional named list of child codes for each parent.
#'
#' @return A list of plots for each Phecode, including `rate_plot` and `combined_plot`.
#' @export
analyze_code_hierarchy <- function(data_inputs, dictionary, sample_labels, phecodes, custom_children = NULL) {
  # Prepare codified data list using sample_labels
  codified_data_list <- list()
  if (!is.null(data_inputs$codified1)) codified_data_list[["1"]] <- data_inputs$codified1
  if (!is.null(data_inputs$codified2)) codified_data_list[["2"]] <- data_inputs$codified2
  
  # Generate Phecode summaries
  phecode_data <- lapply(codified_data_list, generate_phecode_summary)
  
  # Loop through Phecodes and generate plots
  plots <- lapply(phecodes, function(p) {
    rate_plot <- plot_trends_v2(phecode_data, p, dictionary, "Rate", "Rate", "Rates", sample_labels, custom_children)
    count_plot <- plot_trends_v2(phecode_data, p, dictionary, "Patients", "Patients per Year", "Patient Counts", sample_labels, custom_children)
    bar_plot <- plot_total_patients_v2(codified_data_list, p, dictionary, sample_labels, custom_children)
    
    combined_plot <- cowplot::plot_grid(
      count_plot + theme(legend.position = "none"),
      bar_plot + theme(legend.position = "none"),
      ncol = 2,
      rel_widths = c(3, 2)
    )
    
    list(phecode = p, rate_plot = rate_plot, combined_plot = combined_plot)
  })
  
  return(plots)
}