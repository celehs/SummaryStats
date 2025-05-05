
# ----------------------- GLOBAL SETTINGS -----------------------

required_packages <- c(
  "dplyr", "stringr", "tidyr", "ggplot2", "gridExtra", 
  "ggtext", "knitr", "lubridate", "cowplot", "RColorBrewer", "purrr"
)

# Install any packages that are missing
installed <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)

if (length(to_install) > 0) {
  install.packages(to_install)
}

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))

# Set global themes
theme_global <- theme_minimal() + 
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_textbox_simple(size = 16, lineheight = 1.3),  
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

update_geom_defaults("line", list(linewidth = 1))
update_geom_defaults("point", list(size = 3))

theme_bar <- theme_minimal() +
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_textbox_simple(size = 16, lineheight = 1.3),  
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )

set_palette <- scale_color_brewer(palette = "Set2")
set_fill_palette <- scale_fill_brewer(palette = "Set2")

# ----------------------- MODULE 1 HELPERS -----------------------

clean_ONCE_data <- function(target_code, O2 = TRUE, path_code = NULL, path_nlp = NULL) {
  formatted_code <- gsub(":", "", target_code)
  formatted_code <- gsub("\\.", "_", formatted_code)
  
  if (O2) {
    path_code <- paste0("/n/data1/hsph/biostat/celehs/lab/sm731/ONCE_features/codified/", formatted_code, "_cod_features.csv")
    path_nlp <- paste0("/n/data1/hsph/biostat/celehs/lab/sm731/ONCE_features/NLP/", formatted_code, "_nlp_features.csv")
  } else {
    if (is.null(path_code) || is.null(path_nlp)) {
      stop("Must provide manual ONCE paths when O2 is FALSE.")
    }
  }
  
  code <- read.csv(path_code) %>%
    dplyr::select(Variable, Description, target_similarity) %>%
    rename(feature_id = Variable, description = Description) %>%
    mutate(description = tolower(description))
  
  nlp <- read.csv(path_nlp) %>%
    dplyr::select(cui, term, target_similarity) %>%
    rename(feature_id = cui, description = term) %>%
    mutate(description = tolower(description))
  
  return(list(code = code, nlp = nlp))
}

clean_raw_data <- function(df, date_col = "start_date", id_col = "feature_id") {
  df %>%
    mutate(
      patient_num = as.character(patient_num),
      !!date_col := as.character(.data[[date_col]]),
      year = ifelse(
        nchar(.data[[date_col]]) == 4,
        as.character(.data[[date_col]]),
        format(as.Date(.data[[date_col]]), "%Y")
      ),
      !!id_col := str_replace(.data[[id_col]], "CCS-PCS", "CCS")
    )
}

clean_data <- function(paths, date_col = "start_date", id_col = "feature_id") {
  input_data <- lapply(paths, read.csv)
  cleaned <- lapply(input_data, clean_raw_data, date_col = date_col, id_col = id_col)
  names(cleaned) <- names(paths)
  return(cleaned)
}

clean_dictionary <- function(dictionary_path) {
  read.csv(dictionary_path) %>%
    select(feature_id, description) %>%
    distinct() %>%
    mutate(
      description = case_when(
        str_detect(description, "forms of") ~ str_replace(description, "(.*)forms of.*", "\\1forms"),
        TRUE ~ str_replace_all(description, "\\s*\\(.*?\\)|,.*", "")
      ),
      description = tolower(description)
    )
}

# generate_sample_size_table <- function(data_inputs, sample_labels) {
#   sample_sizes <- lapply(names(data_inputs), function(name) {
#     data <- data_inputs[[name]]
#     data_type <- ifelse(grepl("nlp", name), "NLP", "Codified")
#     sample_key <- ifelse(grepl("2", name), "2", "1")
#     sample_label <- sample_labels[[sample_key]]
#     
#     data.frame(Sample = sample_label, Dataset = data_type, `Number of Patients` = length(unique(data$patient_num)))
#   })
#   
#   bind_rows(sample_sizes)
# }

# ----------------------- MODULE 2 HELPERS -----------------------

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
  combined_data <- bind_rows(
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
  combined_data <- bind_rows(
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
  combined_data <- bind_rows(summary_list)
  
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

# Final wrapper function
module2 <- function(data_inputs, target_code, target_cui, sample_labels) {
  
  # Total sample sizes
  sample_sizes <- lapply(names(data_inputs), function(name) {
    data <- data_inputs[[name]]
    data_type <- ifelse(grepl("nlp", name), "NLP", "Codified")
    sample_key <- ifelse(grepl("2", name), "2", "1")
    sample_label <- sample_labels[[sample_key]]
    data.frame(Sample = sample_label, Dataset = data_type,
               `Number of Patients` = length(unique(data$patient_num)))
  }) %>% bind_rows()
  
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

# ----------------------- MODULE 3 HELPERS ----------------------- 

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

get_legend_labels <- function(phecode_pattern, dictionary) {
  dictionary %>%
    filter(
      str_detect(feature_id, phecode_pattern) &
        !str_detect(feature_id, "\\.\\d{2,}")
    ) %>%
    mutate(feature_label = paste0(feature_id, " | ", description)) %>%
    arrange(as.numeric(str_remove_all(feature_id, "[A-Za-z:]"))) %>%
    pull(feature_label)
}

plot_trends_v2 <- function(data_list, phecode_pattern, dictionary, y_var, y_label, title_suffix, sample_labels) {
  phecode_description <- get_phecode_description(phecode_pattern, dictionary)
  legend_labels <- get_legend_labels(phecode_pattern, dictionary)
  
  combined <- purrr::imap_dfr(data_list, function(df, key) {
    df %>% mutate(Sample = sample_labels[[key]])
  }) %>%
    filter(str_detect(feature_id, phecode_pattern) & !str_detect(feature_id, "\\.\\d{2,}")) %>%
    mutate(is_parent = ifelse(feature_id == phecode_pattern, "Parent", "Child"))
  
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
    scale_color_brewer(palette = "Set2", labels = legend_labels) +
    scale_size_manual(values = c("Parent" = 2, "Child" = 1), guide = "none") +
    labs(
      title = paste(title_suffix, "for Parent-Child Phecodes"),
      subtitle = paste0("<b><i>", phecode_pattern, "</i></b> | <i>", phecode_description, "</i>"),
      x = "",
      y = y_label,
      color = "Code",
      linetype = "Sample"
    ) +
    theme_global
}

plot_total_patients_v2 <- function(data_list, phecode_pattern, dictionary, sample_labels) {
  all_data <- purrr::imap_dfr(data_list, function(data, key) {
    label <- sample_labels[[key]]
    data %>%
      filter(str_detect(feature_id, phecode_pattern) & !str_detect(feature_id, "\\.\\d{2,}")) %>%
      group_by(feature_id) %>%
      summarise(Total_Patients = n_distinct(patient_num), .groups = "drop") %>%
      left_join(dictionary, by = "feature_id") %>%
      mutate(feature_label = paste0(feature_id, " | ", description), Dataset = label)
  })
  
  labels <- unique(all_data$Dataset)
  
  ggplot(all_data, aes(x = feature_label, y = Total_Patients, fill = feature_label)) +
    geom_bar(data = all_data %>% filter(Dataset == labels[2]),
             stat = "identity", position = position_identity(),
             width = 0.4, aes(x = as.numeric(as.factor(feature_label)) + 0.1),
             linetype = "dashed", color = "black") +
    geom_bar(data = all_data %>% filter(Dataset == labels[1]),
             stat = "identity", position = position_identity(),
             width = 0.4, aes(x = as.numeric(as.factor(feature_label)) - 0.1),
             color = "black") +
    scale_x_continuous(breaks = unique(as.numeric(as.factor(all_data$feature_label))),
                       labels = unique(all_data$feature_label)) +
    labs(x = "", y = "Total Patients Across all Years") +
    scale_fill_brewer(palette = "Set2") +
    theme_bar
}

# Final wrapper function
module3 <- function(data_inputs, dictionary, sample_labels, phecodes) {
  # 3.a. Prepare codified data list using sample_labels
  codified_data_list <- list()
  if (!is.null(data_inputs$codified1)) codified_data_list[["1"]] <- data_inputs$codified1
  if (!is.null(data_inputs$codified2)) codified_data_list[["2"]] <- data_inputs$codified2
  
  # 3.b. Generate Phecode summaries
  phecode_data <- lapply(codified_data_list, generate_phecode_summary)
  
  # 3.c. Loop through Phecodes and generate plots
  plots <- lapply(phecodes, function(p) {
    rate_plot <- plot_trends_v2(phecode_data, p, dictionary, "Rate", "Rate", "Rates", sample_labels)
    count_plot <- plot_trends_v2(phecode_data, p, dictionary, "Patients", "Patients per Year", "Patient Counts", sample_labels)
    bar_plot <- plot_total_patients_v2(codified_data_list, p, dictionary, sample_labels)
    
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

# ----------------------- MODULE 4 HELPERS -----------------------

summarize_target_code_trends <- function(codified_list, nlp_list, target_code, target_cui, sample_labels) {
  keys <- names(codified_list)
  
  all_summaries <- map2(seq_along(codified_list), seq_along(nlp_list), function(i, j) {
    codified <- codified_list[[i]]
    nlp <- nlp_list[[j]]
    key <- keys[[i]]
    label <- sample_labels[[key]]
    
    nlp_target <- nlp[nlp$feature_id == target_cui, ]
    codified_nlp_target <- rbind(codified, nlp_target)
    
    phecode_cui_wide <- codified_nlp_target %>%
      group_by(year, patient_num, feature_id) %>%
      summarise(Code_Count = n(), .groups = "drop") %>%
      pivot_wider(names_from = feature_id, values_from = Code_Count, values_fill = 0)
    
    target_phecode_summary <- codified %>%
      filter(str_detect(feature_id, target_code)) %>%
      group_by(year) %>%
      summarise(PheCode_Patients = n_distinct(patient_num), .groups = "drop")
    
    target_cui_summary <- nlp %>%
      filter(str_detect(feature_id, target_cui)) %>%
      group_by(year) %>%
      summarise(CUI_Patients = n_distinct(patient_num), .groups = "drop")
    
    target_total_patients <- codified_nlp_target %>%
      group_by(year) %>%
      summarise(Total_Patients = n_distinct(patient_num), .groups = "drop")
    
    combined <- target_phecode_summary %>%
      left_join(target_cui_summary, by = "year") %>%
      left_join(target_total_patients, by = "year") %>%
      mutate(
        PheCode_Rate = PheCode_Patients / Total_Patients,
        CUI_Rate = CUI_Patients / Total_Patients,
        Sample = label
      )
    
    phecode_cui_wide$Sample <- label
    
    list(combined = combined, wide = phecode_cui_wide)
  })
  
  combined_summary <- bind_rows(map(all_summaries, "combined"))
  combined_wide <- bind_rows(map(all_summaries, "wide"))
  
  list(combined = combined_summary, wide = combined_wide)
}


plot_target_code_trends <- function(summary_data, target_code, target_cui, dictionary_code, dictionary_nlp) {
  phecode_description <- get_phecode_description(gsub("Phecode:", "", target_code), dictionary_code)
  
  cui_description <- dictionary_nlp %>%
    filter(feature_id == target_cui) %>%
    pull(description)
  cui_description <- ifelse(length(cui_description) > 0, cui_description, "description not found")
  
  subtitle_text <- paste0("<i><b>Target Code:</b> ", target_code, "</i> | <i>", phecode_description,
                          "<br><b>Target CUI:</b> ", target_cui, "</i> | <i>", cui_description, "</i>")
  
  plot_rates <- ggplot(summary_data, aes(x = as.numeric(year))) +
    geom_line(aes(y = PheCode_Rate, color = "Target Code", linetype = Sample)) +
    geom_line(aes(y = CUI_Rate, color = "Target CUI", linetype = Sample)) +
    geom_point(aes(y = PheCode_Rate, color = "Target Code"), shape = 16) +
    geom_point(aes(y = CUI_Rate, color = "Target CUI"), shape = 16) +
    labs(
      title = "Rates for Target Code and CUI",
      subtitle = subtitle_text,
      x = "", y = "Rate", color = "Code", linetype = "Sample"
    ) +
    theme_global + set_palette +
    scale_x_continuous(breaks = unique(as.numeric(summary_data$year)))
  
  plot_counts <- ggplot(summary_data, aes(x = as.numeric(year))) +
    geom_line(aes(y = PheCode_Patients, color = "Target Code", linetype = Sample)) +
    geom_line(aes(y = CUI_Patients, color = "Target CUI", linetype = Sample)) +
    geom_point(aes(y = PheCode_Patients, color = "Target Code"), shape = 16) +
    geom_point(aes(y = CUI_Patients, color = "Target CUI"), shape = 16) +
    # geom_line(aes(y = Total_Patients, group = Sample), linetype = "dashed", color = "black") +
    labs(
      title = "Patient Counts for Target Code and CUI",
      subtitle = subtitle_text,
      x = "", y = "Patients per Year", color = "Code", linetype = "Sample"
    ) +
    theme_global + set_palette +
    scale_x_continuous(breaks = unique(as.numeric(summary_data$year)))
  
  list(rates = plot_rates, counts = plot_counts)
}

plot_target_code_correlation <- function(wide_data, target_code, target_cui, summary_data, dictionary_code, dictionary_nlp) {
  phecode_description <- get_phecode_description(gsub("Phecode:", "", target_code), dictionary_code)
  
  cui_description <- dictionary_nlp %>%
    filter(feature_id == target_cui) %>%
    pull(description) %>%
    { if (length(.) > 0) . else "description not found" }
  
  subtitle_text <- paste0(
    "<i><b>Target Code:</b> ", target_code, "</i> | <i>", phecode_description,
    "<br><b>Target CUI:</b> ", target_cui, "</i> | <i>", cui_description, "</i>"
  )
  
  correlation_by_year <- wide_data %>%
    group_by(Sample, year) %>%
    summarise(Correlation = {
      data_year <- pick(everything())
      if (sd(data_year[[target_code]], na.rm = TRUE) == 0 || sd(data_year[[target_cui]], na.rm = TRUE) == 0) {
        NA
      } else {
        cor(data_year[[target_code]], data_year[[target_cui]], use = "pairwise.complete.obs", method = "spearman")
      }
    }, .groups = "drop") %>%
    filter(!is.na(Correlation))
  
  ggplot(correlation_by_year, aes(x = as.numeric(year), y = Correlation, group = Sample)) +
    geom_line(aes(linetype = Sample), color = "black") +
    geom_point(color = "black", shape = 16) +
    labs(
      title = "Intra-Patient Correlation for Target Code and CUI",
      subtitle = subtitle_text,
      x = "", y = "Correlation", linetype = "Sample"
    ) +
    theme_global +
    scale_x_continuous(breaks = unique(as.numeric(summary_data$year)))
}

# Final wrapper function

module4 <- function(data_inputs, target_code, target_cui, dictionary, sample_labels) {
  # 4.a. Build codified and NLP lists from sample labels
  codified_list <- list()
  nlp_list <- list()
  if (!is.null(data_inputs$codified1)) codified_list[["1"]] <- data_inputs$codified1
  if (!is.null(data_inputs$codified2)) codified_list[["2"]] <- data_inputs$codified2
  if (!is.null(data_inputs$nlp1)) nlp_list[["1"]] <- data_inputs$nlp1
  if (!is.null(data_inputs$nlp2)) nlp_list[["2"]] <- data_inputs$nlp2
  
  # 4.b. Summarize across codified and NLP
  summarized <- summarize_target_code_trends(
    codified_list = codified_list,
    nlp_list = nlp_list,
    target_code = target_code,
    target_cui = target_cui,
    sample_labels = sample_labels 
  )
  
  # 4.c. Plot trends across samples/institutions
  trend_plots <- plot_target_code_trends(
    summary_data = summarized$combined,
    target_code = target_code,
    target_cui = target_cui,
    dictionary_code = dictionary,
    dictionary_nlp = dictionary
  )
  
  # 4.d. Plot correlation across samples/institutions
  correlation_plot <- plot_target_code_correlation(
    wide_data = summarized$wide,
    target_code = target_code,
    target_cui = target_cui,
    summary_data = summarized$combined,
    dictionary_code = dictionary,
    dictionary_nlp = dictionary
  )
  
  return(list(
    rates_plot = trend_plots$rates,
    counts_plot = trend_plots$counts,
    correlation_plot = correlation_plot
  ))
}


# ----------------------- MODULE 5 HELPERS -----------------------

plot_related_code_trends_v2 <- function(Type, target_code, target_cui, codified_list, nlp_list, sample_labels, ONCE,
                                          type_dict = list("Diagnosis" = "PheCode", 
                                                           "Medication" = "RXNORM", 
                                                           "Lab" = c("LOINC","ShortName","Other Lab"), 
                                                           "Procedure" = "CCS")) {
    
  if (!(Type %in% c(names(type_dict), "CUI"))) return(list(NULL, paste("Invalid Type:", Type)))
  
  # Determine context
  is_cui <- (Type == "CUI")
  source_list <- if (is_cui) nlp_list else codified_list
  keyword <- if (!is_cui) type_dict[[Type]] else NULL
  target_feature <- if (is_cui) target_cui else target_code
  dict <- if (is_cui) ONCE$nlp else ONCE$code
  
  # Get top 5 similar features
  related_features <- dict %>%
    # filter((if (is_cui) TRUE else str_detect(feature_id, keyword)) & feature_id != target_feature) %>%
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
  
  # Total patient counts
  total_data <- purrr::imap_dfr(codified_list, function(df, key) {
    df %>%
      group_by(year) %>%
      summarise(Total_Patients = n_distinct(patient_num), .groups = "drop") %>%
      mutate(Sample = sample_labels[[key]])
  })
  
  combined_data <- long_data %>%
    left_join(total_data, by = c("year", "Sample")) %>%
    mutate(Rate = Patients / Total_Patients)
  
  # Description labeling
  descriptions <- dict %>%
    filter(feature_id %in% selected_features) %>%
    mutate(description_label = paste0(feature_id, " | ", description, " [", round(target_similarity, 3), "]")) %>%
    arrange(desc(target_similarity))
  
  combined_data <- combined_data %>%
    left_join(descriptions, by = "feature_id") %>%
    mutate(
      description_label = factor(description_label, levels = unique(descriptions$description_label)),
      is_target = ifelse(feature_id == target_feature, "Target", "Other")
    )
  
  # Bar plot data (complete grid)
  bar_data <- expand.grid(
    feature_id = selected_features,
    Sample = as.character(unname(sample_labels)),
    stringsAsFactors = FALSE
  ) %>%
    left_join(descriptions, by = "feature_id") %>%
    mutate(description_label = factor(description_label, levels = levels(combined_data$description_label)))
  
  bar_counts <- purrr::imap_dfr(source_list, function(df, key) {
    df %>%
      filter(feature_id %in% selected_features) %>%
      distinct(patient_num, feature_id) %>%
      group_by(feature_id) %>%
      summarise(Total_Patients = n(), .groups = "drop") %>%
      mutate(Sample = sample_labels[[key]])
  })
  
  bar_counts <- bar_counts %>% mutate(Sample = as.character(Sample))
  
  bar_data <- bar_data %>%
    left_join(bar_counts, by = c("feature_id", "Sample")) %>%
    mutate(Total_Patients = replace_na(Total_Patients, 0))
  
  target_desc <- descriptions$description[descriptions$feature_id == target_feature]
  subtitle_text <- paste0("<i><b>Target ", ":</b> ", target_feature, "</i> | <i>", target_desc, "</i>")
  
  # Color palette
  color_palette <- scale_color_brewer(palette = "Set2")
  fill_palette <- scale_fill_brewer(palette = "Set2")
  
  # Line plot (Rate)
  plot_rates <- ggplot(combined_data, aes(x = as.numeric(year), y = Rate,
                                          color = description_label, linetype = Sample,
                                          group = interaction(feature_id, Sample), size = is_target)) +
    geom_line() + geom_point(size = 3) +
    scale_size_manual(values = c("Target" = 2, "Other" = 1), guide = "none") +
    scale_linetype_manual(values = c("solid", "dashed")[seq_along(unique(combined_data$Sample))]) +
    color_palette +
    labs(title = paste("Rates for Related", Type, "Codes"),
         subtitle = subtitle_text,
         x = "", y = "Rate", color = Type, linetype = "Sample") +
    theme_global +
    scale_x_continuous(breaks = unique(as.numeric(combined_data$year)))
  
  # Line plot (Counts)
  plot_counts <- ggplot(combined_data, aes(x = as.numeric(year), y = Patients,
                                           color = description_label, linetype = Sample,
                                           group = interaction(feature_id, Sample), size = is_target)) +
    geom_line() + geom_point(size = 3) +
    scale_size_manual(values = c("Target" = 2, "Other" = 1), guide = "none") +
    scale_linetype_manual(values = c("solid", "dashed")[seq_along(unique(combined_data$Sample))]) +
    color_palette +
    labs(title = paste("Patient Counts for Related", Type, "Codes"),
         subtitle = subtitle_text,
         x = "", y = "Patients per Year", color = Type, linetype = "Sample") +
    theme_global +
    scale_x_continuous(breaks = unique(as.numeric(combined_data$year)))
  
  # Bar plot (no legend)
  bar_data <- bar_data %>% filter(Total_Patients > 0)
  
  if (is.null(sample_labels[["2"]])) {
    sample_labels[["2"]] <- 0
  }
  
  plot_bar <- ggplot(bar_data, aes(fill = description_label)) +
    geom_bar(data = bar_data %>% filter(Sample == sample_labels[["2"]]),
             aes(x = as.numeric(description_label) + 0.1, y = Total_Patients),
             stat = "identity", position = position_identity(), width = 0.4,
             color = "black", linetype = "dashed") +
    geom_bar(data = bar_data %>% filter(Sample == sample_labels[["1"]]),
             aes(x = as.numeric(description_label) - 0.1, y = Total_Patients),
             stat = "identity", position = position_identity(), width = 0.4,
             color = "black") +
    scale_x_continuous(breaks = unique(as.numeric(bar_data$description_label)),
                       labels = levels(bar_data$description_label)) +
    fill_palette +
    labs(y = "Total Patients Across All Years", x = "") +
    theme_bar +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none")
  
  
  # Combined count plot (line + bar side-by-side, no legend)
  combined_plot <- cowplot::plot_grid(
    plot_counts + theme(legend.position = "none"),
    plot_bar,
    ncol = 2,
    rel_widths = c(2, 1)
  )
  
  return(list(list(plot_rates, combined_plot), NULL))
}

# Final wrapper function
module5 <- function(data_inputs, target_code, target_cui, sample_labels,
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

