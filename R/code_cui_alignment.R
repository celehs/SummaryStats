utils::globalVariables(c("theme_global", "theme_bar", "set_palette", "set_fill_palette"))

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
      if (nrow(data_year) < 5 ||
          sd(data_year[[target_code]], na.rm = TRUE) == 0 || 
          sd(data_year[[target_cui]], na.rm = TRUE) == 0) {
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

#' Assess Code-CUI Temporal Alignment
#'
#' Analyzes agreement between a codified code and its corresponding CUI over time. Produces trends, patient counts, and correlation plots.
#' 
#' @importFrom dplyr group_by summarise mutate filter left_join pull n_distinct case_when select rename bind_rows
#' @importFrom purrr map2 map
#' @importFrom tidyr pivot_wider everything
#' @importFrom stringr str_detect str_replace str_replace_all str_remove_all
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme scale_x_continuous scale_color_manual scale_linetype_manual scale_size_manual
#' @importFrom stats cor sd
#' 
#' @param data_inputs Named list of codified and NLP datasets.
#' @param target_code The Phecode of interest.
#' @param target_cui The CUI of interest.
#' @param dictionary Cleaned feature dictionary.
#' @param sample_labels Named list labeling the samples.
#'
#' @return A list with plots: `rates_plot`, `counts_plot`, and `correlation_plot`.
#' @export
code_cui_alignment <- function(data_inputs, target_code, target_cui, dictionary, sample_labels) {
  
  # Build codified and NLP lists from sample labels
  codified_list <- list()
  nlp_list <- list()
  if (!is.null(data_inputs$codified1)) codified_list[["1"]] <- data_inputs$codified1
  if (!is.null(data_inputs$codified2)) codified_list[["2"]] <- data_inputs$codified2
  if (!is.null(data_inputs$nlp1)) nlp_list[["1"]] <- data_inputs$nlp1
  if (!is.null(data_inputs$nlp2)) nlp_list[["2"]] <- data_inputs$nlp2
  
  # Summarize across codified and NLP
  summarized <- summarize_target_code_trends(
    codified_list = codified_list,
    nlp_list = nlp_list,
    target_code = target_code,
    target_cui = target_cui,
    sample_labels = sample_labels 
  )
  
  # Plot trends across samples/institutions
  trend_plots <- plot_target_code_trends(
    summary_data = summarized$combined,
    target_code = target_code,
    target_cui = target_cui,
    dictionary_code = dictionary,
    dictionary_nlp = dictionary
  )
  
  # Plot correlation across samples/institutions
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