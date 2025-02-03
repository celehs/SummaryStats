assign_colors_to_categories <- function(categories) {
  unique_categories <- unique(categories)
  color_palette <- scales::hue_pal()(length(unique_categories))
  names(color_palette) <- unique_categories
  return(color_palette)
}

# Helper function to map additional variables to Group Code and Description, with prefix handling
map_additional_variables <- function(additional_vars, dictionary_mapping, prefix) {
  mapped_items <- list()
  
  for (var_name in additional_vars) {
    # Search for Group_Description in the dictionary
    mapped_item <- dictionary_mapping %>%
      filter(Group_Description == var_name & grepl(paste0("^", prefix), Group_Code)) %>%
      select(Group_Code, Group_Description) %>%
      distinct()
    
    if (nrow(mapped_item) > 0) {
      group_code <- mapped_item$Group_Code[1]
      cleaned_code <- sub(".*:", "", group_code)  # Remove prefix for cleaner display
      label <- paste0(var_name, " (", cleaned_code, ")")
      mapped_items[[var_name]] <- list(Group_Code = group_code, Name = label)
    } else {
      # Assign fallback for missing variables
      mapped_items[[var_name]] <- list(Group_Code = NA, Name = var_name)
    }
  }
  
  return(mapped_items)
}

# Helper function to map descriptions and truncate names
map_descriptions <- function(data, prefix, dictionary_mapping, dict_prefix) {
  # Extract mapping for Group_Code
  code_dict_group <- dictionary_mapping %>%
    filter(grepl(paste0("^", dict_prefix), Group_Code)) %>%
    select(Group_Code, Group_Description)
  code_map_group <- setNames(code_dict_group$Group_Description, sub(dict_prefix, "", code_dict_group$Group_Code))
  
  # Extract mapping for Common_Ontology_Code
  code_dict_common <- dictionary_mapping %>%
    filter(grepl(paste0("^", prefix), Common_Ontology_Code)) %>%
    select(Common_Ontology_Code, Group_Description)
  code_map_common <- setNames(code_dict_common$Group_Description, sub(prefix, "", code_dict_common$Common_Ontology_Code))
  
  # Map descriptions with priority order
  data$Name <- sapply(data$Parent_Code, function(code) {
    code_number <- sub(prefix, "", code)
    description <- if (!is.na(code_map_group[code_number])) {
      code_map_group[code_number]
    } else if (!is.na(code_map_common[code_number])) {
      code_map_common[code_number]
    } else {
      code_number  # Default to code_number if no match
    }
    # Handle NA in description safely
    if (!is.na(description) && nchar(description) > 30) {
      description <- paste0(substr(description, 1, 30), "[..]")
    }
    paste0(description, " (", code_number, ")")
  })
  
  return(data)
}

# Helper function to map wanted items based on dictionary
map_items <- function(wanted_items_df, dictionary_mapping) {
  selected_items <- list()
  wanted_items_df$Name <- tolower(wanted_items_df$Name)  # Normalize wanted items to lowercase
  
  dictionary_mapping <- dictionary_mapping %>%
    mutate(
      Group_Description = tolower(Group_Description),  # Normalize Group Description
      Common_Ontology_Description = tolower(Common_Ontology_Description)  # Normalize Common Ontology Description
    )
  
  for (i in 1:nrow(wanted_items_df)) {
    type <- wanted_items_df$Type[i]
    category <- wanted_items_df$Category[i]
    group_description <- wanted_items_df$Name[i]
    
    # Match normalized descriptions
    mapped_item <- dictionary_mapping %>%
      filter(
        Type == type & 
          (Group_Description == group_description | Common_Ontology_Description == group_description)
      ) %>%
      select(Group_Code, Group_Description) %>%
      distinct()
    
    if (nrow(mapped_item) == 1) {
      group_code <- mapped_item$Group_Code
      selected_items[[group_code]] <- list(Description = group_description, Category = category)
    } else {
      selected_items[[paste0("NoCode:", group_description)]] <- list(Description = group_description, Category = category)
    }
  }
  return(selected_items)
}

# Helper function to fetch top N codes by Total_Count within a prefix
fetch_top_n_codes <- function(con, prefix, batch_size, top_n) {
  # Construct the query to fetch relevant rows
  query <- sprintf("SELECT Patient, Parent_Code, Count FROM processed_data WHERE Parent_Code LIKE '%s%%'", prefix)
  
  # Send the query
  res <- dbSendQuery(con, query)
  
  # Initialize variables for storing results
  all_patient_data <- list()
  
  # Fetch data in batches
  while (!dbHasCompleted(res)) {
    df_batch <- dbFetch(res, n = batch_size)  # Fetch `batch_size` rows
    if (nrow(df_batch) > 0) {
      dt_batch <- as.data.table(df_batch)
      patient_counts <- dt_batch[, .(Count = sum(Count)), by = .(Patient, Parent_Code)]
      all_patient_data <- append(all_patient_data, list(patient_counts))
    }
  }
  
  # Clear the query result
  dbClearResult(res)
  
  # Combine all fetched batches
  combined_data <- rbindlist(all_patient_data, fill = TRUE)
  
  # Aggregate Total_Count and Patient_Count by Parent_Code
  combined_data_grouped <- combined_data[, .(
    Total_Count = sum(Count),
    Patient_Count = length(unique(Patient))
  ), by = Parent_Code]
  
  # Get top N codes by Total_Count
  top_n_codes <- combined_data_grouped[order(-Total_Count)][1:top_n]
  
  return(top_n_codes)
}

fetch_data_for_items <- function(con, selected_items, batch_size) {
  # Construct the query for fetching only the desired Parent_Code values
  query <- paste0(
    "SELECT Patient, Parent_Code, Count FROM processed_data WHERE Parent_Code IN ('",
    paste(names(selected_items), collapse = "', '"), "')"
  )
  
  # Send the query
  res <- dbSendQuery(con, query)
  all_patient_data <- list()
  
  # Fetch data in batches dynamically
  while (!dbHasCompleted(res)) {
    df_batch <- dbFetch(res, n = batch_size)  # Fetch `batch_size` rows
    if (nrow(df_batch) > 0) {
      dt_batch <- as.data.table(df_batch)
      patient_counts <- dt_batch[, .(Count = sum(Count)), by = .(Patient, Parent_Code)]
      all_patient_data <- append(all_patient_data, list(patient_counts))
    }
  }
  
  # Clear the query result
  dbClearResult(res)
  
  # Combine all fetched data
  combined_data <- rbindlist(all_patient_data, fill = TRUE)
  
  # Add zero counts for items not found
  for (code in names(selected_items)) {
    if (!code %in% combined_data$Parent_Code) {
      combined_data <- rbind(combined_data, data.table(Patient = NA, Parent_Code = code, Count = 0))
    }
  }
  
  return(combined_data)
}

# Helper function to combine top N codes and additional variables data
process_additional_vars <- function(con, top_n_codes, additional_vars, prefix, dict_prefix, dictionary_mapping, batch_size, top_n) {
  # Fetch top N rows
  top_n_rows <- map_descriptions(top_n_codes, prefix, dictionary_mapping, dict_prefix)
  
  # Fetch additional variables and their mapped codes
  mapped_additional_vars <- map_additional_variables(additional_vars, dictionary_mapping, dict_prefix)
  
  # Initialize a data.table to store results for additional variables
  additional_rows <- rbindlist(
    lapply(names(mapped_additional_vars), function(var) {
      var_info <- mapped_additional_vars[[var]]
      group_code <- var_info$Group_Code
      
      if (!is.na(group_code)) {
        # Get all corresponding Common_Ontology_Code for the Group_Description
        ontology_codes <- dictionary_mapping %>%
          filter(Group_Description == var & grepl(paste0("^", prefix), Common_Ontology_Code)) %>%
          pull(Common_Ontology_Code)
        
        # Fetch matching rows from the dataset
        if (length(ontology_codes) > 0) {
          query <- paste0(
            "SELECT Patient, Parent_Code, SUM(Count) AS Total_Count ",
            "FROM processed_data WHERE Parent_Code IN ('",
            paste(ontology_codes, collapse = "', '"),
            "') GROUP BY Patient, Parent_Code"
          )
          matching_data <- dbGetQuery(con, query)
          
          # Deduplicate patient counts
          unique_patients <- matching_data %>%
            group_by(Patient) %>%
            summarise(Distinct_Patient = n_distinct(Patient))
          
          total_count <- sum(matching_data$Total_Count, na.rm = TRUE)
          patient_count <- nrow(unique_patients)
          
          # Return the row
          return(data.table(
            Parent_Code = group_code,  # Use Group_Code as Parent_Code
            Total_Count = total_count,
            Patient_Count = patient_count,
            Name = var_info$Name,  # Correctly formatted Name
            Code = NA  # Leave Code column empty
          ))
        }
      }
      
      # Return a row with 0 counts if no match is found
      return(data.table(
        Parent_Code = group_code,
        Total_Count = 0,
        Patient_Count = NA,
        Name = var_info$Name,
        Code = NA
      ))
    }), fill = TRUE
  )
  
  # Combine top N and additional variables
  combined_data <- rbindlist(list(top_n_rows, additional_rows), use.names = TRUE, fill = TRUE)
  
  return(combined_data)
}

# Helper function to create summary data for Total Count and Patient Count
create_summary_data <- function(combined_data, selected_items, count_type) {
  if (count_type == "Total_Count") {
    # Summarize total counts
    df_wide <- dcast(combined_data, Patient ~ Parent_Code, value.var = "Count", fun.aggregate = sum, fill = 0)
    code_totals <- colSums(df_wide[, -1, with = FALSE], na.rm = TRUE)
    
    code_summary <- data.frame(Code = names(code_totals), Total_Count = code_totals)
    code_summary$Name <- sapply(code_summary$Code, function(code) {
      if (code %in% names(selected_items)) {
        item_info <- selected_items[[code]]
        description <- item_info$Description
        code_number <- ifelse(grepl("NoCode:", code), description, sub(".*:", "", code))
        paste0(description, " (", code_number, ")")
      } else {
        code
      }
    })
    code_summary$Category <- sapply(code_summary$Code, function(code) {
      if (code %in% names(selected_items)) {
        selected_items[[code]]$Category
      } else {
        NA
      }
    })
  } else if (count_type == "Patient_Count") {
    # Summarize patient counts
    combined_data[, Presence := 1]
    df_binary <- dcast(combined_data, Patient ~ Parent_Code, value.var = "Presence", fun.aggregate = function(x) as.integer(length(x) > 0), fill = 0)
    patient_counts <- colSums(df_binary[, -1, with = FALSE], na.rm = TRUE)
    
    code_summary <- data.frame(Code = names(patient_counts), Patient_Count = patient_counts)
    code_summary$Name <- sapply(code_summary$Code, function(code) {
      if (code %in% names(selected_items)) {
        item_info <- selected_items[[code]]
        description <- item_info$Description
        code_number <- ifelse(grepl("NoCode:", code), description, sub(".*:", "", code))
        paste0(description, " (", code_number, ")")
      } else {
        code
      }
    })
    code_summary$Category <- sapply(code_summary$Code, function(code) {
      if (code %in% names(selected_items)) {
        selected_items[[code]]$Category
      } else {
        NA
      }
    })
  }
  
  return(code_summary)
}

# Helper function to create bar plot with optional log scaling
create_histogram_plot <- function(data, count_column, x_label, y_label, plot_title, category_colors = NULL, save_path = NULL, save_plots = FALSE, log_scale = FALSE) {
  # Check if Category column is in the data
  has_category <- "Category" %in% names(data)
  
  # Dynamically use the count column in `reorder`
  p <- ggplot(data, aes(x = reorder(Name, -data[[count_column]]), y = data[[count_column]], fill = if (has_category) Category else NULL)) +
    geom_bar(stat = "identity") +
    labs(
      title = plot_title,
      x = x_label,
      y = y_label,
      fill = if (has_category) "Category" else NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
      axis.text.y = element_text(size = 15),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      plot.title = element_text(size = 20),
      plot.margin = unit(c(1, 1, 1, 4), "cm"),
      legend.position = if (has_category) c(0.95, 0.95) else "none",
      legend.justification = c(1, 1),
      legend.background = element_rect(fill = alpha('white', 0.8)),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14)
    ) +
    scale_y_continuous(labels = scales::comma)
  
  # Apply log scaling to y-axis if specified
  if (log_scale) {
    p <- p + scale_y_log10()
  }
  
  # Conditionally add color scale if category colors are provided and Category exists
  if (has_category && !is.null(category_colors)) {
    p <- p + scale_fill_manual(values = category_colors)
  }
  
  # Save plot if required
  if (save_plots && !is.null(save_path)) {
    ggsave(filename = save_path, plot = p, width = 18, height = 14, dpi = 300)
  }
  
  return(p)
}

# Consolidated plotting function
plot_summary_data <- function(data, count_column, title_label, x_axis_label, output_path, save_plots, log_scale) {
  create_histogram_plot(
    data = data,
    count_column = count_column,
    x_label = x_axis_label,
    y_label = ifelse(count_column == "Total_Count", "Total Count", "Patient Count"),
    plot_title = paste(title_label, ifelse(count_column == "Total_Count", "(Total Counts)", "(Patient Counts)")),
    category_colors = if ("Category" %in% names(data)) assign_colors_to_categories(unique(data$Category)) else NULL,
    save_path = file.path(output_path, paste0(title_label, "_", count_column, ".png")),
    save_plots = save_plots,
    log_scale = log_scale && count_column == "Total_Count"  # Apply log scale only for Total Count
  )
}