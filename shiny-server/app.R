library(shiny)
library(DBI)
library(RSQLite)
library(DT)
library(shinyjs)
library(ggplot2)
library(data.table)
library(tools)
library(gridExtra)
library(Matrix)
library(stringr)

# Set maximum upload size to 1GB for co-occurrence, 2GB for intermediary
options(shiny.maxRequestSize = 2048 * 1024^2)

# Load dictionary at startup
dictionary_path <- "dictionary_mapping_v3.4.tsv"  # File should be in the same directory as app.R
code_dictionary <- data.table::fread(dictionary_path, sep = "\t")

# Keep only the first occurrence of each Group_Code
code_dictionary <- code_dictionary[!duplicated(Group_Code), .(Group_Code, Group_Description)]

# Define prefix mappings for code normalization (case-insensitive)
prefix_mappings <- list(
  "ccs" = "CCS",
  "icd" = "ICD",
  "icd9" = "ICD9",
  "icd10" = "ICD10",
  "loinc" = "LOINC",
  "ndc" = "NDC",
  "phecode" = "PheCode",
  "rxnorm" = "RXNORM",
  "snomed" = "SNOMED"
)

# Helper function to validate and load input files
validate_input_file <- function(df_input) {
  fpath <- df_input$datapath
  fext <- tools::file_ext(df_input$name)
  
  data_obj <- switch(fext,
    csv = {
      df <- data.table::fread(fpath, sep = ",", data.table = FALSE)
      # Check if columns exist and rename if needed
      if (ncol(df) >= 3) {
        names(df)[1:3] <- c("V1", "V2", "value")
      }
      df
    },
    tsv = {
      df <- data.table::fread(fpath, sep = "\t", data.table = FALSE)
      # Check if columns exist and rename if needed
      if (ncol(df) >= 3) {
        names(df)[1:3] <- c("V1", "V2", "value")
      }
      df
    },
    gz = {
      df <- data.table::fread(fpath, data.table = FALSE)
      # Check if columns exist and rename if needed
      if (ncol(df) >= 3) {
        names(df)[1:3] <- c("V1", "V2", "value")
      }
      df
    },
    rda = {
      loaded_obj <- get(load(fpath))
      if (inherits(loaded_obj, "dgCMatrix")) {
        sparse_to_cooccurrence(loaded_obj)
      } else {
        loaded_obj
      }
    },
    Rdata = {
      loaded_obj <- get(load(fpath))
      if (inherits(loaded_obj, "dgCMatrix")) {
        sparse_to_cooccurrence(loaded_obj)
      } else {
        loaded_obj
      }
    },
    rds = {
      loaded_obj <- readRDS(fpath)
      if (inherits(loaded_obj, "dgCMatrix")) {
        sparse_to_cooccurrence(loaded_obj)
      } else {
        loaded_obj
      }
    },
    sqlite = fpath,
    db = fpath,
    sqlite3 = fpath,
    validate("Invalid file; Please upload a .csv, .tsv, .gz, .rda, .Rdata, .rds, or SQLite file")
  )
  
  # For non-SQLite files, ensure the data has the correct structure
  if (!is.character(data_obj)) {
    if (is.data.frame(data_obj)) {
      # Ensure the data frame has the required columns
      if (!all(c("V1", "V2", "value") %in% names(data_obj))) {
        stop("Data must contain columns: V1, V2, and value")
      }
    } else {
      stop("Unsupported data format. Please provide a data frame, dgCMatrix, or SQLite file.")
    }
  }
  
  return(data_obj)
}

# Helper function to convert sparse matrix to co-occurrence data frame
sparse_to_cooccurrence <- function(sparse_mat) {
  # Convert sparse matrix to triplet form
  coords <- which(sparse_mat != 0, arr.ind = TRUE)
  values <- sparse_mat[coords]
  
  # Create data frame with V1, V2, and value columns
  data.frame(
    V1 = rownames(sparse_mat)[coords[,1]],
    V2 = colnames(sparse_mat)[coords[,2]],
    value = values
  )
}

# Helper function to normalize code format
normalize_code <- function(code) {
  # Split code into prefix and value
  parts <- strsplit(code, ":")[[1]]
  if (length(parts) != 2) return(code)  # Return original if no prefix found
  
  prefix <- tolower(parts[1])
  value <- parts[2]
  
  # Check if prefix exists in mappings
  for (mapping in names(prefix_mappings)) {
    if (grepl(mapping, prefix, ignore.case = TRUE)) {
      return(paste0(prefix_mappings[[mapping]], ":", value))
    }
  }
  
  return(code)  # Return original if no mapping found
}

# Helper function to get description from dictionary
get_code_description <- function(code) {
  # Normalize the code format
  normalized_code <- normalize_code(code)
  
  # Look up in dictionary
  description <- code_dictionary[Group_Code == normalized_code, Group_Description]
  
  if (length(description) == 0) {
    return(NA_character_)
  } else {
    return(description)
  }
}

# Helper function to convert to sparse matrix format
build_spm_cooc <- function(df_cooc, uniq_codes) {
  # Get indices and remove any rows with NA matches
  i <- match(df_cooc$V1, uniq_codes)
  j <- match(df_cooc$V2, uniq_codes)
  valid_idx <- !is.na(i) & !is.na(j)
  
  if (!any(valid_idx)) {
    # If no valid matches, return empty sparse matrix
    return(Matrix::sparseMatrix(i = integer(0), j = integer(0), 
                               dims = c(length(uniq_codes), length(uniq_codes))))
  }
  
  Matrix::sparseMatrix(
    i = i[valid_idx],
    j = j[valid_idx],
    x = df_cooc$value[valid_idx],
    triangular = TRUE,
    check = FALSE,
    dims = c(length(uniq_codes), length(uniq_codes))
  )
}

# Helper function to get co-occurrences
get_cooccurrences <- function(data_obj, code_of_interest, limit = 20) {
  # Normalize the code format
  normalized_code <- normalize_code(code_of_interest)
  
  # If data_obj is a file path (SQLite), use database query
  if (is.character(data_obj)) {
    con <- dbConnect(RSQLite::SQLite(), data_obj)
    on.exit(dbDisconnect(con))
    
    query <- sprintf("
      SELECT 
        CASE 
          WHEN V1 = '%s' THEN V2 
          ELSE V1 
        END as paired_code,
        CAST(SUM(value) as INTEGER) as total_count
      FROM df_monthly
      WHERE V1 = '%s' OR V2 = '%s'
      GROUP BY paired_code
      ORDER BY total_count DESC
      LIMIT %d
    ", normalized_code, normalized_code, normalized_code, limit)
    
    result <- dbGetQuery(con, query)
  } else {
    # For data frames, convert to sparse matrix for efficient processing
    uniq_codes <- unique(c(data_obj$V1, data_obj$V2))
    spm <- build_spm_cooc(data_obj, uniq_codes)
    
    # Get the row/column index for the code of interest
    code_idx <- match(normalized_code, uniq_codes)
    
    # Get co-occurrences (combine both row and column values)
    cooc_values <- spm[, code_idx] + spm[code_idx, ]
    
    # Convert to dense vector and get indices
    dense_values <- as.vector(cooc_values)
    ordered_indices <- order(dense_values, decreasing = TRUE)
    
    # Create result data frame
    result <- data.frame(
      paired_code = uniq_codes[ordered_indices],
      total_count = dense_values[ordered_indices]
    )
    
    # Remove zero values and limit results
    result <- result[result$total_count > 0, ]
    result <- head(result, limit)
  }
  
  return(result)
}

# Helper function to get co-occurrence between two specific codes
get_pair_cooccurrence <- function(data_obj, code1, code2) {
  # Normalize the codes
  code1 <- normalize_code(code1)
  code2 <- normalize_code(code2)
  
  if (is.character(data_obj)) {
    # SQLite database
    con <- dbConnect(RSQLite::SQLite(), data_obj)
    on.exit(dbDisconnect(con))
    
    query <- sprintf("
      SELECT CAST(SUM(value) as INTEGER) as total_count
      FROM df_monthly
      WHERE (V1 = '%s' AND V2 = '%s') OR (V1 = '%s' AND V2 = '%s')
    ", code1, code2, code2, code1)
    
    result <- dbGetQuery(con, query)
    return(as.numeric(result$total_count))
  } else {
    # Data frame
    count <- sum(data_obj$value[
      (data_obj$V1 == code1 & data_obj$V2 == code2) |
      (data_obj$V1 == code2 & data_obj$V2 == code1)
    ])
    return(count)
  }
}

# Helper function to create subset of data for top codes
create_data_subset <- function(data_obj, codes) {
  # Normalize all codes
  codes <- sapply(codes, normalize_code)
  
  if (is.character(data_obj)) {
    # SQLite database
    con <- dbConnect(RSQLite::SQLite(), data_obj)
    on.exit(dbDisconnect(con))
    
    # Create a temporary table with the codes we're interested in
    codes_str <- paste0("'", codes, "'", collapse = ",")
    query <- sprintf("
      CREATE TEMP TABLE temp_codes AS
      SELECT * FROM df_monthly
      WHERE V1 IN (%s) OR V2 IN (%s)
    ", codes_str, codes_str)
    
    dbExecute(con, query)
    result <- dbGetQuery(con, "SELECT * FROM temp_codes")
    dbExecute(con, "DROP TABLE temp_codes")
    return(result)
  } else {
    # Data frame
    return(data_obj[
      (data_obj$V1 %in% codes) | (data_obj$V2 %in% codes),
    ])
  }
}

# Helper function to build complete co-occurrence matrix
build_cooccurrence_matrix <- function(data_obj, codes) {
  if (is.character(data_obj)) {
    # For SQLite database
    con <- dbConnect(RSQLite::SQLite(), data_obj)
    on.exit(dbDisconnect(con))
    
    # Create a temporary table with the codes we're interested in
    codes_str <- paste0("'", codes, "'", collapse = ",")
    query <- sprintf("
      SELECT V1, V2, value FROM df_monthly
      WHERE (V1 IN (%s) AND V2 IN (%s))
    ", codes_str, codes_str)
    
    temp_data <- dbGetQuery(con, query)
    
    # Convert to sparse matrix
    spm <- build_spm_cooc(temp_data, codes)
    # Convert to dense matrix for plotting
    matrix_data <- as.matrix(spm + t(spm))
    diag(matrix_data) <- diag(matrix_data)/2  # Fix diagonal values that were doubled
    
  } else {
    # For data frames, use sparse matrix directly
    filtered_data <- data_obj[data_obj$V1 %in% codes & data_obj$V2 %in% codes, ]
    spm <- build_spm_cooc(filtered_data, codes)
    # Convert to dense matrix for plotting
    matrix_data <- as.matrix(spm + t(spm))
    diag(matrix_data) <- diag(matrix_data)/2  # Fix diagonal values that were doubled
  }
  
  # Ensure row and column names are set
  rownames(matrix_data) <- codes
  colnames(matrix_data) <- codes
  
  return(matrix_data)
}

# Helper function to get available codes from data
get_available_codes <- function(data_obj, prefix = NULL) {
  if (is.character(data_obj)) {
    # SQLite database
    con <- dbConnect(RSQLite::SQLite(), data_obj)
    on.exit(dbDisconnect(con))
    
    if (!is.null(prefix)) {
      query <- sprintf("
        SELECT DISTINCT V1 as code FROM df_monthly 
        WHERE V1 LIKE '%s%%'
        UNION
        SELECT DISTINCT V2 as code FROM df_monthly 
        WHERE V2 LIKE '%s%%'
        ORDER BY code
      ", prefix, prefix)
    } else {
      query <- "
        SELECT DISTINCT V1 as code FROM df_monthly 
        UNION
        SELECT DISTINCT V2 as code FROM df_monthly 
        ORDER BY code
      "
    }
    
    result <- dbGetQuery(con, query)
    return(result$code)
  } else {
    # Data frame
    if (!is.null(prefix)) {
      codes <- unique(c(
        data_obj$V1[grepl(paste0("^", prefix), data_obj$V1, ignore.case = TRUE)],
        data_obj$V2[grepl(paste0("^", prefix), data_obj$V2, ignore.case = TRUE)]
      ))
    } else {
      codes <- unique(c(data_obj$V1, data_obj$V2))
    }
    return(sort(codes))
  }
}

# Helper function to format code with description
format_code_with_desc <- function(code) {
  description <- get_code_description(code)
  if (!is.na(description) && nchar(description) > 30) {
    description <- paste0(substr(description, 1, 30), "[..]")
  }
  if (!is.na(description)) {
    return(paste0(code, " (", description, ")"))
  } else {
    return(code)
  }
}

# UI definition
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Code Co-occurrence Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Upload and Search"),
      div(
        style = "background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; padding: 16px; margin-bottom: 18px;",
        fileInput("sqlite_file", "Upload Co-occurrence Matrix File",
                  accept = c(".sqlite", ".db", ".sqlite3", ".csv", ".tsv", ".gz", ".rda", ".Rdata", ".rds")),
        tags$p("Max file size: 1GB for co-occurrence matrix.", style = "margin: 2px 0 0 0; color: #888; font-size: 13px;")
      ),
      div(
        style = "background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; padding: 16px; margin-bottom: 18px;",
        h4("(Optional) Upload Intermediary File for Patient/Total Counts", style = "margin-top: 0;"),
        fileInput("intermediary_file", "Upload Intermediary File",
                  accept = c(".sqlite", ".db", ".sqlite3")),
        tags$p("Max file size: 2GB for intermediary file.", style = "margin: 2px 0 0 0; color: #888; font-size: 13px;"),
        tags$p(
          "This file should reflect the counts of each code per patient. It must include the following columns: Patient, Parent_Code, Count. For more info, see ",
          tags$a(href = "https://github.com/celehs/SummaryStats", target = "_blank", "SummaryStats GitHub"), ".",
          style = "margin: 4px 0 8px 0; color: #888; font-size: 13px;"
        )
      ),
      selectizeInput("code_input", "Select Code of Interest",
                    choices = NULL,
                    options = list(
                      placeholder = "Select a code...",
                      create = FALSE,
                      maxOptions = 1000,
                      openOnFocus = TRUE,
                      selectOnTab = TRUE,
                      render = I("{\n                        option: function(item, escape) {\n                          return '<div>' + escape(item.label) + '</div>';\n                        }\n                      }")
                    )),
      helpText("Supported prefixes: CCS, LOINC, NDC, PheCode, RXNORM.. (case-insensitive)"),
      numericInput("num_codes", "Number of Top Codes to Show",
                  value = 20, min = 1, max = 100, step = 1),
      actionButton("search_btn", "Search Co-occurrences", 
                  class = "btn-primary"),
      hr(),
      h4("Instructions"),
      tags$ul(
        tags$li("Upload your data file (max 1GB) for co-occurrence matrix"),
        tags$li("(Optional) Upload intermediary file for patient/total counts (max 2GB)"),
        tags$li("Select the code you want to analyze from the dropdown"),
        tags$li("Select number of top codes to show"),
        tags$li("Click 'Search Co-occurrences' to see results")
      ),
      width = 3
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.search_btn > 0",
        h3("Top Co-occurring Codes"),
        textOutput("normalized_code"),
        downloadButton("download_pdf", "Download PDF Report", 
                      class = "btn-primary"),
        br(),
        br(),
        tabsetPanel(
          tabPanel("Table",
                   DTOutput("results_table"),
                   br(),
                   downloadButton("download_csv", "Download Results as CSV")
          ),
          tabPanel("Histogram",
                   h4("Co-occurrence Visualization", style = "margin-bottom: 10px;"),
                   div(
                     style = "overflow-x: auto; width: 100%;",
                     plotOutput("cooccurrence_plot", width = "1000px", height = "600px")
                   )
          ),
          tabPanel("Heatmap",
                   h4("Co-occurrence Heatmap"),
                   plotOutput("cooccurrence_heatmap", height = "800px")
          )
        )
      ),
      width = 9
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive value to store the data object
  data_obj <- reactive({
    req(input$sqlite_file)
    
    # Check if file exists
    if (!file.exists(input$sqlite_file$datapath)) {
      showNotification("Error: File not found", type = "error")
      return(NULL)
    }
    
    # Check file size (1GB limit)
    file_size_gb <- file.size(input$sqlite_file$datapath) / (1024^3)
    if (file_size_gb > 1) {
      showNotification("Error: File size exceeds 1GB limit", type = "error")
      return(NULL)
    }
    
    # Validate and load the file
    tryCatch({
      data_obj <- validate_input_file(input$sqlite_file)
      if (tools::file_ext(input$sqlite_file$name) %in% c("sqlite", "db", "sqlite3")) {
        return(data_obj)  # Return file path for SQLite
      } else {
        return(data_obj)  # Return loaded data for other formats
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Reactive for intermediary file (optional)
  intermediary_db <- reactive({
    if (is.null(input$intermediary_file)) return(NULL)
    if (!file.exists(input$intermediary_file$datapath)) return(NULL)
    # Check file size (2GB limit)
    file_size_gb <- file.size(input$intermediary_file$datapath) / (1024^3)
    if (file_size_gb > 2) {
      showNotification("Error: Intermediary file size exceeds 2GB limit", type = "error")
      return(NULL)
    }
    return(input$intermediary_file$datapath)
  })
  
  # Helper to get patient/total counts for a vector of codes
  get_patient_total_counts <- function(db_path, codes) {
    if (is.null(db_path)) return(NULL)
    con <- dbConnect(RSQLite::SQLite(), db_path)
    on.exit(dbDisconnect(con))
    codes_str <- paste0("'", codes, "'", collapse = ",")
    query <- sprintf(
      "SELECT Parent_Code, SUM(Count) AS Total_Count, COUNT(DISTINCT Patient) AS Patient_Count FROM processed_data WHERE Parent_Code IN (%s) GROUP BY Parent_Code",
      codes_str
    )
    res <- dbGetQuery(con, query)
    return(res)
  }
  
  # Reactive value to store the code of interest
  code_of_interest <- reactive({
    req(input$code_input)
    input$code_input
  })
  
  # Reactive value to store the normalized code
  normalized_code <- reactive({
    req(code_of_interest())
    normalize_code(code_of_interest())
  })
  
  # Reactive value to store the number of codes to show
  num_codes <- reactive({
    req(input$num_codes)
    input$num_codes
  })
  
  # Show the normalized code to the user
  output$normalized_code <- renderText({
    if (normalized_code() != code_of_interest()) {
      paste("Searching for:", normalized_code())
    }
  })
  
  # Reactive value to store the query results
  results <- eventReactive(input$search_btn, {
    req(data_obj(), normalized_code(), num_codes())
    
    tryCatch({
      get_cooccurrences(data_obj(), normalized_code(), num_codes())
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Reactive for patient/total counts for the codes in results
  code_counts <- reactive({
    req(results())
    db_path <- intermediary_db()
    if (is.null(db_path)) return(NULL)
    codes <- results()$paired_code
    get_patient_total_counts(db_path, codes)
  })
  
  # Render the results table
  output$results_table <- renderDT({
    req(results())
    
    # Add description column
    results_with_desc <- results()
    results_with_desc$description <- sapply(results_with_desc$paired_code, get_code_description)
    
    counts <- code_counts()
    
    if (is.null(counts)) {
      # Only co-occurrence matrix: original logic
      col_order <- c("paired_code", "description", "total_count")
      results_with_desc <- results_with_desc[, col_order, drop = FALSE]
      results_with_desc <- results_with_desc[order(-results_with_desc$total_count, na.last = TRUE), , drop = FALSE]
      # Set co-occurrence count to NA for target code row and move to top
      idx_target <- which(results_with_desc$paired_code == normalized_code())
      if (length(idx_target) == 1) {
        results_with_desc$total_count[idx_target] <- NA
        target_row <- results_with_desc[idx_target, ]
        rest <- results_with_desc[-idx_target, , drop = FALSE]
        results_with_desc <- rbind(target_row, rest)
      }
      datatable(
        results_with_desc,
        options = list(
          pageLength = 20,
          searching = TRUE,
          ordering = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          order = list(),
          columnDefs = list(
            list(
              targets = 0,  # Code column
              width = '100px'
            ),
            list(
              targets = 1,  # Description column
              width = '240px',
              render = JS('function(data, type, row) { if (type === "display") { return "<div style=\'white-space: normal; word-wrap: break-word;\'>" + data + "</div>"; } return data; }')
            ),
            list(targets = 2, width = '120px')  # Co-occurrence
          ),
          rowCallback = JS(sprintf(
            'function(row, data, index) {
              if(data[0] === "%s") {
                $(row).css({"background-color": "#fff3cd"});
              }
            }', normalized_code()))
        ),
        extensions = 'Buttons',
        rownames = FALSE,
        colnames = c("Code", "Description", "Co-occurrence Count")
      )
    } else {
      # Intermediary file present: enhanced logic
      results_with_desc <- merge(results_with_desc, counts, by.x = "paired_code", by.y = "Parent_Code", all.x = TRUE)
      col_order <- c("paired_code", "description", "Patient_Count", "Total_Count", "total_count")
      results_with_desc <- results_with_desc[, col_order, drop = FALSE]
      # Always show target code as first row, rest sorted by co-occurrence count
      target_code <- normalized_code()
      idx_target <- which(results_with_desc$paired_code == target_code)
      if (length(idx_target) == 1) {
        # Blank co-occurrence count for target code
        results_with_desc$total_count[idx_target] <- NA
        # Split and sort
        target_row <- results_with_desc[idx_target, ]
        rest <- results_with_desc[-idx_target, ]
        if (nrow(rest) > 0) {
          rest <- rest[order(-rest$total_count, na.last = TRUE), , drop = FALSE]
        }
        results_with_desc <- rbind(target_row, rest)
      }
      datatable(
        results_with_desc,
        options = list(
          pageLength = 20,
          searching = TRUE,
          ordering = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          order = list(),
          columnDefs = list(
            list(
              targets = 0,  # Code column
              width = '100px'
            ),
            list(
              targets = 1,  # Description column
              width = '240px',
              render = JS('function(data, type, row) { if (type === "display") { return "<div style=\'white-space: normal; word-wrap: break-word;\'>" + data + "</div>"; } return data; }')
            ),
            list(targets = c(2, 3, 4), width = '120px')  # Patient, Total, Co-occurrence
          ),
          rowCallback = JS(sprintf(
            'function(row, data, index) {
              if(data[0] === "%s") {
                $(row).css({"background-color": "#fff3cd"});
              }
            }', normalized_code()))
        ),
        extensions = 'Buttons',
        rownames = FALSE,
        colnames = c("Code", "Description", "Patient Count", "Total Count", "Co-occurrence Count")
      )
    }
  })
  
  # Render the histogram plot
  output$cooccurrence_plot <- renderPlot({
    req(results())
    
    plot_data <- results()
    plot_data$label <- plot_data$paired_code
    create_histogram(plot_data)
  })
  
  # Reactive value to store the complete co-occurrence matrix
  cooccurrence_matrix <- reactive({
    req(results(), data_obj())
    
    # Get all codes (code of interest + top co-occurring codes)
    all_codes <- c(normalized_code(), results()$paired_code)
    
    # Build the complete co-occurrence matrix
    build_cooccurrence_matrix(data_obj(), all_codes)
  })
  
  # Render the heatmap
  output$cooccurrence_heatmap <- renderPlot({
    req(cooccurrence_matrix())
    
    # Convert to long format for ggplot
    heatmap_long <- reshape2::melt(cooccurrence_matrix())
    names(heatmap_long) <- c("Var1", "Var2", "value")
    heatmap_long$log_value <- log10(heatmap_long$value + 1)
    
    # Create the heatmap with target code
    create_heatmap(heatmap_long, normalized_code())
  })
  
  # Download handler for CSV export
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("cooccurrence_results_", normalized_code(), "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(results(), file, row.names = FALSE)
    }
  )
  
  # Function to create the histogram plot
  create_histogram <- function(plot_data) {
    # Add formatted labels using the correct code description
    plot_data$formatted_label <- sapply(plot_data$paired_code, format_code_with_desc)
    
    ggplot(plot_data, aes(x = reorder(formatted_label, -total_count), y = total_count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        plot.margin = margin(t = 10, r = 20, b = 60, l = 60, unit = "pt"),
        axis.title.x = element_text(margin = margin(t = 60, unit = "pt")),
        axis.text = element_text(size = 10),
        plot.title = element_text(margin = margin(b = 10)),
        plot.subtitle = element_text(margin = margin(b = 10))
      ) +
      labs(x = "Paired Code", y = "Total Co-occurrence Count") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      coord_cartesian(clip = "off")  # Prevent clipping of labels
  }
  
  # Function to create the heatmap plot
  create_heatmap <- function(heatmap_long, target_code) {
    # Add formatted labels
    heatmap_long$formatted_Var1 <- sapply(as.character(heatmap_long$Var1), format_code_with_desc)
    heatmap_long$formatted_Var2 <- sapply(as.character(heatmap_long$Var2), format_code_with_desc)
    
    # Get all codes except target code
    other_codes <- setdiff(unique(c(heatmap_long$Var1, heatmap_long$Var2)), target_code)
    
    # Create distance matrix for hierarchical clustering
    dist_matrix <- matrix(0, nrow = length(other_codes), ncol = length(other_codes))
    rownames(dist_matrix) <- colnames(dist_matrix) <- other_codes
    
    # Fill distance matrix with co-occurrence values
    for (i in 1:length(other_codes)) {
      for (j in 1:length(other_codes)) {
        code1 <- other_codes[i]
        code2 <- other_codes[j]
        # Get the co-occurrence value, defaulting to 0 if not found
        cooc_value <- heatmap_long$value[
          (heatmap_long$Var1 == code1 & heatmap_long$Var2 == code2) |
          (heatmap_long$Var1 == code2 & heatmap_long$Var2 == code1)
        ]
        dist_matrix[i, j] <- ifelse(length(cooc_value) > 0, cooc_value[1], 0)
      }
    }
    
    # Convert to distance matrix (using negative correlation as distance)
    dist_matrix <- 1 - cor(dist_matrix)
    dist_matrix[is.na(dist_matrix)] <- 1
    
    # Perform hierarchical clustering
    hc <- hclust(as.dist(dist_matrix), method = "complete")
    ordered_other_codes <- other_codes[hc$order]
    
    # Put target code first, then add clustered codes
    ordered_codes <- c(target_code, ordered_other_codes)
    all_formatted <- sapply(as.character(ordered_codes), format_code_with_desc)
    
    # Set factor levels
    heatmap_long$formatted_Var1 <- factor(heatmap_long$formatted_Var1, levels = all_formatted)
    heatmap_long$formatted_Var2 <- factor(heatmap_long$formatted_Var2, levels = all_formatted)
    
    # Determine if we should show cell values
    n_codes <- length(unique(c(heatmap_long$Var1, heatmap_long$Var2)))
    show_cell_values <- n_codes <= 30
    
    # Create the heatmap
    p <- ggplot(heatmap_long, aes(x = formatted_Var1, y = formatted_Var2, fill = log_value)) +
      geom_tile(color = "white", linewidth = 0.1) +  # Add thin white borders
      scale_fill_gradientn(
        colors = c("#F0F8FF", "#E6F3FF", "#CCE6FF", "#99CCFF", "#66B2FF", "#3399FF", "#0080FF", "#0066CC", "#004C99", "#003366"),
        breaks = c(0, 1, 2, 3, 4, 5),
        labels = c("1", "10", "100", "1k", "10k", "100k"),
        name = "Co-occurrence Count\n(log scale)",
        limits = c(min(heatmap_long$log_value[heatmap_long$value > 0]), 
                   max(heatmap_long$log_value)),
        na.value = "#F0F8FF"  # Use same light blue for NA values
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(hjust = 1),
        legend.position = "right",
        legend.key.height = unit(1.5, "cm"),
        panel.grid.major = element_blank(),  # Remove grid lines
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(color = "red", size = 11, face = "bold")  # Make note red and larger
      ) +
      labs(x = "Code", y = "Code",
           subtitle = "Note: Numbers shown are in thousands (k)") +
      coord_fixed()
    
    # Add cell values if conditions are met
    if (show_cell_values) {
      p <- p + geom_text(
        data = subset(heatmap_long, value > 0),
        aes(label = round(value/1000, 0)),
        size = 3,
        color = "black"
      )
    }
    
    return(p)
  }
  
  # PDF download handler
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("cooccurrence_report_", normalized_code(), "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Create a temporary directory to store the plots
      temp_dir <- tempdir()
      
      plot_data <- results()
      plot_data$formatted_label <- sapply(plot_data$paired_code, format_code_with_desc)
      hist_plot <- create_histogram(plot_data)
      
      heatmap_long <- reshape2::melt(cooccurrence_matrix())
      names(heatmap_long) <- c("Var1", "Var2", "value")
      heatmap_long$log_value <- log10(heatmap_long$value + 1)
      heatmap_plot <- create_heatmap(heatmap_long, normalized_code())
      
      # Add descriptions to results for PDF
      results_with_desc <- results()
      results_with_desc$description <- sapply(results_with_desc$paired_code, get_code_description)
      
      counts <- code_counts()
      target_code <- normalized_code()
      idx_target <- which(results_with_desc$paired_code == target_code)
      
      if (is.null(counts)) {
        # Only co-occurrence matrix: original logic
        col_order <- c("paired_code", "description", "total_count")
        results_with_desc <- results_with_desc[, col_order, drop = FALSE]
        results_with_desc <- results_with_desc[order(-results_with_desc$total_count, na.last = TRUE), , drop = FALSE]
        # Set co-occurrence count to NA for target code row and move to top
        idx_target <- which(results_with_desc$paired_code == normalized_code())
        if (length(idx_target) == 1) {
          results_with_desc$total_count[idx_target] <- NA
          target_row <- results_with_desc[idx_target, ]
          rest <- results_with_desc[-idx_target, , drop = FALSE]
          results_with_desc <- rbind(target_row, rest)
        }
        colnames(results_with_desc) <- c("Code", "Description", "Co-occurrence\nCount")
        # Wrap description
        results_with_desc$Description <- stringr::str_wrap(results_with_desc$Description, width = 35)
        highlight_row <- NULL
      } else {
        # Intermediary file present: enhanced logic
        results_with_desc <- merge(results_with_desc, counts, by.x = "paired_code", by.y = "Parent_Code", all.x = TRUE)
        col_order <- c("paired_code", "description", "Patient_Count", "Total_Count", "total_count")
        results_with_desc <- results_with_desc[, col_order, drop = FALSE]
        # Always show target code as first row, rest sorted by co-occurrence count
        idx_target <- which(results_with_desc$paired_code == target_code)
        if (length(idx_target) == 1) {
          results_with_desc$total_count[idx_target] <- NA
          target_row <- results_with_desc[idx_target, ]
          rest <- results_with_desc[-idx_target, ]
          if (nrow(rest) > 0) {
            rest <- rest[order(-rest$total_count, na.last = TRUE), , drop = FALSE]
          }
          results_with_desc <- rbind(target_row, rest)
          highlight_row <- 1
        } else {
          highlight_row <- NULL
        }
        colnames(results_with_desc) <- c("Code", "Description", "Patient\nCount", "Total\nCount", "Co-occurrence\nCount")
        # Wrap description
        results_with_desc$Description <- stringr::str_wrap(results_with_desc$Description, width = 35)
      }
      
      # Create PDF in A4 landscape format
      pdf(file, width = 11.69, height = 8.27)  # A4 landscape dimensions in inches
      
      # Add title page
      grid::grid.newpage()
      grid::grid.text(
        paste("Co-occurrence Analysis Report for", normalized_code()),
        x = 0.5, y = 0.7, gp = grid::gpar(fontsize = 20)
      )
      grid::grid.text(
        paste("Generated on", format(Sys.Date(), "%B %d, %Y")),
        x = 0.5, y = 0.6, gp = grid::gpar(fontsize = 14)
      )
      
      # Add table with wrapped text and highlight if needed
      grid::grid.newpage()
      grid::grid.text("Top Co-occurring Codes", x = 0.5, y = 0.95, gp = grid::gpar(fontsize = 16))
      
      table_theme <- gridExtra::ttheme_minimal(
        core = list(
          fg_params = list(hjust = 0, x = 0.1),
          bg_params = list(fill = c("white", "grey95"))
        ),
        colhead = list(
          fg_params = list(hjust = 0, x = 0.1)
        )
      )
      table_grob <- gridExtra::tableGrob(
        results_with_desc,
        rows = NULL,
        theme = table_theme
      )
      # Highlight the target code row if needed
      if (!is.null(highlight_row)) {
        table_grob$grobs[table_grob$layout$name == paste0("core-", highlight_row, "-1") | 
                         table_grob$layout$name == paste0("core-", highlight_row, "-2") |
                         table_grob$layout$name == paste0("core-", highlight_row, "-3") |
                         table_grob$layout$name == paste0("core-", highlight_row, "-4") |
                         table_grob$layout$name == paste0("core-", highlight_row, "-5") ] <-
          lapply(table_grob$grobs[table_grob$layout$name == paste0("core-", highlight_row, "-1") | 
                                  table_grob$layout$name == paste0("core-", highlight_row, "-2") |
                                  table_grob$layout$name == paste0("core-", highlight_row, "-3") |
                                  table_grob$layout$name == paste0("core-", highlight_row, "-4") |
                                  table_grob$layout$name == paste0("core-", highlight_row, "-5") ],
                 function(g) {g$gp <- grid::gpar(fill = "#fff3cd"); g})
      }
      # Adjust column widths
      ncol_tbl <- ncol(results_with_desc)
      if (ncol_tbl == 5) {
        table_grob$widths <- unit(c(0.16, 0.28, 0.12, 0.12, 0.12), "npc")
      } else if (ncol_tbl == 3) {
        table_grob$widths <- unit(c(0.15, 0.35, 0.2), "npc")
      }
      grid::grid.draw(table_grob)
      
      # Add histogram
      print(hist_plot)
      
      # Add heatmap
      print(heatmap_plot)
      
      dev.off()
    }
  )
  
  # Update code input choices when data is loaded
  observe({
    req(data_obj())
    
    # Get all available codes
    all_codes <- get_available_codes(data_obj())
    
    # Update selectize input
    updateSelectizeInput(
      session, 
      "code_input",
      choices = all_codes,
      selected = character(0),
      server = TRUE,
      options = list(
        openOnFocus = TRUE,
        selectOnTab = TRUE
      )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
