# SummaryStats R Package

## Overview

**SummaryStats** is an R package developed to support **exploratory summary** and **quality control** of electronic health record (EHR) data. It is capable of processing structured healthcare data such as **diagnoses**, **medications**, **procedures**, **labs**, and **concept identifiers (CUIs)** to intermediary files which supports downstream analysis. The package allows users to extract frequency counts of these codes across patients and time periods, and visualize trends through customizable summary plots.

This package has two major function modules:

1. **Quality Control (QC) Pipelines**: Processes both Codified and NLP-derived features by cleaning & formatting to facilitate downstream feature harmonization and similarity-based validation.
2. **Summary Statistics Function**:
   * **Main Summary Function**: Aggregates raw data, generates total and patient-specific counts, maps medical codes to their ontology descriptions, and visualizes these counts.
   * **Code Over Time Function**: Extends the primary capabilities by capturing and visualizing data over specified timeframes, enabling trend analysis and temporal insights.

## Features

- Aggregates data into structured, analyzable intermediary files.
- Provides summary statistics on medications, lab tests, procedures, diagnoses, and CUIs.
- Maps medical codes to standardized ontology descriptions.
- Customizable visualizations.
- Supports SQLite for efficient data storage and retrieval.

## Installation

Install directly from GitHub:
```R
devtools::install_github("celehs/SummaryStats")
```

Load the package in R:
```R
library(SummaryStats)
```
## Module 1: Quality Control Pipelines

### <ins>Key Functions and Parameters</ins>

#### 1. `clean_dictionary()`

- **Description**: Prepares a code-to-description mapping from a user-provided dictionary CSV. Cleans and deduplicates descriptions for consistency.
- **Parameters**:
  - `dictionary_path`: File path to the data dictionary.
- **Output**: A cleaned data dictionary with two columns: `feature_id` and `description`.

#### 2. `clean_ONCE_data()`

- **Description**: Loads ONCE-generated codified and NLP feature dictionaries based on a target phenotype. Used in Module 5 but initialized here for consistency.
- **Parameters**:
  - `target_code`: The codified feature ID of interest (e.g., `"PheCode:335"`).
  - `O2`: Logical flag for whether the script is running on O2 (default: `TRUE`).
  - `path_code`, `path_nlp`: (Optional) Manual file paths for ONCE codified and NLP CSVs if not using O2.
- **Output**: A list of two data frames: one for codified (`code`) and one for NLP (`nlp`), each including feature similarity scores.

#### 3. `plot_target_prevalence()`

- **Description**: Visualizes the annual trends and overall patient counts for a selected target code and CUI. Returns combined line and bar plots for both NLP and codified features.
- **Parameters**:
  - `data_inputs`: A named list of cleaned codified and NLP datasets from `clean_data()`.
  - `target_code`: The codified feature ID of interest (e.g., `"PheCode:335"`).
  - `target_cui`: The NLP-derived concept ID of interest (e.g., `"C0026769"`).
  - `sample_labels`: A named list labeling each sample (e.g., `"1"` = "Site A", `"2"` = "Site B").
- **Output**: A list containing three elements: `sample_sizes` (summary table), `nlp_plot` (CUI trends), and `codified_plot` (code trends).

#### 4. `analyze_code_hierarchy()`

- **Description**: Evaluates hierarchical consistency for selected Phecodes by comparing trends for parent and child codes. Supports automatic or user-defined child relationships.
- **Parameters**:
  - `data_inputs`: A named list of cleaned codified datasets.
  - `dictionary`: Cleaned feature dictionary.
  - `sample_labels`: A named list labeling each sample.
  - `phecodes`: A character vector of parent Phecodes to analyze.
  - `custom_children`: (Optional) Named list specifying child codes for each parent.
- **Output**: A list of plots for each parent Phecode, including `rate_plot` (line chart of prevalence) and `combined_plot` (line + bar charts of patient counts).

#### 5. `code_cui_alignment()`

- **Description**: Analyzes agreement between a codified code and its corresponding CUI over time. Produces prevalence trends, patient count plots, and intra-patient correlation metrics.
- **Parameters**:
  - `data_inputs`: Cleaned codified and NLP datasets from Module 1.
  - `target_code`: The Phecode of interest.
  - `target_cui`: The CUI of interest.
  - `dictionary`: Cleaned feature dictionary.
  - `sample_labels`: A named list labeling the samples.
- **Output**: A list with three plots: `rates_plot`, `counts_plot`, and `correlation_plot`, each comparing trends and alignment across samples.

#### 6. `plot_related_features()`

- **Description**: Identifies and visualizes trends for ONCE-derived features most similar to the target code and CUI, across multiple domains (e.g., diagnoses, labs, medications).
- **Parameters**:
  - `data_inputs`: Cleaned codified and NLP datasets from Module 1.
  - `target_code`: Target Phecode.
  - `target_cui`: Target CUI.
  - `sample_labels`: A named list labeling each sample.
  - `O2`: Logical flag for whether ONCE dictionaries are loaded from the O2 cluster.
  - `manual_ONCE_path_code`: File path to the ONCE codified dictionary (if `O2 = FALSE`).
  - `manual_ONCE_path_nlp`: File path to the ONCE NLP dictionary (if `O2 = FALSE`).
  - `types`: Character vector of clinical domains to evaluate (e.g., `"Diagnosis"`, `"Lab"`).
  - `type_dict`: A named list mapping domains to one or more feature prefixes.
- **Output**: A list of plot objects for each domain, including rate and count line plots and total patient bar plots for the most related features.

### <ins>Usage Examples</ins>

Below are example outputs from each module of the QC pipeline, generated using data from a single institution with multiple sclerosis as the target feature. Each function visualizes trends in codified and/or NLP-derived clinical features and supports quality control through interpretable plots.

#### **Visualizing target feature prevalence over time with `plot_target_prevalence()`**

```r
# Generate module 2 results
results_module2 <- plot_target_prevalence(data_inputs, target_code, target_cui, sample_labels)

# Display plots
print(results_module2$nlp_plot)
print(results_module2$codified_plot)
```

**NLP Plot:**

![](https://github.com/user-attachments/assets/f71ee965-0431-4704-b0e2-9ac08da11786)

**Codified Plot:**

![](https://github.com/user-attachments/assets/d40b939a-182e-4c9b-a03e-30750041c969)

#### **Evaluating parent-child code consistency with `analyze_code_hierarchy()`**

```r
# Generate module 3 results 
results_module3 <- analyze_code_hierarchy(data_inputs, dictionary, sample_labels,
                           phecodes = c("PheCode:411"),
                           custom_children = NULL)

# Display plots
for (res in results_module3) {
  print(res$rate_plot)
  print(res$combined_plot)
}

```

![](https://github.com/user-attachments/assets/4c09b6ea-3beb-41e9-b806-c06082790b6a)

---

#### **Comparing temporal alignment between codified and NLP data with `code_cui_alignment()`**

```r
# Generate module 4 results
results_module4 <- code_cui_alignment(data_inputs, target_code, target_cui, dictionary, sample_labels)

# Print plots
print(results_module4$rates_plot)
```

![](https://github.com/user-attachments/assets/297be072-196d-43da-8c8f-7babf4fceacd)


#### **Exploring ONCE-derived related features with `plot_related_features()`**

```r
# Generate module 5 results
results_module5 <- plot_related_features(data_inputs, target_code, target_cui, sample_labels, O2,
                           manual_ONCE_path_code = manual_ONCE_path_code,
                           manual_ONCE_path_nlp = manual_ONCE_path_nlp,
                           types = c("Diagnosis"),
                           type_dict = list("Diagnosis" = "PheCode"))

# Print plots
for (res in results_module5) {
  if (!is.null(res$line_plot)) print(res$line_plot)
  if (!is.null(res$combined_plot)) print(res$combined_plot)
}
```

![](https://github.com/user-attachments/assets/4e619caa-5aa9-470e-b51f-3acdf567df56)



## Module 2: Main Summary & Code-over-Time Function

### <ins>Key Functions and Parameters<ins>

#### 1. `generate_intermediary_sqlite`
- **Description**: Aggregates data from an SQLite database into a processed format.
- **Parameters**:
  - `con`: Connection to the source SQLite database.
  - `output_sqlite_path`: Path to save the aggregated intermediary SQLite database.
  - `time_column` (optional): Column name indicating time data for aggregation by year.

#### 2. `extract_data_for_visualization`
- **Description**: Extracts and processes data from the SQLite database for visualization.
- **Parameters**:
  - `sqlite_file`: Path to the intermediary SQLite file.
  - `prefix`: Medical code prefix (e.g., "RXNORM:").
  - `top_n`: Number of top occurrences to select (default 20).
  - `additional_vars` (optional): Additional variables to include.
  - `wanted_items_df` (optional): Specific items to extract.
  - `manual_replacement_bank` (optional): List for renaming codes.
  - `dict_prefix`: Dictionary prefix for mapping.
  - `dictionary_mapping`: Dictionary used for mapping codes.

#### 3. `plot_visualized_data`
- **Description**: Creates histograms for patient and total counts.
- **Parameters**:
  - `data`: List containing `Total_Counts` and `Patient_Counts`.
  - `count_column` (optional): Specify "Total_Count" or "Patient_Count".
  - `prefix`: Medical code prefix.
  - `description_label`: Dataset description label.
  - `output_path`: Path for saving plots.
  - `save_plots`: Logical flag to save plots (default FALSE).
  - `log_scale`: Logical flag for logarithmic scale on y-axis (default FALSE).

#### 4. `extract_patient_counts_over_years`
- **Description**: Extracts yearly patient counts for specified codes.
- **Parameters**:
  - `sqlite_file`: Path to intermediary SQLite database.
  - `codes_of_interest`: Codes to extract.
  - `dictionary_mapping`: Dictionary for code descriptions.

#### 5. `plot_patient_counts_over_time`
- **Description**: Line plots for patient counts over time.
- **Parameters**:
  - `data`: Data from `extract_patient_counts_over_years`.
  - `title`: Plot title.
  - `year_range` (optional): Year range for x-axis.
  - `output_path`: Directory for saving plot.
  - `save_plots`: Logical flag to save plot.
  - `auto_breaks`: Logical flag for non-uniform y-axis breaks.
  - `log_scale`: Logical flag for logarithmic y-axis scale.

### <ins>Usage Examples</ins>
#### Simulating Example Data & Dictionary

```R
library(RSQLite)

# Dictionary mapping
 dictionary_mapping <- data.frame(
   Group_Code = c("RXNORM:123", "RXNORM:456"),
   Common_Ontology_Code = c("RXNORM:001", "RXNORM:002"),
   Common_Ontology_Description = c("Acetaminophen", "Ibuprofen"),
   Group_Description = c("Pain Relievers", "NSAIDs")
 )

# Simulated EHR data
df_ehr <- data.frame(
  Year = sample(1995:2020, 1000, replace = TRUE),
  Patient = sample(1:500, 1000, replace = TRUE),
  Parent_Code = sample(c("RXNORM:123", "RXNORM:456", "RXNORM:789"), 1000, replace = TRUE),
  Count = sample(1:10, 1000, replace = TRUE)
)

# Creating temporary SQLite database
test_db_path <- tempfile()
test_db <- dbConnect(SQLite(), test_db_path)
dbWriteTable(test_db, 'df_monthly', df_ehr, overwrite = TRUE)
```

### <ins>Generating Intermediary SQLite Database<ins>

#### Without Year Aggregation
```R
intermediary_test_db_path <- tempfile()
generate_intermediary_sqlite(test_db, output_sqlite_path = intermediary_test_db_path)
```

#### With Year Aggregation
```R
intermediary_test_db_path <- tempfile()
generate_intermediary_sqlite(test_db, output_sqlite_path = intermediary_test_db_path, time_column = "Year")
```

### <ins>Data Extraction<ins>

#### Main Function Example (without year) 
- Datafile contains two tables: Total_Counts and Patient_Counts
```R
data_null <- extract_data_for_visualization(
  sqlite_file = intermediary_test_db_path,
  prefix = "RXNORM:",
  top_n = 20,
  dictionary_mapping = dictionary_mapping
)
head(data_null$Patient_Counts, 3)
```
![image](https://github.com/user-attachments/assets/9870fe03-449c-44a4-9d9d-a3d97f11827a)


#### Codes Over Time Example (with year) 
- Datafile contains n+1(n=the number of codes of interest) tables: one for each code of interest (here n=3) and combined table. All the counts are in patient counts.
```R
data_test <- extract_patient_counts_over_years(
  sqlite_file = intermediary_test_db_path,
  codes_of_interest = c("RXNORM:123","RXNORM:456","RXNORM:789"),
  dictionary_mapping = dictionary_mapping
)
head(data_test$`RXNORM:123`,3)
head(data_test$combined,6)
```
![image](https://github.com/user-attachments/assets/11e2d998-d46e-4da9-917d-f52afa34d52a)

![image](https://github.com/user-attachments/assets/145d3681-bd98-4a01-aec3-f7601daf93d3)


### <ins>Data Visualization - Main<ins>
Here we use real dataset of RA (Rheumatoid Arthritis) in monthly counts, which contains information on 6,131 patients. And we load a comprehensive mapping dictionary that includes accurate corresponding Common Ontology Description and Group Description to the Code.

#### Commonly used Scenario: Top 20 
```R
data_rxnorm <- extract_data_for_visualization(
    sqlite_file = "./RA_intermediary.sqlite",
    prefix = "RXNORM:",
    top_n = 20,
    dictionary_mapping = dictionary_mapping
  )

plot_visualized_data(
  data = data_rxnorm,
  count_column = "Patient_Count",  
  prefix = "RXNORM:",
  description_label = "RXNORM Medications"
)
```
![image](https://github.com/user-attachments/assets/9a6e9fd7-fba7-42cf-9f88-69e35e2cc4d3)

#### Additional variables in additional to default of Top 20 
```R
additional_vars <- c("Erythrocyte sedimentation rate", "C reactive protein", 
                     "Rheumatoid factor")

data_add <- extract_data_for_visualization(
  sqlite_file = "./RA_intermediary.sqlite", 
  prefix = "LOINC:", 
  dictionary_mapping = dictionary_mapping,
  additional_vars = additional_vars
 )
}

plot_visualized_data(
  data = data_add,
  count_column = "Patient_Count",  
  prefix = "LOINC:",
  description_label = "Lab Tests"
)
```
![image](https://github.com/user-attachments/assets/7fecab69-1bc5-4779-a77a-b3f8dfdaf1d6)

#### Input of a list of items with categories 
```R
wanted_items <- data.frame(
  Type = rep("MED", 26),
  Category = c(rep("Non-biologics DMARDs", 10),
               rep("Biologics DMARDs", 10),
               rep("Targeted synthetic DMARDs", 3),
               rep("Glucocorticoids", 3)),
  Name = c('Azathioprine', 'Cyclophosphamide', 'Hydroxychloroquine', 
           'Leflunomide', 'Methotrexate', 'Sulfasalazine', 'Gold', 
           'Penicillamine', 'Chloroquine', 'Cyclosporine', 
           'Adalimumab', 'Certolizumab pegol', 'Etanercept', 
           'Golimumab', 'Infliximab', 'Abatacept', 'Anakinra', 
           'Rituximab', 'Sarilumab', 'Tocilizumab', 
           'Baricitinib', 'Tofacitinib', 'Upadacitinib', 
           'Methylprednisolone', 'Prednisone', 'Dexamethasone')
)

data_want <- extract_data_for_visualization(
  sqlite_file = "./RA_intermediary.sqlite", 
  prefix = "RXNORM:", 
  dictionary_mapping = dictionary_mapping,
  wanted_items_df = wanted_items
 )

plot_visualized_data(
  data = data_want,
  prefix = "RXNORM:",
  description_label = "RA Medications"
)
```
![image](https://github.com/user-attachments/assets/22dcb47a-5a90-4c67-9f87-bcb8ad15956d)
![image](https://github.com/user-attachments/assets/ceec6813-cd25-414e-9508-7ede23b2df1e)


### <ins>Data Visualization - Code Over Time<ins>
Here we also use real dataset of RA (Rheumatoid Arthritis) in monthly counts with a comprehensive dictionary.

#### Three codes of interests: "PheCode:714.1", "RXNORM:5487" and "RXNORM:6851"
```R
output_data <- extract_patient_counts_over_years(
  sqlite_file = "./RA_intermediary_time.sqlite",
  codes_of_interest = c("PheCode:714.1", "RXNORM:5487", "RXNORM:6851"),
  dictionary_mapping = dictionary_mapping
  )

plot_patient_counts_over_time(
  data = summary_stats_data_year$combined,
  title = "RA Trends (2000–2020)",
  year_range = c(2000, 2020),
  auto_breaks = FALSE,
  log_scale = TRUE,
  save_plot = FALSE,
  output_path = tempdir()
)
```
![image](https://github.com/user-attachments/assets/5bf7866d-6ec5-4b71-b93f-561a6a521b1f)


## Detailed Documentation and Vignettes

- [Summary Stats Main Function Vignette](https://github.com/celehs/SummaryStats/blob/main/vignettes/summary_stats.html)
- [Codes Over Years Vignette](https://github.com/celehs/SummaryStats/blob/main/vignettes/any_codes_over_year.html)

## Dependencies

- R (>= 3.5.0)
- Essential R libraries: `data.table`, `dplyr`, `ggplot2`, `Matrix`, `RSQLite`, and more.


**License:** GPL-3 

## Shiny App Setup and Deployment - Co-occurrence checking

### Required Files
The Shiny app requires a dictionary mapping file to function properly. Due to its large size (>200MB), this file is not included in the repository.

#### Dictionary File Setup
1. Download the dictionary mapping file (`dictionary_mapping_v3.4.tsv`) from one of these sources:
   - O2 Server: `/n/data1/hsph/biostat/celehs/lab/datasets/dictionaries/dictionary_mapping_v3.4.tsv`
   - Dropbox: [Download dictionary_mapping_v3.4.tsv.gz](https://www.dropbox.com/scl/fi/smrb7mm9hmlgk6ss4symi/dictionary_mapping_v3.4.tsv.gz?dl=0&e=1&rlkey=80b97b4894wwsn3u50b91jtz2&st=6druitgo)
   
   Note: If downloading from Dropbox, you'll need to decompress the .gz file after downloading.

2. Place the file in the `shiny-server` directory of your cloned repository:
   ```
   SummaryStats/
   └── shiny-server/
       ├── app.R
       └── dictionary_mapping_v3.4.tsv  # Place the downloaded file here
   ```
3. This location is required for the Docker setup to work correctly - the app will not function without the dictionary file in this exact location

### Running with Docker
Once you have placed the dictionary file in the correct location, you can run the app using Docker:

```bash
docker compose up --build shiny
```

The app will be available at http://localhost:3838

Note: The Docker setup will only work if the dictionary file is present in the correct location (`shiny-server/dictionary_mapping_v3.4.tsv`). 
