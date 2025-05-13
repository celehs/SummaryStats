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

### <ins>Key Functions and Parameters<ins> 

### <ins>Usage Examples<ins> 

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