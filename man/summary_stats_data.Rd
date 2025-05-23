% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_stats_data.R
\docType{data}
\name{summary_stats_data}
\alias{summary_stats_data}
\alias{data_add}
\alias{data_ccs}
\alias{data_rxnorm}
\alias{data_want}
\title{Summary Statistics Dataset for Visualization}
\format{
A list of four datasets: `data_add`, `data_ccs`, `data_rxnorm`, and `data_want`.
Each dataset consists of two data frames (`Total_Counts` and `Patient_Counts`).
}
\usage{
data("summary_stats_data")
}
\description{
A dataset containing summary statistics for different medical coding categories, 
including medications, laboratory tests, procedures, and diagnoses.
}
\details{
Each dataset in this file is a list containing two data frames:
- **Total_Counts**: Summary of total occurrences for each medical code.
- **Patient_Counts**: Number of unique patients associated with each medical code.

## Description of datasets:
- `data_add`: Includes top N most frequent lab tests along with additional user-specified variables.
- `data_ccs`: Summary statistics for CCS procedure codes.
- `data_rxnorm`: Summary statistics for RxNorm medication codes.
- `data_want`: User-defined list of medications with additional categorical labels.

## Column Descriptions:
The first three datasets (`data_add`, `data_ccs`, `data_rxnorm`) contain the following columns:
- **Parent_Code**: Medical code with relevant ontology prefix (e.g., RXNORM, LOINC, CCS-PCS).
- **Total_Count**: Total occurrences of the code in the dataset.
- **Patient_Count**: Number of unique patients linked to the code.
- **Name**: Description of the medical code (truncated if longer than 30 characters).

`data_want` contains an additional **Category** column:
- **Category**: Classification of medications (e.g., "Biologics DMARDs", "Glucocorticoids").

## Example Usage:
```r
data("summary_stats_data")
head(data_rxnorm$Total_Counts)
```
}
