#' #' Clean and Process ONCE Data
#'
#' This function processes and formats ONCE feature data from codified and NLP-based sources. 
#' It reads data from CSV files, processes them by selecting relevant columns, renaming variables, 
#' and converting descriptions to lowercase for consistency.
#'
#' @param target_code A character string representing the target code to process. 
#' @param O2 A logical value (TRUE/FALSE) indicating whether to use predefined O2 paths for ONCE feature data.
#'        If TRUE, the function constructs file paths automatically based on the target code.
#' @param manual_ONCE_path_code A character string specifying a manual file path for codified ONCE feature data.
#'        Required only if `O2` is FALSE.
#' @param manual_ONCE_path_nlp A character string specifying a manual file path for NLP-based ONCE feature data.
#'        Required only if `O2` is FALSE.
#'
#' @return A list containing two data frames:
#' \item{code}{A processed data frame of codified ONCE features with selected columns and formatted descriptions.}
#' \item{nlp}{A processed data frame of NLP-based ONCE features with selected columns and formatted descriptions.}
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with predefined O2 paths
#' result <- clean_ONCE_data(target_code = "PheCode:", O2 = TRUE)
#'
#' # Example with manual paths
#' result <- clean_ONCE_data(target_code = "PheCode:335", O2 = FALSE, 
#'                           manual_ONCE_path_code = "path/to/codified.csv", 
#'                           manual_ONCE_path_nlp = "path/to/nlp.csv")
#'
#' # Access results
#' result$code  # Processed codified feature data
#' result$nlp   # Processed NLP feature data
#' }

clean_ONCE_data <- function(target_code, O2, manual_ONCE_path_code = NULL, manual_ONCE_path_nlp = NULL) {
  
  # Format target_code
  formatted_code <- gsub(":", "", target_code)
  formatted_code <- gsub("\\.", "_", formatted_code)
  
  # Define paths based on O2 flag
  if (O2) {
    ONCE_path_code <- paste0("/n/data1/hsph/biostat/celehs/lab/sm731/ONCE_features/codified/", formatted_code, "_cod_features.csv")
    ONCE_path_nlp <- paste0("/n/data1/hsph/biostat/celehs/lab/sm731/ONCE_features/NLP/", formatted_code, "_nlp_features.csv")
  } else {
    if (is.null(manual_ONCE_path_code) || is.null(manual_ONCE_path_nlp)) {
      stop("Manual paths must be provided when O2 is FALSE.")
    }
    ONCE_path_code <- manual_ONCE_path_code
    ONCE_path_nlp <- manual_ONCE_path_nlp
  }
  
  # Read CSV files
  ONCE_dictionary_code <- read.csv(ONCE_path_code)
  ONCE_dictionary_nlp <- read.csv(ONCE_path_nlp)
  
  # Process Code Dictionary
  ONCE_dictionary_code <- ONCE_dictionary_code %>%
    dplyr::select(Variable, Description, target_similarity) %>%
    rename(feature_id = Variable, description = Description) %>%
    mutate(description = tolower(description))
  
  # Process NLP Dictionary
  ONCE_dictionary_nlp <- ONCE_dictionary_nlp %>%
    dplyr::select(cui, term, target_similarity) %>%
    rename(feature_id = cui, description = term) %>%
    mutate(description = tolower(description))
  
  return(list(code = ONCE_dictionary_code, nlp = ONCE_dictionary_nlp))
} 
