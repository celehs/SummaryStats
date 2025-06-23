#' Load ONCE-Generated Feature Data
#'
#' Loads ONCE-generated codified and NLP feature dictionaries based on a target phenotype.
#'
#' @param target_code The codified feature ID of interest (e.g., "PheCode:335").
#' @param O2 Logical flag for whether the script is running on O2 (default = TRUE).
#' @param path_code Optional file path to codified CSV (if O2 = FALSE).
#' @param path_nlp Optional file path to NLP CSV (if O2 = FALSE).
#'
#' @return A named list with two data frames: `code` and `nlp`, each including `feature_id`, `description`, and `target_similarity`.
#' @export
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
