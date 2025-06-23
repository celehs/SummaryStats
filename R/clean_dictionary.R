#' Clean Feature Dictionary
#'
#' Prepares a code-to-description mapping from a user-provided dictionary CSV. Cleans and deduplicates descriptions for consistency.
#'
#' @param dictionary_path File path to the data dictionary CSV.
#' @return A cleaned data frame with two columns: `feature_id` and `description`.
#' @export

clean_dictionary <- function(dictionary_path) {
  read.csv(dictionary_path) %>%
    select(feature_id, description) %>%
    distinct() %>%
    mutate(
      description = dplyr::case_when(
        stringr::str_detect(description, "forms of") ~ stringr::str_replace(description, "(.*)forms of.*", "\\1forms"),
        TRUE ~ stringr::str_replace_all(description, "\\s*\\(.*?\\)|,.*", "")
      ),
      description = tolower(description)
    )
}
