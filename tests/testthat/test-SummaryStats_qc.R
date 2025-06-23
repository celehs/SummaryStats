
test_that("clean_dictionary outputs correct structure", {
  test_path <- testthat::test_path("testdata/fake_dict.csv")
  result <- clean_dictionary(test_path)

  expect_true(is.data.frame(result))
  expect_named(result, c("feature_id", "description"))
  expect_gt(nrow(result), 0)
})

test_that("clean_data processes sample files correctly", {
  test_paths <- list(codified1 = testthat::test_path("testdata/fake_codified.csv"))
  result <- clean_data(test_paths)

  expect_type(result, "list")
  expect_true(all(c("patient_num", "year", "feature_id") %in% names(result$codified1)))
})

test_that("clean_ONCE_data loads files manually when O2 = FALSE", {
  path_code <- testthat::test_path("testdata/ONCE_multiple_sclerosis_codified.csv")
  path_nlp <- testthat::test_path("testdata/ONCE_multiple_sclerosis_nlp.csv")

  result <- clean_ONCE_data("PheCode:335", O2 = FALSE, path_code = path_code, path_nlp = path_nlp)

  expect_true("code" %in% names(result))
  expect_true("nlp" %in% names(result))
  expect_s3_class(result$code, "data.frame")
  expect_s3_class(result$nlp, "data.frame")
})

test_that("plot_target_prevalence returns expected structure", {
  data_paths <- list(codified1 = testthat::test_path("testdata/fake_codified.csv"),
                     nlp1 = testthat::test_path("testdata/fake_nlp.csv"))
  inputs <- clean_data(data_paths)
  result <- plot_target_prevalence(inputs, "PheCode:335", "C0026769", list("1" = "Site A"))

  expect_named(result, c("sample_sizes", "nlp_plot", "codified_plot"))
  expect_true(inherits(result$nlp_plot, "gg"))
})

test_that("analyze_code_hierarchy returns plots for Phecodes", {
  paths <- list(codified1 = testthat::test_path("testdata/fake_codified.csv"))
  inputs <- clean_data(paths)
  dictionary <- clean_dictionary(testthat::test_path("testdata/fake_dict.csv"))

  results <- analyze_code_hierarchy(inputs, dictionary, list("1" = "Site A"), phecodes = c("PheCode:250"))
  expect_type(results, "list")
  expect_named(results[[1]], c("phecode", "rate_plot", "combined_plot"))
})

test_that("code_cui_alignment returns plots", {
  paths <- list(codified1 = testthat::test_path("testdata/fake_codified.csv"),
                nlp1 = testthat::test_path("testdata/fake_nlp.csv"))
  inputs <- clean_data(paths)
  dictionary <- clean_dictionary(testthat::test_path("testdata/fake_dict.csv"))

  result <- code_cui_alignment(inputs, "PheCode:335", "C0026769", dictionary, list("1" = "Site A"))
  expect_named(result, c("rates_plot", "counts_plot", "correlation_plot"))
})

test_that("plot_related_features returns expected domain results", {
  paths <- list(codified1 = testthat::test_path("testdata/fake_codified.csv"),
                nlp1 = testthat::test_path("testdata/fake_nlp.csv"))
  inputs <- clean_data(paths)
  result <- plot_related_features(
    data_inputs = inputs,
    target_code = "PheCode:335",
    target_cui = "C0026769",
    sample_labels = list("1" = "Site A"),
    O2 = FALSE,
    manual_ONCE_path_code = testthat::test_path("testdata/ONCE_multiple_sclerosis_codified.csv"),
    manual_ONCE_path_nlp = testthat::test_path("testdata/ONCE_multiple_sclerosis_nlp.csv"),
    types = c("Diagnosis"),
    type_dict = list("Diagnosis" = "PheCode")
  )

  expect_true(all(c("type", "line_plot", "combined_plot", "message") %in% names(result[[1]])))
})

# devtools::test()
