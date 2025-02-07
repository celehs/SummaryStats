
dictionary_mapping <- data.frame(
  Group_Code = c("RXNORM:123", "RXNORM:456"),
  Common_Ontology_Code = c("RXNORM:001", "RXNORM:002"),
  Common_Ontology_Description = c("Acetaminophen", "Ibuprofen"),
  Group_Description = c("Pain Relievers", "NSAIDs")
)

df_ehr <- data.frame(
  Month = sample(1:12, 1000, replace = TRUE),
  Patient = sample(1:500, 1000, replace = TRUE),
  Parent_Code = sample(c("RXNORM:123", "RXNORM:456", "RXNORM:789"), 1000, replace = TRUE),
  Count = sample(1:10, 1000, replace = TRUE)
)


test_db_path = tempfile()
test_db = dbConnect(SQLite(), test_db_path)
dbWriteTable(test_db, 'df_monthly', df_ehr, overwrite = TRUE)

intermediary_test_db_path = tempfile()
generate_intermediary_sqlite(test_db, output_sqlite_path = intermediary_test_db_path)

interm_test_db = dbConnect(SQLite(), intermediary_test_db_path)
df_summ = dbGetQuery(interm_test_db, 'select * from processed_data;')
dbDisconnect(interm_test_db)

data_null <- extract_data_for_visualization(
  sqlite_file = intermediary_test_db_path,
  prefix = "RXNORM:",
  top_n = 20,
  dictionary_mapping = dictionary_mapping
)

data_null = lapply(data_null, na.omit)

gg = plot_visualized_data(
  data = data_null,
  count_column = "Patient_Count",
  prefix = "RXNORM:",
  description_label = "RXNORM Medications"
)

expect_true(ggplot2::is.ggplot(gg))

