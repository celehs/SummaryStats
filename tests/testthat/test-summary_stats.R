
dictionary_mapping <- data.frame(
  Group_Code = c("RXNORM:123", "RXNORM:456"),
  Common_Ontology_Code = c("ONT:001", "ONT:002"),
  Group_Description = c("Description1", "Description2")
)

assign("dictionary_mapping", dictionary_mapping, envir = .GlobalEnv)

df_ehr <- data.frame(
  Month = round(runif(1000, 1, 100)),
  Patient = round(runif(1000, 1, 100)),
  Parent_Code = c("RXNORM:123", "RXNORM:456", "RXNORM:789")[sample(3, 1000, replace = TRUE)],
  Count = round(runif(1000, 1, 10))
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
  top_n = 30,
  dict_prefix = "RXNORM:"
)

data_null = lapply(data_null, na.omit)

gg = plot_visualized_data(
  data = data_null,
  count_column = "Patient_Count",
  prefix = "RXNORM:",
  description_label = "RXNORM Medications"
)

expect_true(ggplot2::is.ggplot(gg))

