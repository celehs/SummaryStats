
if (getRversion() >= "2.15.1") {                                                

  vars <- c(
    '.', 'label', 'id', 'Patient', 'Month', 'Parent_Code', 'value', 'db_name', 
    'file_name', 'word', 'text','Category', 'Common_Ontology_Code', 'Common_Ontology_Description',
    'Count', 'Group_Code', 'Group_Description', 'Name', 'Presence',
    'Total_Count', 'Type', 'dictionary_mapping', 'data.table'
  )
  
  utils::globalVariables(vars)
}
