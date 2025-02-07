
if (getRversion() >= "2.15.1") {                                                

  vars <- c(
    '.', 'Patient', 'Month', 'Parent_Code', 'Name', 'Presence', 
    'Category', 'Common_Ontology_Code', 'Common_Ontology_Description',
    'Group_Code', 'Group_Description', 'Count',
    'Total_Count', 'Patient_Count', 'Type'
  )
  
  utils::globalVariables(vars)
}
