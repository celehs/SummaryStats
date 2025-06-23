
if (getRversion() >= "2.15.1") {                                                

  vars <- c(
    '.', 'Patient', 'Month', 'Parent_Code', 'Name', 'Presence', 
    'Category', 'Common_Ontology_Code', 'Common_Ontology_Description',
    'Group_Code', 'Group_Description', 'Count',
    'Total_Count', 'Patient_Count', 'Type', 'Year', 'Variable', 'Description',
    'target_similarity', 'description', 'cui', 'term', 
    'feature_id', '.data', 'patient_num', 'year', 'Feature', 'Total_Patients',
    'Dataset', 'Patients', 'Sample', 'total_patients', 'target_patients', 'feature_label',
    'desc', 'description_label', 'Rate', 'is_target', 'pick', 'Correlation', 'PheCode_Rate',
    'CUI_Rate', 'PheCode_Patients', 'CUI_Patients', 'x_pos', 'is_parent', 'Code_Count')
  
  utils::globalVariables(vars)
}
