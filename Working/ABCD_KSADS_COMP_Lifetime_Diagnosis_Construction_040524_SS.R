#Setup: load necessary packages
library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(lubridate)
library(readr)
library(stringr)
options(digits = 4)

## Setup ## 
#Setup: Set your desired working directory to output the data 
setwd("C:/Users/Kate Scheuer/OneDrive - UW/Desktop/Lab/Second_year_ABCD_project/Working")
#Setup: Read in the KSADS-COMP youth and parent report diagnosis data
ABCD_KSADS_Parent_Diagnoses <- read.csv("mh_p_ksads_ss.csv")
ABCD_KSADS_Youth_Diagnoses <- read.csv("mh_y_ksads_ss.csv")
#Setup: Read in the KSADS V1 and V2 column structures to be used in selecting columns of interest and creating lifetime diagnoses
ABCD_KSADS_Baseline_T1_T2_Column_Structure <- read.csv("ABCD_Release_5.1_Baseline_T1_T2_KSADS_Diagnosis_Data_Structure_040324_SS.csv")
ABCD_KSADS_T3_T4_Column_Structure <- read.csv("ABCD_Release_5.1_T3_T4_KSADS_Diagnosis_Data_Structure_040324_SS.csv")

## Data Wrangling & Harmonization ## 
#1. Harmonize the parent and youth report data
#1.1 Merge the parent and youth report data frames
ABCD_Both_Parent_Youth_Report_Diagnoses <- full_join(ABCD_KSADS_Parent_Diagnoses, ABCD_KSADS_Youth_Diagnoses)
#2 Create a dataframe containing the diagnoses of interest for each relevant time point
#2.1 Subset the data based on assessment time point
#2.11 Baseline Diagnosis Data
ABCD_Baseline_KSADS_COMP_Diagnoses <- ABCD_Both_Parent_Youth_Report_Diagnoses[ABCD_Both_Parent_Youth_Report_Diagnoses$eventname == "baseline_year_1_arm_1", ]
#2.12 Year 1 Follow-up Diagnosis Data
ABCD_T1_KSADS_COMP_Diagnoses <- ABCD_Both_Parent_Youth_Report_Diagnoses[ABCD_Both_Parent_Youth_Report_Diagnoses$eventname == "1_year_follow_up_y_arm_1", ]
#2.13 Year 2 Follow-up Diagnosis Data
ABCD_T2_KSADS_COMP_Diagnoses <- ABCD_Both_Parent_Youth_Report_Diagnoses[ABCD_Both_Parent_Youth_Report_Diagnoses$eventname == "2_year_follow_up_y_arm_1", ]
#2.14 Year 3 Follow-up Diagnosis Data
ABCD_T3_KSADS_COMP_Diagnoses <- ABCD_Both_Parent_Youth_Report_Diagnoses[ABCD_Both_Parent_Youth_Report_Diagnoses$eventname == "3_year_follow_up_y_arm_1", ]
#2.15 Year 4 Follow-up Diagnosis Data
ABCD_T4_KSADS_COMP_Diagnoses <- ABCD_Both_Parent_Youth_Report_Diagnoses[ABCD_Both_Parent_Youth_Report_Diagnoses$eventname == "4_year_follow_up_y_arm_1", ]
#2.2 Subset the diagnoses of interest at each time point
#2.21 First, create a vector to use in sub-setting the relevant diagnosis columns (one for each version of the KSADS-COMP)
ABCD_KSADS_Baseline_T1_T2_Diagnosis_Columns <- ABCD_KSADS_Baseline_T1_T2_Column_Structure$variable_name
ABCD_KSADS_T3_T4_Columns <- ABCD_KSADS_T3_T4_Column_Structure$variable_name
#2.22 Baseline Diagnosis Data
ABCD_Baseline_KSADS_COMP_Diagnoses <- ABCD_Baseline_KSADS_COMP_Diagnoses[c("src_subject_id", "eventname", ABCD_KSADS_Baseline_T1_T2_Diagnosis_Columns)]
#2.23 Year 1 Follow-up Diagnosis Data
ABCD_T1_KSADS_COMP_Diagnoses <- ABCD_T1_KSADS_COMP_Diagnoses[c("src_subject_id", "eventname", ABCD_KSADS_Baseline_T1_T2_Diagnosis_Columns)]
#2.24 Year 2 Follow-up Diagnosis Data
ABCD_T2_KSADS_COMP_Diagnoses <- ABCD_T2_KSADS_COMP_Diagnoses[c("src_subject_id", "eventname", ABCD_KSADS_Baseline_T1_T2_Diagnosis_Columns)]
#2.25 Year 3 Follow-up Diagnosis Data
ABCD_T3_KSADS_COMP_Diagnoses <- ABCD_T3_KSADS_COMP_Diagnoses[c("src_subject_id", "eventname", ABCD_KSADS_T3_T4_Columns)]
#2.26 Year 4 Follow-up Diagnosis Data
ABCD_T4_KSADS_COMP_Diagnoses <- ABCD_T4_KSADS_COMP_Diagnoses[c("src_subject_id", "eventname", ABCD_KSADS_T3_T4_Columns)]
#2.3 Create a lifetime diagnosis column for each of the diagnoses of interest
#2.31 Baseline Diagnosis Data
for (diagnosis in unique(ABCD_KSADS_Baseline_T1_T2_Column_Structure$specific_diagnosis)) {
  #2.311 Extract variable names corresponding to the current specific diagnosis
  variables <- ABCD_KSADS_Baseline_T1_T2_Column_Structure$variable_name[ABCD_KSADS_Baseline_T1_T2_Column_Structure$specific_diagnosis == diagnosis]
  #2.312 Check if any of the variable columns have a 1 in ABCD_Baseline_KSADS_COMP_Diagnoses
  ABCD_Baseline_KSADS_COMP_Diagnoses[[diagnosis]] <-
    apply(ABCD_Baseline_KSADS_COMP_Diagnoses[variables], 1, function(x)
      any(x == 1, na.rm = TRUE))
  #2.313 Convert logical values to 0s and 1s
  ABCD_Baseline_KSADS_COMP_Diagnoses[[diagnosis]] <-
    as.integer(ABCD_Baseline_KSADS_COMP_Diagnoses[[diagnosis]])
}
#2.32 Year 1 Follow-up Diagnosis Data
for (diagnosis in unique(ABCD_KSADS_Baseline_T1_T2_Column_Structure$specific_diagnosis)) {
  #2.321 Extract variable names corresponding to the current specific diagnosis
  variables <- ABCD_KSADS_Baseline_T1_T2_Column_Structure$variable_name[ABCD_KSADS_Baseline_T1_T2_Column_Structure$specific_diagnosis == diagnosis]
  #2.322 Check if any of the variable columns have a 1 in ABCD_T1_KSADS_COMP_Diagnoses
  ABCD_T1_KSADS_COMP_Diagnoses[[diagnosis]] <-
    apply(ABCD_T1_KSADS_COMP_Diagnoses[variables], 1, function(x)
      any(x == 1, na.rm = TRUE))
  #2.323 Convert logical values to 0s and 1s
  ABCD_T1_KSADS_COMP_Diagnoses[[diagnosis]] <-
    as.integer(ABCD_T1_KSADS_COMP_Diagnoses[[diagnosis]])
}
#2.33 Year 2 Follow-up Diagnosis Data
for (diagnosis in unique(ABCD_KSADS_Baseline_T1_T2_Column_Structure$specific_diagnosis)) {
  #2.331 Extract variable names corresponding to the current specific diagnosis
  variables <- ABCD_KSADS_Baseline_T1_T2_Column_Structure$variable_name[ABCD_KSADS_Baseline_T1_T2_Column_Structure$specific_diagnosis == diagnosis]
  #2.332 Check if any of the variable columns have a 1 in ABCD_T2_KSADS_COMP_Diagnoses
  ABCD_T2_KSADS_COMP_Diagnoses[[diagnosis]] <-
    apply(ABCD_T2_KSADS_COMP_Diagnoses[variables], 1, function(x)
      any(x == 1, na.rm = TRUE))
  #2.333 Convert logical values to 0s and 1s
  ABCD_T2_KSADS_COMP_Diagnoses[[diagnosis]] <-
    as.integer(ABCD_T2_KSADS_COMP_Diagnoses[[diagnosis]])
}
#2.34 Year 3 Follow-up Diagnosis Data
for (diagnosis in unique(ABCD_KSADS_T3_T4_Column_Structure$specific_diagnosis)) {
  #2.341 Extract variable names corresponding to the current specific diagnosis
  variables <- ABCD_KSADS_T3_T4_Column_Structure$variable_name[ABCD_KSADS_T3_T4_Column_Structure$specific_diagnosis == diagnosis]
  #2.342 Check if any of the variable columns have a 1 in ABCD_T3_KSADS_COMP_Diagnoses
  ABCD_T3_KSADS_COMP_Diagnoses[[diagnosis]] <-
    apply(ABCD_T3_KSADS_COMP_Diagnoses[variables], 1, function(x)
      any(x == 1, na.rm = TRUE))
  #2.343 Convert logical values to 0s and 1s
  ABCD_T3_KSADS_COMP_Diagnoses[[diagnosis]] <-
    as.integer(ABCD_T3_KSADS_COMP_Diagnoses[[diagnosis]])
}
#2.35 Year 4 Follow-up Diagnosis Data
for (diagnosis in unique(ABCD_KSADS_T3_T4_Column_Structure$specific_diagnosis)) {
  #2.351 Extract variable names corresponding to the current specific diagnosis
  variables <- ABCD_KSADS_T3_T4_Column_Structure$variable_name[ABCD_KSADS_T3_T4_Column_Structure$specific_diagnosis == diagnosis]
  #2.352 Check if any of the variable columns have a 1 in ABCD_T4_KSADS_COMP_Diagnoses
  ABCD_T4_KSADS_COMP_Diagnoses[[diagnosis]] <-
    apply(ABCD_T4_KSADS_COMP_Diagnoses[variables], 1, function(x)
      any(x == 1, na.rm = TRUE))
  #2.353 Convert logical values to 0s and 1s
  ABCD_T4_KSADS_COMP_Diagnoses[[diagnosis]] <-
    as.integer(ABCD_T4_KSADS_COMP_Diagnoses[[diagnosis]])
}
#2.4 Use the specific diagnoses to generate umbrella (category) diagnoses
#2.41 Baseline Diagnosis Data
for (umbrella_diagnosis in unique(ABCD_KSADS_Baseline_T1_T2_Column_Structure$umbrella_diagnosis)) {
  #2.411 Concatenate "_disorder" to the umbrella diagnosis
  new_column_name <- paste0("umbrella_", umbrella_diagnosis, "_disorder")
  #2.412 Get specific diagnoses under the current umbrella diagnosis
  specific_diagnoses <- unique(ABCD_KSADS_Baseline_T1_T2_Column_Structure$specific_diagnosis[ABCD_KSADS_Baseline_T1_T2_Column_Structure$umbrella_diagnosis == umbrella_diagnosis])
  #2.413 Initialize a logical vector to store if any of the specific diagnoses have a 1
  any_diagnosis <- rep(FALSE, nrow(ABCD_Baseline_KSADS_COMP_Diagnoses))
  #2.414 Iterate over each specific diagnosis
  for (specific_diagnosis in specific_diagnoses) {
    #2.4141 Extract variable names corresponding to the current specific diagnosis
    variables <- ABCD_KSADS_Baseline_T1_T2_Column_Structure$variable_name[ABCD_KSADS_Baseline_T1_T2_Column_Structure$specific_diagnosis == specific_diagnosis]
    #2.4142 Check if any of the variable columns have a 1 in ABCD_Baseline_KSADS_COMP_Diagnoses
    any_diagnosis <- any_diagnosis | apply(ABCD_Baseline_KSADS_COMP_Diagnoses[variables], 1, function(x) any(x == 1, na.rm = TRUE))
  }
  #2.415 Store the result in a new column in ABCD_Baseline_KSADS_COMP_Diagnoses with the concatenated name
  ABCD_Baseline_KSADS_COMP_Diagnoses[[new_column_name]] <- as.integer(any_diagnosis)
}
#2.42 Year 1 Follow-up Diagnosis Data
for (umbrella_diagnosis in unique(ABCD_KSADS_Baseline_T1_T2_Column_Structure$umbrella_diagnosis)) {
  #2.421 Concatenate "_disorder" to the umbrella diagnosis
  new_column_name <- paste0("umbrella_", umbrella_diagnosis, "_disorder")
  #2.422 Get specific diagnoses under the current umbrella diagnosis
  specific_diagnoses <- unique(ABCD_KSADS_Baseline_T1_T2_Column_Structure$specific_diagnosis[ABCD_KSADS_Baseline_T1_T2_Column_Structure$umbrella_diagnosis == umbrella_diagnosis])
  #2.423 Initialize a logical vector to store if any of the specific diagnoses have a 1
  any_diagnosis <- rep(FALSE, nrow(ABCD_T1_KSADS_COMP_Diagnoses))
  #2.424 Iterate over each specific diagnosis
  for (specific_diagnosis in specific_diagnoses) {
    #2.4241 Extract variable names corresponding to the current specific diagnosis
    variables <- ABCD_KSADS_Baseline_T1_T2_Column_Structure$variable_name[ABCD_KSADS_Baseline_T1_T2_Column_Structure$specific_diagnosis == specific_diagnosis]
    #2.4242 Check if any of the variable columns have a 1 in ABCD_T1_KSADS_COMP_Diagnoses
    any_diagnosis <- any_diagnosis | apply(ABCD_T1_KSADS_COMP_Diagnoses[variables], 1, function(x) any(x == 1, na.rm = TRUE))
  }
  #2.425 Store the result in a new column in ABCD_T1_KSADS_COMP_Diagnoses with the concatenated name
  ABCD_T1_KSADS_COMP_Diagnoses[[new_column_name]] <- as.integer(any_diagnosis)
}
#2.43 Year 2 Follow-up Diagnosis Data
for (umbrella_diagnosis in unique(ABCD_KSADS_Baseline_T1_T2_Column_Structure$umbrella_diagnosis)) {
  #2.431 Concatenate "_disorder" to the umbrella diagnosis
  new_column_name <- paste0("umbrella_", umbrella_diagnosis, "_disorder")
  #2.432 Get specific diagnoses under the current umbrella diagnosis
  specific_diagnoses <- unique(ABCD_KSADS_Baseline_T1_T2_Column_Structure$specific_diagnosis[ABCD_KSADS_Baseline_T1_T2_Column_Structure$umbrella_diagnosis == umbrella_diagnosis])
  #2.433 Initialize a logical vector to store if any of the specific diagnoses have a 1
  any_diagnosis <- rep(FALSE, nrow(ABCD_T2_KSADS_COMP_Diagnoses))
  #2.434 Iterate over each specific diagnosis
  for (specific_diagnosis in specific_diagnoses) {
    #2.4341 Extract variable names corresponding to the current specific diagnosis
    variables <- ABCD_KSADS_Baseline_T1_T2_Column_Structure$variable_name[ABCD_KSADS_Baseline_T1_T2_Column_Structure$specific_diagnosis == specific_diagnosis]
    #2.4342 Check if any of the variable columns have a 1 in ABCD_T2_KSADS_COMP_Diagnoses
    any_diagnosis <- any_diagnosis | apply(ABCD_T2_KSADS_COMP_Diagnoses[variables], 1, function(x) any(x == 1, na.rm = TRUE))
  }
  #2.435 Store the result in a new column in ABCD_T2_KSADS_COMP_Diagnoses with the concatenated name
  ABCD_T2_KSADS_COMP_Diagnoses[[new_column_name]] <- as.integer(any_diagnosis)
}
#2.44 Year 3 Follow-up Diagnosis Data
for (umbrella_diagnosis in unique(ABCD_KSADS_T3_T4_Column_Structure$umbrella_diagnosis)) {
  #2.441 Concatenate "_disorder" to the umbrella diagnosis
  new_column_name <- paste0("umbrella_", umbrella_diagnosis, "_disorder")
  #2.442 Get specific diagnoses under the current umbrella diagnosis
  specific_diagnoses <- unique(ABCD_KSADS_T3_T4_Column_Structure$specific_diagnosis[ABCD_KSADS_T3_T4_Column_Structure$umbrella_diagnosis == umbrella_diagnosis])
  #2.443 Initialize a logical vector to store if any of the specific diagnoses have a 1
  any_diagnosis <- rep(FALSE, nrow(ABCD_T3_KSADS_COMP_Diagnoses))
  #2.444 Iterate over each specific diagnosis
  for (specific_diagnosis in specific_diagnoses) {
    #2.4441 Extract variable names corresponding to the current specific diagnosis
    variables <- ABCD_KSADS_T3_T4_Column_Structure$variable_name[ABCD_KSADS_T3_T4_Column_Structure$specific_diagnosis == specific_diagnosis]
    #2.4442 Check if any of the variable columns have a 1 in ABCD_T3_KSADS_COMP_Diagnoses
    any_diagnosis <- any_diagnosis | apply(ABCD_T3_KSADS_COMP_Diagnoses[variables], 1, function(x) any(x == 1, na.rm = TRUE))
  }
  #2.445 Store the result in a new column in ABCD_T3_KSADS_COMP_Diagnoses with the concatenated name
  ABCD_T3_KSADS_COMP_Diagnoses[[new_column_name]] <- as.integer(any_diagnosis)
}
#2.45 Year 4 Follow-up Diagnosis Data
for (umbrella_diagnosis in unique(ABCD_KSADS_T3_T4_Column_Structure$umbrella_diagnosis)) {
  #2.451 Concatenate "_disorder" to the umbrella diagnosis
  new_column_name <- paste0("umbrella_", umbrella_diagnosis, "_disorder")
  #2.452 Get specific diagnoses under the current umbrella diagnosis
  specific_diagnoses <- unique(ABCD_KSADS_T3_T4_Column_Structure$specific_diagnosis[ABCD_KSADS_T3_T4_Column_Structure$umbrella_diagnosis == umbrella_diagnosis])
  #2.453 Initialize a logical vector to store if any of the specific diagnoses have a 1
  any_diagnosis <- rep(FALSE, nrow(ABCD_T4_KSADS_COMP_Diagnoses))
  #2.454 Iterate over each specific diagnosis
  for (specific_diagnosis in specific_diagnoses) {
    #2.4541 Extract variable names corresponding to the current specific diagnosis
    variables <- ABCD_KSADS_T3_T4_Column_Structure$variable_name[ABCD_KSADS_T3_T4_Column_Structure$specific_diagnosis == specific_diagnosis]
    #2.4542 Check if any of the variable columns have a 1 in ABCD_T4_KSADS_COMP_Diagnoses
    any_diagnosis <- any_diagnosis | apply(ABCD_T4_KSADS_COMP_Diagnoses[variables], 1, function(x) any(x == 1, na.rm = TRUE))
  }
  #2.455 Store the result in a new column in ABCD_T4_KSADS_COMP_Diagnoses with the concatenated name
  ABCD_T4_KSADS_COMP_Diagnoses[[new_column_name]] <- as.integer(any_diagnosis)
}
#2.5 Subset the lifetime/umbrella diagnosis (and other) columns of interest from each data frame
#2.51 Baseline Diagnosis Data
ABCD_Baseline_KSADS_COMP_Diagnoses <- ABCD_Baseline_KSADS_COMP_Diagnoses %>% 
  dplyr::select(c(src_subject_id, eventname, lifetime_adhd, lifetime_unspecified_adhd, lifetime_agoraphobia, lifetime_gad, lifetime_other_anxiety, lifetime_panic, lifetime_separation_anxiety, lifetime_social_anxiety, lifetime_specific_phobia, lifetime_other_asd, lifetime_bipolar_I, lifetime_bipolar_II, lifetime_unspecified_bipolar, lifetime_conduct, lifetime_odd, lifetime_dmdd, lifetime_mdd, lifetime_pdd, lifetime_unspecified_depression, lifetime_unspecified_schizophrenia, lifetime_other_ptsd, lifetime_ptsd, umbrella_adhd_disorder, umbrella_unspecified_adhd_disorder, umbrella_anxiety_disorder, umbrella_other_anxiety_disorder, umbrella_specific_phobia_disorder, umbrella_other_asd_disorder, umbrella_bipolar_disorder, umbrella_unspecified_bipolar_disorder, umbrella_conduct_disorder, umbrella_depressive_disorder, umbrella_unspecified_depressive_disorder, umbrella_unspecified_psychosis_disorder, umbrella_other_trauma_stress_disorder, umbrella_trauma_stress_disorder))
#2.52 Year 1 Follow-up Diagnosis Data
ABCD_T1_KSADS_COMP_Diagnoses <- ABCD_T1_KSADS_COMP_Diagnoses %>% 
  dplyr::select(c(src_subject_id, eventname, lifetime_adhd, lifetime_unspecified_adhd, lifetime_agoraphobia, lifetime_gad, lifetime_other_anxiety, lifetime_panic, lifetime_separation_anxiety, lifetime_social_anxiety, lifetime_specific_phobia, lifetime_other_asd, lifetime_bipolar_I, lifetime_bipolar_II, lifetime_unspecified_bipolar, lifetime_conduct, lifetime_odd, lifetime_dmdd, lifetime_mdd, lifetime_pdd, lifetime_unspecified_depression, lifetime_unspecified_schizophrenia, lifetime_other_ptsd, lifetime_ptsd, umbrella_adhd_disorder, umbrella_unspecified_adhd_disorder, umbrella_anxiety_disorder, umbrella_other_anxiety_disorder, umbrella_specific_phobia_disorder, umbrella_other_asd_disorder, umbrella_bipolar_disorder, umbrella_unspecified_bipolar_disorder, umbrella_conduct_disorder, umbrella_depressive_disorder, umbrella_unspecified_depressive_disorder, umbrella_unspecified_psychosis_disorder, umbrella_other_trauma_stress_disorder, umbrella_trauma_stress_disorder))
#2.53 Year 2 Follow-up Diagnosis Data
ABCD_T2_KSADS_COMP_Diagnoses <- ABCD_T2_KSADS_COMP_Diagnoses %>% 
  dplyr::select(c(src_subject_id, eventname, lifetime_adhd, lifetime_unspecified_adhd, lifetime_agoraphobia, lifetime_gad, lifetime_other_anxiety, lifetime_panic, lifetime_separation_anxiety, lifetime_social_anxiety, lifetime_specific_phobia, lifetime_other_asd, lifetime_bipolar_I, lifetime_bipolar_II, lifetime_unspecified_bipolar, lifetime_conduct, lifetime_odd, lifetime_dmdd, lifetime_mdd, lifetime_pdd, lifetime_unspecified_depression, lifetime_unspecified_schizophrenia, lifetime_other_ptsd, lifetime_ptsd, umbrella_adhd_disorder, umbrella_unspecified_adhd_disorder, umbrella_anxiety_disorder, umbrella_other_anxiety_disorder, umbrella_specific_phobia_disorder, umbrella_other_asd_disorder, umbrella_bipolar_disorder, umbrella_unspecified_bipolar_disorder, umbrella_conduct_disorder, umbrella_depressive_disorder, umbrella_unspecified_depressive_disorder, umbrella_unspecified_psychosis_disorder, umbrella_other_trauma_stress_disorder, umbrella_trauma_stress_disorder))
#2.54 Year 3 Follow-up Diagnosis Data
ABCD_T3_KSADS_COMP_Diagnoses <- ABCD_T3_KSADS_COMP_Diagnoses %>% 
  dplyr::select(c(src_subject_id, eventname, lifetime_adhd, lifetime_other_adhd, lifetime_agoraphobia, lifetime_gad, lifetime_other_anxiety, lifetime_panic, lifetime_separation_anxiety, lifetime_social_anxiety, lifetime_specific_phobia, lifetime_asd, lifetime_other_asd, lifetime_bipolar_I, lifetime_bipolar_II, lifetime_other_bipolar, lifetime_conduct, lifetime_odd, lifetime_dmdd, lifetime_mdd, lifetime_other_depression, lifetime_pdd, lifetime_unspecified_depression, lifetime_other_psychosis, lifetime_schizoaffective, lifetime_schizophrenia, lifetime_schizophreniform, lifetime_adjustment, lifetime_other_ptsd, lifetime_ptsd, umbrella_adhd_disorder, umbrella_other_adhd_disorder, umbrella_anxiety_disorder, umbrella_other_anxiety_disorder, umbrella_specific_phobia_disorder, umbrella_asd_disorder, umbrella_other_asd_disorder, umbrella_bipolar_disorder, umbrella_other_bipolar_disorder, umbrella_conduct_disorder, umbrella_depressive_disorder, umbrella_other_depressive_disorder, umbrella_unspecified_depressive_disorder, umbrella_other_psychosis_disorder, umbrella_psychosis_disorder, umbrella_trauma_stress_disorder, umbrella_other_trauma_stress_disorder))
#2.55 Year 4 Follow-up Diagnosis Data
ABCD_T4_KSADS_COMP_Diagnoses <- ABCD_T4_KSADS_COMP_Diagnoses %>% 
  dplyr::select(c(src_subject_id, eventname, lifetime_adhd, lifetime_other_adhd, lifetime_agoraphobia, lifetime_gad, lifetime_other_anxiety, lifetime_panic, lifetime_separation_anxiety, lifetime_social_anxiety, lifetime_specific_phobia, lifetime_asd, lifetime_other_asd, lifetime_bipolar_I, lifetime_bipolar_II, lifetime_other_bipolar, lifetime_conduct, lifetime_odd, lifetime_dmdd, lifetime_mdd, lifetime_other_depression, lifetime_pdd, lifetime_unspecified_depression, lifetime_other_psychosis, lifetime_schizoaffective, lifetime_schizophrenia, lifetime_schizophreniform, lifetime_adjustment, lifetime_other_ptsd, lifetime_ptsd, umbrella_adhd_disorder, umbrella_other_adhd_disorder, umbrella_anxiety_disorder, umbrella_other_anxiety_disorder, umbrella_specific_phobia_disorder, umbrella_asd_disorder, umbrella_other_asd_disorder, umbrella_bipolar_disorder, umbrella_other_bipolar_disorder, umbrella_conduct_disorder, umbrella_depressive_disorder, umbrella_other_depressive_disorder, umbrella_unspecified_depressive_disorder, umbrella_other_psychosis_disorder, umbrella_psychosis_disorder, umbrella_trauma_stress_disorder, umbrella_other_trauma_stress_disorder))
#2.5 Merge the Data from Each Time Point
#2.51 Create a list containing the lifetime diagnosis dataframe from each time point
ABCD_KSADS_Diagnosis_Dataframes <- list(ABCD_Baseline_KSADS_COMP_Diagnoses, ABCD_T1_KSADS_COMP_Diagnoses, ABCD_T2_KSADS_COMP_Diagnoses, ABCD_T3_KSADS_COMP_Diagnoses, ABCD_T4_KSADS_COMP_Diagnoses)
#2.52 Merge the lifetime diagnosis data by shared columns
ABCD_KSADS_Merged_Lifetime_Diagnoses <- Reduce(function(x, y) merge(x, y, all = TRUE), ABCD_KSADS_Diagnosis_Dataframes) 
#2.6 Write the data as a CSV
write.csv(ABCD_KSADS_Merged_Lifetime_Diagnoses, "ABCD_Baseline_T1_T2_T3_T4_KSADS_Lifetime_Diagnoses.csv", row.names = FALSE)
