normalize_gender <- function(x) {
  x <- stringr::str_trim(as.character(x))
  x_up <- stringr::str_to_upper(x)
  
  dplyr::case_when(
    x_up %in% c("M", "MALE") ~ "Male",
    x_up %in% c("F", "FEMALE") ~ "Female",
    is.na(x) | x == "" ~ NA_character_,
    TRUE ~ stringr::str_to_title(x)
  )
}

make_recovery_status <- function(score) {
  dplyr::case_when(
    score >= 90 ~ "Excellent (90-100)",
    score >= 75 ~ "Good (75-89)",
    score >= 60 ~ "Fair (60-74)",
    TRUE ~ "Poor (<60)"
  ) |>
    factor(levels = c("Poor (<60)", "Fair (60-74)", "Good (75-89)", "Excellent (90-100)"))
}

# cleaning function
make_clean_hospital_data <- function(path_in = "hospital_patient_treatment_dataset.csv") {
  df <- readr::read_csv(path_in, show_col_types = FALSE) |>
    janitor::clean_names()
  
  # Expected columns after clean_names()
  required <- c(
    "patient_id", "department", "treatment_type", "doctor_name", "gender",
    "age", "treatment_cost", "hospital_stay_days", "recovery_score"
  )
  
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      "Missing required columns after janitor::clean_names(): ",
      paste(missing, collapse = ", ")
    )
  }
  
  df |>
    dplyr::mutate(
      # text cleanup
      dplyr::across(
        c(department, treatment_type, doctor_name),
        ~ stringr::str_squish(as.character(.x))
      ),
      gender = normalize_gender(gender),
      
      # types
      age = as.integer(age),
      treatment_cost = round(as.numeric(treatment_cost), 2),
      hospital_stay_days = as.integer(hospital_stay_days),
      recovery_score = as.integer(recovery_score),
      
      # age
      age_group = cut(
        age,
        breaks = c(0, 18, 35, 50, 65, 100),
        labels = c("0-18", "19-35", "36-50", "51-65", "65+"),
        include.lowest = TRUE
      ),
      
      # cost
      cost_category = cut(
        treatment_cost,
        breaks = c(0, 25000, 50000, 100000, 150000, Inf),
        labels = c(
          "Very Low (≤25k)",
          "Low (25k-50k)",
          "Medium (50k-100k)",
          "High (100k-150k)",
          "Very High (>150k)"
        ),
        include.lowest = TRUE
      ),
      
      recovery_status = make_recovery_status(recovery_score),
      department = factor(department),
      treatment_type = factor(stringr::str_to_title(treatment_type)),
      doctor_name = factor(doctor_name),
      gender = factor(gender, levels = c("Male", "Female", "Other"))
    ) |>
    dplyr::select(
      patient_id, department, treatment_type, doctor_name, gender,
      age, age_group, treatment_cost, cost_category,
      hospital_stay_days, recovery_score, recovery_status
    )
}

#save cleaned data
save_cleaned_names_csv <- function(clean_df,
                                   path_out = "hospital_data_cleaned_names.csv") {
  
  out <- clean_df |>
    dplyr::rename(
      Patient_ID = patient_id,
      Department = department,
      Treatment_Type = treatment_type,
      Doctor_Name = doctor_name,
      Gender = gender,
      Age = age,
      Age_Group = age_group,
      Treatment_Cost = treatment_cost,
      Cost_Category = cost_category,
      Hospital_Stay_Days = hospital_stay_days,
      Recovery_Score = recovery_score,
      Recovery_Status = recovery_status
    )
  
  readr::write_csv(out, path_out)
  invisible(out)
}

# clean raw CSV and save
prepare_cleaned_names_file <- function(
    path_in = "hospital_patient_treatment_dataset.csv",
    path_out = "hospital_data_cleaned_names.csv"
) {
  clean_df <- make_clean_hospital_data(path_in)
  save_cleaned_names_csv(clean_df, path_out)
  invisible(clean_df)
}