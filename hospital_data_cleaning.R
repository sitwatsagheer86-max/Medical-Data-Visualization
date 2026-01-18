# hospital_data_cleaning_names.R

# Load required libraries
library(dplyr)
library(tidyr)

# Read the data
data <- read.csv("hospital_patient_treatment_dataset.csv", 
                 stringsAsFactors = FALSE,
                 check.names = FALSE)

# Check unique values for each column
cat("=== Unique Values in Each Column ===\n")
cat("\nDepartments:\n")
print(unique(data$Department))
cat("\nTreatment Types:\n")
print(unique(data$`Treatment Type`))
cat("\nDoctor Names:\n")
print(unique(data$`Doctor Name`))
cat("\nGender:\n")
print(unique(data$Gender))

# Data Cleaning with proper name handling
clean_data <- data %>%
  # Create clean column names (no spaces)
  rename(
    Patient_ID = `Patient ID`,
    Department = Department,
    Treatment_Type = `Treatment Type`,
    Doctor_Name = `Doctor Name`,
    Gender = Gender,
    Age = Age,
    Treatment_Cost = `Treatment Cost`,
    Hospital_Stay_Days = `Hospital Stay (Days)`,
    Recovery_Score = `Recovery Score`
  ) %>%
  
  # Clean and standardize text values
  mutate(
    # Trim whitespace from text columns
    across(c(Department, Treatment_Type, Doctor_Name, Gender), 
           ~ trimws(.)),
    
    # Standardize treatment types if needed
    Treatment_Type = case_when(
      Treatment_Type == "Therapy" ~ "Therapy",
      Treatment_Type == "Medication" ~ "Medication",
      Treatment_Type == "Observation" ~ "Observation",
      Treatment_Type == "Surgery" ~ "Surgery",
      TRUE ~ Treatment_Type
    ),
    
    # Standardize gender values
    Gender = case_when(
      toupper(Gender) %in% c("M", "MALE") ~ "Male",
      toupper(Gender) %in% c("F", "FEMALE") ~ "Female",
      TRUE ~ Gender
    ),
    
    # Convert to proper data types
    Age = as.integer(Age),
    Treatment_Cost = round(as.numeric(Treatment_Cost), 2),
    Hospital_Stay_Days = as.integer(Hospital_Stay_Days),
    Recovery_Score = as.integer(Recovery_Score)
  ) %>%
  
  # Convert to factors with proper levels
  mutate(
    Department = factor(Department, 
                        levels = sort(unique(Department))),
    Treatment_Type = factor(Treatment_Type,
                            levels = c("Medication", "Observation", 
                                       "Therapy", "Surgery")),
    Doctor_Name = factor(Doctor_Name,
                         levels = sort(unique(Doctor_Name))),
    Gender = factor(Gender,
                    levels = c("Male", "Female", "Other"))
  ) %>%
  
  # Create derived columns
  mutate(
    # Age groups
    Age_Group = cut(Age,
                    breaks = c(0, 18, 35, 50, 65, 100),
                    labels = c("0-18", "19-35", "36-50", "51-65", "65+"),
                    include.lowest = TRUE),
    
    # Cost categories
    Cost_Category = cut(Treatment_Cost,
                        breaks = c(0, 25000, 50000, 100000, 150000, Inf),
                        labels = c("Very Low (≤25k)", 
                                   "Low (25k-50k)", 
                                   "Medium (50k-100k)", 
                                   "High (100k-150k)", 
                                   "Very High (>150k)"),
                        include.lowest = TRUE),
    
    # Recovery status
    Recovery_Status = case_when(
      Recovery_Score >= 90 ~ "Excellent (90-100)",
      Recovery_Score >= 75 ~ "Good (75-89)",
      Recovery_Score >= 60 ~ "Fair (60-74)",
      TRUE ~ "Poor (<60)"
    ),
    Recovery_Status = factor(Recovery_Status,
                             levels = c("Poor (<60)", "Fair (60-74)", 
                                        "Good (75-89)", "Excellent (90-100)"))
  ) %>%
  
  # Select and order columns
  select(
    Patient_ID, Department, Treatment_Type, Doctor_Name, Gender,
    Age, Age_Group, Treatment_Cost, Cost_Category,
    Hospital_Stay_Days, Recovery_Score, Recovery_Status
  )

# Display cleaned data info
cat("\n=== Cleaned Data Information ===\n")
cat("Number of patients:", nrow(clean_data), "\n")
cat("Number of departments:", n_distinct(clean_data$Department), "\n")
cat("Departments:", paste(levels(clean_data$Department), collapse = ", "), "\n")
cat("\nTreatment types:", paste(levels(clean_data$Treatment_Type), collapse = ", "), "\n")
cat("Number of doctors:", n_distinct(clean_data$Doctor_Name), "\n")
cat("Doctors:", paste(levels(clean_data$Doctor_Name), collapse = ", "), "\n")
cat("\nGender distribution:\n")
print(table(clean_data$Gender))
cat("\nAge groups:\n")
print(table(clean_data$Age_Group))

# Save cleaned data
write.csv(clean_data, "hospital_data_cleaned_names.csv", row.names = FALSE)

# Create a summary table for quick reference
summary_table <- clean_data %>%
  group_by(Department) %>%
  summarise(
    Patients = n(),
    Avg_Recovery = round(mean(Recovery_Score), 1),
    Avg_Cost = round(mean(Treatment_Cost), 2),
    Avg_Stay = round(mean(Hospital_Stay_Days), 1)
  )

cat("\n=== Department Summary ===\n")
print(summary_table)