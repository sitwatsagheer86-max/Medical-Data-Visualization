# hospital_dashboard_names.R
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)

# Load cleaned data
clean_data <- read.csv("hospital_data_cleaned_names.csv", stringsAsFactors = FALSE)

# Ensure factors are properly set
clean_data <- clean_data %>%
  mutate(
    Department = factor(Department),
    Treatment_Type = factor(Treatment_Type),
    Doctor_Name = factor(Doctor_Name),
    Gender = factor(Gender),
    Age_Group = factor(Age_Group),
    Cost_Category = factor(Cost_Category),
    Recovery_Status = factor(Recovery_Status)
  )

# Create a color palette for consistent coloring
dept_colors <- c("Cardiology" = "#FF6B6B", 
                 "Gastroenterology" = "#4ECDC4", 
                 "Neurology" = "#FFD166", 
                 "Oncology" = "#06D6A0", 
                 "Orthopedics" = "#118AB2", 
                 "Pediatrics" = "#EF476F")

treatment_colors <- c("Medication" = "#1F77B4", 
                      "Observation" = "#FF7F0E", 
                      "Therapy" = "#2CA02C", 
                      "Surgery" = "#D62728")

gender_colors <- c("Male" = "#1F77B4", 
                   "Female" = "#FF7F0E", 
                   "Other" = "#2CA02C")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Hospital Patient Analytics Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Patient Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Treatment Analysis", tabName = "treatment", icon = icon("pills")),
      menuItem("Department Performance", tabName = "department", icon = icon("hospital")),
      menuItem("Doctor Performance", tabName = "doctor", icon = icon("user-md")),
      menuItem("Cost & Recovery Analysis", tabName = "cost", icon = icon("dollar-sign")),
      menuItem("Patient Records", tabName = "records", icon = icon("database")),
      
      hr(),
      h4("Data Filters", style = "padding-left: 15px; color: #2C3E50;"),
      
      selectizeInput("department_filter", "Filter by Department:",
                     choices = c("All Departments", levels(clean_data$Department)),
                     selected = "All Departments",
                     multiple = TRUE,
                     options = list(placeholder = 'Select departments...')),
      
      selectizeInput("treatment_filter", "Filter by Treatment Type:",
                     choices = c("All Treatments", levels(clean_data$Treatment_Type)),
                     selected = "All Treatments",
                     multiple = TRUE,
                     options = list(placeholder = 'Select treatments...')),
      
      selectizeInput("doctor_filter", "Filter by Doctor:",
                     choices = c("All Doctors", levels(clean_data$Doctor_Name)),
                     selected = "All Doctors",
                     multiple = TRUE,
                     options = list(placeholder = 'Select doctors...')),
      
      sliderInput("age_filter", "Patient Age Range:",
                  min = min(clean_data$Age),
                  max = max(clean_data$Age),
                  value = c(min(clean_data$Age), max(clean_data$Age)),
                  step = 1),
      
      sliderInput("recovery_filter", "Recovery Score Range:",
                  min = 0, max = 100,
                  value = c(0, 100),
                  step = 1),
      
      sliderInput("cost_filter", "Treatment Cost Range ($):",
                  min = floor(min(clean_data$Treatment_Cost) / 1000) * 1000,
                  max = ceiling(max(clean_data$Treatment_Cost) / 1000) * 1000,
                  value = c(min(clean_data$Treatment_Cost), max(clean_data$Treatment_Cost)),
                  step = 1000,
                  pre = "$")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box {border-radius: 10px;}
        .box {border-radius: 10px; border-top: 3px solid #3C8DBC;}
        .content-wrapper {background-color: #f4f6f9;}
        .skin-blue .main-header .logo {background-color: #2C3E50;}
        .skin-blue .main-header .navbar {background-color: #3498DB;}
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_patients_box", width = 3),
                valueBoxOutput("avg_recovery_box", width = 3),
                valueBoxOutput("avg_cost_box", width = 3),
                valueBoxOutput("avg_stay_box", width = 3)
              ),
              
              fluidRow(
                box(
                  title = "Patient Distribution by Department",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("department_dist_chart"),
                  width = 6
                ),
                box(
                  title = "Treatment Type Distribution",
                  status = "success",
                  solidHeader = TRUE,
                  plotlyOutput("treatment_dist_chart"),
                  width = 6
                )
              ),
              
              fluidRow(
                box(
                  title = "Top Performing Departments",
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("top_departments_chart"),
                  width = 12
                )
              )
      ),
      
      # Demographics Tab
      tabItem(tabName = "demographics",
              fluidRow(
                box(
                  title = "Patient Age Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("age_distribution_chart"),
                  width = 6
                ),
                box(
                  title = "Gender Distribution",
                  status = "success",
                  solidHeader = TRUE,
                  plotlyOutput("gender_distribution_chart"),
                  width = 6
                )
              ),
              
              fluidRow(
                box(
                  title = "Age Groups by Department",
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("age_dept_chart"),
                  width = 12
                )
              )
      ),
      
      # Treatment Analysis Tab
      tabItem(tabName = "treatment",
              fluidRow(
                box(
                  title = "Treatment Effectiveness Comparison",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("treatment_effectiveness_chart"),
                  width = 12
                )
              ),
              
              fluidRow(
                box(
                  title = "Treatment Cost by Type",
                  status = "warning",
                  solidHeader = TRUE,
                  plotlyOutput("treatment_cost_boxplot"),
                  width = 6
                ),
                box(
                  title = "Hospital Stay Duration by Treatment",
                  status = "danger",
                  solidHeader = TRUE,
                  plotlyOutput("treatment_stay_boxplot"),
                  width = 6
                )
              )
      ),
      
      # Department Performance Tab
      tabItem(tabName = "department",
              fluidRow(
                box(
                  title = "Department Performance Metrics",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  selectInput("dept_metric", "Select Metric to Compare:",
                              choices = c("Average Recovery Score" = "recovery",
                                          "Average Treatment Cost" = "cost",
                                          "Average Hospital Stay" = "stay",
                                          "Patient Count" = "count"),
                              selected = "recovery"),
                  plotlyOutput("department_metrics_chart")
                )
              ),
              
              fluidRow(
                box(
                  title = "Department Comparison Table",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("department_table")
                )
              )
      ),
      
      # Doctor Performance Tab
      tabItem(tabName = "doctor",
              fluidRow(
                box(
                  title = "Top 10 Doctors by Average Recovery Score",
                  status = "success",
                  solidHeader = TRUE,
                  plotlyOutput("top_doctors_chart"),
                  width = 12
                )
              ),
              
              fluidRow(
                box(
                  title = "Doctor Workload (Number of Patients)",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("doctor_workload_chart"),
                  width = 6
                ),
                box(
                  title = "Doctor Specialization Matrix",
                  status = "warning",
                  solidHeader = TRUE,
                  DTOutput("doctor_specialization_table"),
                  width = 6
                )
              )
      ),
      
      # Cost & Recovery Analysis Tab
      tabItem(tabName = "cost",
              fluidRow(
                box(
                  title = "Treatment Cost vs Recovery Score",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("cost_recovery_scatterplot"),
                  width = 12
                )
              ),
              
              fluidRow(
                box(
                  title = "Recovery Score Distribution",
                  status = "success",
                  solidHeader = TRUE,
                  plotlyOutput("recovery_distribution_hist"),
                  width = 6
                ),
                box(
                  title = "Recovery Status Categories",
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("recovery_status_piechart"),
                  width = 6
                )
              )
      ),
      
      # Patient Records Tab
      tabItem(tabName = "records",
              fluidRow(
                box(
                  title = "Patient Records",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("patient_records_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Data Summary & Statistics",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("data_summary_output")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- clean_data
    
    # Apply department filter
    if (!is.null(input$department_filter) && !"All Departments" %in% input$department_filter) {
      data <- data %>% filter(Department %in% input$department_filter)
    }
    
    # Apply treatment filter
    if (!is.null(input$treatment_filter) && !"All Treatments" %in% input$treatment_filter) {
      data <- data %>% filter(Treatment_Type %in% input$treatment_filter)
    }
    
    # Apply doctor filter
    if (!is.null(input$doctor_filter) && !"All Doctors" %in% input$doctor_filter) {
      data <- data %>% filter(Doctor_Name %in% input$doctor_filter)
    }
    
    # Apply numeric filters
    data <- data %>%
      filter(Age >= input$age_filter[1] & Age <= input$age_filter[2]) %>%
      filter(Recovery_Score >= input$recovery_filter[1] & Recovery_Score <= input$recovery_filter[2]) %>%
      filter(Treatment_Cost >= input$cost_filter[1] & Treatment_Cost <= input$cost_filter[2])
    
    return(data)
  })
  
  # Value boxes
  output$total_patients_box <- renderValueBox({
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Total Patients",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$avg_recovery_box <- renderValueBox({
    valueBox(
      value = round(mean(filtered_data()$Recovery_Score), 1),
      subtitle = "Average Recovery Score",
      icon = icon("heart"),
      color = "green"
    )
  })
  
  output$avg_cost_box <- renderValueBox({
    valueBox(
      value = paste0("$", format(round(mean(filtered_data()$Treatment_Cost), 0), big.mark = ",")),
      subtitle = "Average Treatment Cost",
      icon = icon("dollar-sign"),
      color = "yellow"
    )
  })
  
  output$avg_stay_box <- renderValueBox({
    valueBox(
      value = round(mean(filtered_data()$Hospital_Stay_Days), 1),
      subtitle = "Average Hospital Stay (Days)",
      icon = icon("bed"),
      color = "red"
    )
  })
  
  # Charts
  output$department_dist_chart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Department) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))
    
    plot_ly(data, 
            x = ~reorder(Department, Count), 
            y = ~Count, 
            type = 'bar',
            marker = list(color = ~Department, 
                          colors = dept_colors)) %>%
      layout(title = "",
             xaxis = list(title = "Department", 
                          categoryorder = "array",
                          categoryarray = data$Department[order(data$Count)]),
             yaxis = list(title = "Number of Patients"),
             showlegend = FALSE)
  })
  
  output$treatment_dist_chart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Treatment_Type) %>%
      summarise(Count = n())
    
    plot_ly(data,
            labels = ~Treatment_Type,
            values = ~Count,
            type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = treatment_colors[data$Treatment_Type])) %>%
      layout(title = "",
             showlegend = TRUE)
  })
  
  output$top_departments_chart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Department) %>%
      summarise(
        Avg_Recovery = mean(Recovery_Score),
        Avg_Cost = mean(Treatment_Cost),
        Patient_Count = n()
      ) %>%
      arrange(desc(Avg_Recovery))
    
    plot_ly(data,
            x = ~reorder(Department, Avg_Recovery),
            y = ~Avg_Recovery,
            type = 'bar',
            marker = list(color = dept_colors[data$Department]),
            text = ~paste("Patients: ", Patient_Count, 
                          "<br>Avg Cost: $", round(Avg_Cost, 0))) %>%
      layout(title = "Departments by Average Recovery Score",
             xaxis = list(title = "Department"),
             yaxis = list(title = "Average Recovery Score",
                          range = c(0, 100)))
  })
  
  output$age_distribution_chart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Age_Group) %>%
      summarise(Count = n())
    
    plot_ly(data,
            x = ~Age_Group,
            y = ~Count,
            type = 'bar',
            marker = list(color = 'steelblue')) %>%
      layout(title = "Patient Distribution by Age Group",
             xaxis = list(title = "Age Group"),
             yaxis = list(title = "Number of Patients"))
  })
  
  output$gender_distribution_chart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Gender) %>%
      summarise(Count = n())
    
    plot_ly(data,
            labels = ~Gender,
            values = ~Count,
            type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = gender_colors[data$Gender])) %>%
      layout(title = "",
             showlegend = TRUE)
  })
  
  output$age_dept_chart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Department, Age_Group) %>%
      summarise(Count = n())
    
    plot_ly(data,
            x = ~Department,
            y = ~Count,
            color = ~Age_Group,
            type = 'bar') %>%
      layout(title = "Age Group Distribution by Department",
             xaxis = list(title = "Department"),
             yaxis = list(title = "Number of Patients"),
             barmode = 'stack')
  })
  
  output$treatment_effectiveness_chart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Treatment_Type) %>%
      summarise(
        Avg_Recovery = mean(Recovery_Score),
        Avg_Cost = mean(Treatment_Cost),
        Avg_Stay = mean(Hospital_Stay_Days),
        Patient_Count = n()
      )
    
    plot_ly(data,
            type = 'scatter',
            mode = 'markers',
            x = ~Avg_Cost,
            y = ~Avg_Recovery,
            size = ~Patient_Count,
            color = ~Treatment_Type,
            colors = treatment_colors,
            text = ~paste("Treatment: ", Treatment_Type,
                          "<br>Patients: ", Patient_Count,
                          "<br>Avg Stay: ", round(Avg_Stay, 1), "days"),
            marker = list(sizemode = 'diameter',
                          sizeref = 2.5)) %>%
      layout(title = "Treatment Effectiveness: Cost vs Recovery",
             xaxis = list(title = "Average Treatment Cost ($)"),
             yaxis = list(title = "Average Recovery Score"))
  })
  
  output$treatment_cost_boxplot <- renderPlotly({
    plot_ly(filtered_data(),
            x = ~Treatment_Type,
            y = ~Treatment_Cost,
            color = ~Treatment_Type,
            colors = treatment_colors,
            type = 'box',
            boxpoints = 'all',
            jitter = 0.3,
            pointpos = 0) %>%
      layout(title = "Treatment Cost Distribution by Type",
             xaxis = list(title = "Treatment Type"),
             yaxis = list(title = "Treatment Cost ($)"))
  })
  
  output$treatment_stay_boxplot <- renderPlotly({
    plot_ly(filtered_data(),
            x = ~Treatment_Type,
            y = ~Hospital_Stay_Days,
            color = ~Treatment_Type,
            colors = treatment_colors,
            type = 'box') %>%
      layout(title = "Hospital Stay Duration by Treatment Type",
             xaxis = list(title = "Treatment Type"),
             yaxis = list(title = "Hospital Stay (Days)"))
  })
  
  output$department_metrics_chart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Department) %>%
      summarise(
        recovery = mean(Recovery_Score),
        cost = mean(Treatment_Cost),
        stay = mean(Hospital_Stay_Days),
        count = n()
      )
    
    metric <- input$dept_metric
    
    plot_ly(data,
            x = ~reorder(Department, get(metric)),
            y = ~get(metric),
            type = 'bar',
            marker = list(color = dept_colors[data$Department]),
            text = ~round(get(metric), 1)) %>%
      layout(title = paste("Department Comparison -", 
                           switch(metric,
                                  "recovery" = "Average Recovery Score",
                                  "cost" = "Average Treatment Cost",
                                  "stay" = "Average Hospital Stay",
                                  "count" = "Patient Count")),
             xaxis = list(title = "Department"),
             yaxis = list(title = switch(metric,
                                         "recovery" = "Average Recovery Score",
                                         "cost" = "Average Treatment Cost ($)",
                                         "stay" = "Average Hospital Stay (Days)",
                                         "count" = "Number of Patients")))
  })
  
  output$top_doctors_chart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Doctor_Name) %>%
      summarise(
        Avg_Recovery = mean(Recovery_Score),
        Patient_Count = n(),
        Departments = paste(unique(Department), collapse = ", ")
      ) %>%
      filter(Patient_Count >= 3) %>%
      arrange(desc(Avg_Recovery)) %>%
      head(10)
    
    plot_ly(data,
            x = ~reorder(Doctor_Name, Avg_Recovery),
            y = ~Avg_Recovery,
            type = 'bar',
            marker = list(color = 'lightgreen'),
            text = ~paste("Patients: ", Patient_Count,
                          "<br>Departments: ", Departments)) %>%
      layout(title = "Top 10 Doctors by Average Recovery Score",
             xaxis = list(title = "Doctor"),
             yaxis = list(title = "Average Recovery Score",
                          range = c(0, 100)))
  })
  
  output$doctor_workload_chart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Doctor_Name) %>%
      summarise(Patient_Count = n()) %>%
      arrange(desc(Patient_Count)) %>%
      head(15)
    
    plot_ly(data,
            x = ~reorder(Doctor_Name, Patient_Count),
            y = ~Patient_Count,
            type = 'bar',
            marker = list(color = 'steelblue')) %>%
      layout(title = "Doctor Workload (Patient Count)",
             xaxis = list(title = "Doctor"),
             yaxis = list(title = "Number of Patients"))
  })
  
  output$cost_recovery_scatterplot <- renderPlotly({
    plot_ly(filtered_data(),
            x = ~Treatment_Cost,
            y = ~Recovery_Score,
            color = ~Department,
            colors = dept_colors,
            type = 'scatter',
            mode = 'markers',
            text = ~paste("Patient: ", Patient_ID,
                          "<br>Department: ", Department,
                          "<br>Treatment: ", Treatment_Type,
                          "<br>Doctor: ", Doctor_Name),
            marker = list(size = 8,
                          opacity = 0.7)) %>%
      layout(title = "Treatment Cost vs Recovery Score",
             xaxis = list(title = "Treatment Cost ($)"),
             yaxis = list(title = "Recovery Score",
                          range = c(0, 100)))
  })
  
  output$recovery_distribution_hist <- renderPlotly({
    plot_ly(filtered_data(),
            x = ~Recovery_Score,
            type = 'histogram',
            marker = list(color = 'lightgreen'),
            nbinsx = 20) %>%
      layout(title = "Recovery Score Distribution",
             xaxis = list(title = "Recovery Score",
                          range = c(0, 100)),
             yaxis = list(title = "Number of Patients"))
  })
  
  output$recovery_status_piechart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Recovery_Status) %>%
      summarise(Count = n())
    
    plot_ly(data,
            labels = ~Recovery_Status,
            values = ~Count,
            type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = c('#FF6B6B', '#FFD166', '#06D6A0', '#118AB2'))) %>%
      layout(title = "",
             showlegend = TRUE)
  })
  
  # Tables
  output$department_table <- renderDT({
    data <- filtered_data() %>%
      group_by(Department) %>%
      summarise(
        Patients = n(),
        `Avg Recovery` = round(mean(Recovery_Score), 1),
        `Avg Cost` = paste0("$", format(round(mean(Treatment_Cost), 0), big.mark = ",")),
        `Avg Stay` = round(mean(Hospital_Stay_Days), 1),
        `Most Common Treatment` = names(sort(table(Treatment_Type), decreasing = TRUE))[1]
      ) %>%
      arrange(desc(`Avg Recovery`))
    
    datatable(data,
              options = list(pageLength = 10,
                             scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$doctor_specialization_table <- renderDT({
    data <- filtered_data() %>%
      group_by(Doctor_Name, Department) %>%
      summarise(Patients = n(), .groups = 'drop') %>%
      pivot_wider(names_from = Department, 
                  values_from = Patients,
                  values_fill = 0)
    
    datatable(data,
              options = list(pageLength = 10,
                             scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$patient_records_table <- renderDT({
    datatable(filtered_data(),
              options = list(pageLength = 20,
                             scrollX = TRUE,
                             dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
              extensions = 'Buttons',
              rownames = FALSE,
              filter = 'top')
  })
  
  output$data_summary_output <- renderPrint({
    data <- filtered_data()
    
    cat("DATA SUMMARY FOR FILTERED PATIENTS\n")
    cat("===================================\n\n")
    
    cat("Basic Statistics:\n")
    cat("-----------------\n")
    cat("Total Patients:", nrow(data), "\n")
    cat("Date Range:", min(data$Age), "-", max(data$Age), "years old\n")
    cat("Average Age:", round(mean(data$Age), 1), "years\n")
    cat("Average Recovery Score:", round(mean(data$Recovery_Score), 1), "\n")
    cat("Average Treatment Cost: $", round(mean(data$Treatment_Cost), 2), "\n")
    cat("Average Hospital Stay:", round(mean(data$Hospital_Stay_Days), 1), "days\n\n")
    
    cat("Categorical Distributions:\n")
    cat("--------------------------\n")
    cat("\nDepartments:\n")
    print(table(data$Department))
    cat("\nTreatment Types:\n")
    print(table(data$Treatment_Type))
    cat("\nGender:\n")
    print(table(data$Gender))
    cat("\nRecovery Status:\n")
    print(table(data$Recovery_Status))
  })
}

# Run the app
shinyApp(ui = ui, server = server)