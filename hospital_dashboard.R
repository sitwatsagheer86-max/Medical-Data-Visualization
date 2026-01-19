source("hospital_data_cleaning.R")


clean_df_internal <- prepare_cleaned_names_file(
  path_in = "hospital_patient_treatment_dataset.csv",
  path_out = "hospital_data_cleaned_names.csv"
)

clean_data <- readr::read_csv("hospital_data_cleaned_names.csv", show_col_types = FALSE)

#palettes
make_palette <- function(levels_vec, palette_name = "Set2") {
  levels_vec <- as.character(levels_vec)
  n <- length(levels_vec)
  if (n == 0) return(setNames(character(0), character(0)))
  
  max_pal <- suppressWarnings(RColorBrewer::brewer.pal.info[palette_name, "maxcolors"])
  if (is.na(max_pal)) max_pal <- 8
  
  base_cols <- RColorBrewer::brewer.pal(min(max(n, 3), max_pal), palette_name)
  
  cols <- if (n <= length(base_cols)) base_cols[seq_len(n)] else grDevices::colorRampPalette(base_cols)(n)
  setNames(cols, levels_vec)
}

dept_colors <- make_palette(sort(unique(clean_data$Department)), "Set2")
treatment_colors <- make_palette(sort(unique(clean_data$Treatment_Type)), "Set3")
gender_colors <- make_palette(sort(unique(clean_data$Gender)), "Set1")

#UI
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
      
      selectizeInput(
        "department_filter", "Filter by Department:",
        choices = c("All Departments", sort(unique(clean_data$Department))),
        selected = "All Departments",
        multiple = TRUE,
        options = list(placeholder = "Select departments...")
      ),
      
      selectizeInput(
        "treatment_filter", "Filter by Treatment Type:",
        choices = c("All Treatments", sort(unique(clean_data$Treatment_Type))),
        selected = "All Treatments",
        multiple = TRUE,
        options = list(placeholder = "Select treatments...")
      ),
      
      selectizeInput(
        "doctor_filter", "Filter by Doctor:",
        choices = c("All Doctors", sort(unique(clean_data$Doctor_Name))),
        selected = "All Doctors",
        multiple = TRUE,
        options = list(placeholder = "Select doctors...")
      ),
      
      sliderInput(
        "age_filter", "Patient Age Range:",
        min = min(clean_data$Age, na.rm = TRUE),
        max = max(clean_data$Age, na.rm = TRUE),
        value = c(min(clean_data$Age, na.rm = TRUE), max(clean_data$Age, na.rm = TRUE)),
        step = 1
      ),
      
      sliderInput(
        "recovery_filter", "Recovery Score Range:",
        min = 0, max = 100,
        value = c(0, 100),
        step = 1
      ),
      
      sliderInput(
        "cost_filter", "Treatment Cost Range ($):",
        min = floor(min(clean_data$Treatment_Cost, na.rm = TRUE) / 1000) * 1000,
        max = ceiling(max(clean_data$Treatment_Cost, na.rm = TRUE) / 1000) * 1000,
        value = c(min(clean_data$Treatment_Cost, na.rm = TRUE), max(clean_data$Treatment_Cost, na.rm = TRUE)),
        step = 1000,
        pre = "$"
      )
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
      tabItem(
        tabName = "overview",
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
      
      tabItem(
        tabName = "demographics",
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
      
      tabItem(
        tabName = "treatment",
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
      
      tabItem(
        tabName = "department",
        fluidRow(
          box(
            title = "Department Performance Metrics",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput(
              "dept_metric", "Select Metric to Compare:",
              choices = c(
                "Average Recovery Score" = "recovery",
                "Average Treatment Cost" = "cost",
                "Average Hospital Stay" = "stay",
                "Patient Count" = "count"
              ),
              selected = "recovery"
            ),
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
      
      tabItem(
        tabName = "doctor",
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
      
      tabItem(
        tabName = "cost",
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
      
      tabItem(
        tabName = "records",
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

#Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- clean_data
    
    if (!is.null(input$department_filter) && !"All Departments" %in% input$department_filter) {
      df <- df |> dplyr::filter(Department %in% input$department_filter)
    }
    
    if (!is.null(input$treatment_filter) && !"All Treatments" %in% input$treatment_filter) {
      df <- df |> dplyr::filter(Treatment_Type %in% input$treatment_filter)
    }
    
    if (!is.null(input$doctor_filter) && !"All Doctors" %in% input$doctor_filter) {
      df <- df |> dplyr::filter(Doctor_Name %in% input$doctor_filter)
    }
    
    df |>
      dplyr::filter(Age >= input$age_filter[1], Age <= input$age_filter[2]) |>
      dplyr::filter(Recovery_Score >= input$recovery_filter[1], Recovery_Score <= input$recovery_filter[2]) |>
      dplyr::filter(Treatment_Cost >= input$cost_filter[1], Treatment_Cost <= input$cost_filter[2])
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
      value = round(mean(filtered_data()$Recovery_Score, na.rm = TRUE), 1),
      subtitle = "Average Recovery Score",
      icon = icon("heart"),
      color = "green"
    )
  })
  
  output$avg_cost_box <- renderValueBox({
    valueBox(
      value = paste0("$", format(round(mean(filtered_data()$Treatment_Cost, na.rm = TRUE), 0), big.mark = ",")),
      subtitle = "Average Treatment Cost",
      icon = icon("dollar-sign"),
      color = "yellow"
    )
  })
  
  output$avg_stay_box <- renderValueBox({
    valueBox(
      value = round(mean(filtered_data()$Hospital_Stay_Days, na.rm = TRUE), 1),
      subtitle = "Average Hospital Stay (Days)",
      icon = icon("bed"),
      color = "red"
    )
  })
  
  #Charts
  output$department_dist_chart <- renderPlotly({
    df <- filtered_data() |>
      dplyr::group_by(Department) |>
      dplyr::summarise(Count = dplyr::n(), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(Count))
    
    plotly::plot_ly(
      df,
      x = ~reorder(Department, Count),
      y = ~Count,
      type = "bar",
      color = ~Department,
      colors = dept_colors
    ) |>
      plotly::layout(
        title = "",
        xaxis = list(title = "Department"),
        yaxis = list(title = "Number of Patients"),
        showlegend = FALSE
      )
  })
  
  output$treatment_dist_chart <- renderPlotly({
    df <- filtered_data() |>
      dplyr::group_by(Treatment_Type) |>
      dplyr::summarise(Count = dplyr::n(), .groups = "drop")
    
    plotly::plot_ly(
      df,
      labels = ~Treatment_Type,
      values = ~Count,
      type = "pie",
      textinfo = "label+percent",
      marker = list(colors = unname(treatment_colors[as.character(df$Treatment_Type)]))
    ) |>
      plotly::layout(title = "", showlegend = TRUE)
  })
  
  output$top_departments_chart <- renderPlotly({
    df <- filtered_data() |>
      dplyr::group_by(Department) |>
      dplyr::summarise(
        Avg_Recovery = mean(Recovery_Score, na.rm = TRUE),
        Avg_Cost = mean(Treatment_Cost, na.rm = TRUE),
        Patient_Count = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(Avg_Recovery))
    
    plotly::plot_ly(
      df,
      x = ~reorder(Department, Avg_Recovery),
      y = ~Avg_Recovery,
      type = "bar",
      color = ~Department,
      colors = dept_colors,
      text = ~paste0("Patients: ", Patient_Count, "<br>Avg Cost: $", round(Avg_Cost, 0))
    ) |>
      plotly::layout(
        title = "Departments by Average Recovery Score",
        xaxis = list(title = "Department"),
        yaxis = list(title = "Average Recovery Score", range = c(0, 100)),
        showlegend = FALSE
      )
  })
  
  output$age_distribution_chart <- renderPlotly({
    df <- filtered_data() |>
      dplyr::group_by(Age_Group) |>
      dplyr::summarise(Count = dplyr::n(), .groups = "drop")
    
    plotly::plot_ly(df, x = ~Age_Group, y = ~Count, type = "bar") |>
      plotly::layout(
        title = "Patient Distribution by Age Group",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Number of Patients")
      )
  })
  
  output$gender_distribution_chart <- renderPlotly({
    df <- filtered_data() |>
      dplyr::group_by(Gender) |>
      dplyr::summarise(Count = dplyr::n(), .groups = "drop")
    
    plotly::plot_ly(
      df,
      labels = ~Gender,
      values = ~Count,
      type = "pie",
      textinfo = "label+percent",
      marker = list(colors = unname(gender_colors[as.character(df$Gender)]))
    ) |>
      plotly::layout(title = "", showlegend = TRUE)
  })
  
  output$age_dept_chart <- renderPlotly({
    df <- filtered_data() |>
      dplyr::group_by(Department, Age_Group) |>
      dplyr::summarise(Count = dplyr::n(), .groups = "drop")
    
    plotly::plot_ly(df, x = ~Department, y = ~Count, color = ~Age_Group, type = "bar") |>
      plotly::layout(
        title = "Age Group Distribution by Department",
        xaxis = list(title = "Department"),
        yaxis = list(title = "Number of Patients"),
        barmode = "stack"
      )
  })
  
  output$treatment_effectiveness_chart <- renderPlotly({
    df <- filtered_data() |>
      dplyr::group_by(Treatment_Type) |>
      dplyr::summarise(
        Avg_Recovery = mean(Recovery_Score, na.rm = TRUE),
        Avg_Cost = mean(Treatment_Cost, na.rm = TRUE),
        Avg_Stay = mean(Hospital_Stay_Days, na.rm = TRUE),
        Patient_Count = dplyr::n(),
        .groups = "drop"
      )
    
    plotly::plot_ly(
      df,
      type = "scatter",
      mode = "markers",
      x = ~Avg_Cost,
      y = ~Avg_Recovery,
      size = ~Patient_Count,
      color = ~Treatment_Type,
      colors = treatment_colors,
      text = ~paste0(
        "Treatment: ", Treatment_Type,
        "<br>Patients: ", Patient_Count,
        "<br>Avg Stay: ", round(Avg_Stay, 1), " days"
      ),
      marker = list(sizemode = "diameter", sizeref = 2.5)
    ) |>
      plotly::layout(
        title = "Treatment Effectiveness: Cost vs Recovery",
        xaxis = list(title = "Average Treatment Cost ($)"),
        yaxis = list(title = "Average Recovery Score")
      )
  })
  
  output$treatment_cost_boxplot <- renderPlotly({
    plotly::plot_ly(
      filtered_data(),
      x = ~Treatment_Type,
      y = ~Treatment_Cost,
      color = ~Treatment_Type,
      colors = treatment_colors,
      type = "box",
      boxpoints = "all",
      jitter = 0.3,
      pointpos = 0
    ) |>
      plotly::layout(
        title = "Treatment Cost Distribution by Type",
        xaxis = list(title = "Treatment Type"),
        yaxis = list(title = "Treatment Cost ($)")
      )
  })
  
  output$treatment_stay_boxplot <- renderPlotly({
    plotly::plot_ly(
      filtered_data(),
      x = ~Treatment_Type,
      y = ~Hospital_Stay_Days,
      color = ~Treatment_Type,
      colors = treatment_colors,
      type = "box"
    ) |>
      plotly::layout(
        title = "Hospital Stay Duration by Treatment Type",
        xaxis = list(title = "Treatment Type"),
        yaxis = list(title = "Hospital Stay (Days)")
      )
  })
  
  output$department_metrics_chart <- renderPlotly({
    df <- filtered_data() |>
      dplyr::group_by(Department) |>
      dplyr::summarise(
        recovery = mean(Recovery_Score, na.rm = TRUE),
        cost = mean(Treatment_Cost, na.rm = TRUE),
        stay = mean(Hospital_Stay_Days, na.rm = TRUE),
        count = dplyr::n(),
        .groups = "drop"
      )
    
    metric <- input$dept_metric
    
    title_txt <- switch(
      metric,
      "recovery" = "Average Recovery Score",
      "cost" = "Average Treatment Cost",
      "stay" = "Average Hospital Stay",
      "count" = "Patient Count"
    )
    
    y_txt <- switch(
      metric,
      "recovery" = "Average Recovery Score",
      "cost" = "Average Treatment Cost ($)",
      "stay" = "Average Hospital Stay (Days)",
      "count" = "Number of Patients"
    )
    
    plotly::plot_ly(
      df,
      x = ~reorder(Department, .data[[metric]]),
      y = ~.data[[metric]],
      type = "bar",
      color = ~Department,
      colors = dept_colors,
      text = ~round(.data[[metric]], 1)
    ) |>
      plotly::layout(
        title = paste("Department Comparison -", title_txt),
        xaxis = list(title = "Department"),
        yaxis = list(title = y_txt),
        showlegend = FALSE
      )
  })
  
  output$top_doctors_chart <- renderPlotly({
    df <- filtered_data() |>
      dplyr::group_by(Doctor_Name) |>
      dplyr::summarise(
        Avg_Recovery = mean(Recovery_Score, na.rm = TRUE),
        Patient_Count = dplyr::n(),
        Departments = paste(unique(Department), collapse = ", "),
        .groups = "drop"
      ) |>
      dplyr::filter(Patient_Count >= 3) |>
      dplyr::arrange(dplyr::desc(Avg_Recovery)) |>
      head(10)
    
    plotly::plot_ly(
      df,
      x = ~reorder(Doctor_Name, Avg_Recovery),
      y = ~Avg_Recovery,
      type = "bar",
      text = ~paste0("Patients: ", Patient_Count, "<br>Departments: ", Departments)
    ) |>
      plotly::layout(
        title = "Top 10 Doctors by Average Recovery Score",
        xaxis = list(title = "Doctor"),
        yaxis = list(title = "Average Recovery Score", range = c(0, 100))
      )
  })
  
  output$doctor_workload_chart <- renderPlotly({
    df <- filtered_data() |>
      dplyr::group_by(Doctor_Name) |>
      dplyr::summarise(Patient_Count = dplyr::n(), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(Patient_Count)) |>
      head(15)
    
    plotly::plot_ly(df, x = ~reorder(Doctor_Name, Patient_Count), y = ~Patient_Count, type = "bar") |>
      plotly::layout(
        title = "Doctor Workload (Patient Count)",
        xaxis = list(title = "Doctor"),
        yaxis = list(title = "Number of Patients")
      )
  })
  
  output$cost_recovery_scatterplot <- renderPlotly({
    plotly::plot_ly(
      filtered_data(),
      x = ~Treatment_Cost,
      y = ~Recovery_Score,
      color = ~Department,
      colors = dept_colors,
      type = "scatter",
      mode = "markers",
      text = ~paste0(
        "Patient: ", Patient_ID,
        "<br>Department: ", Department,
        "<br>Treatment: ", Treatment_Type,
        "<br>Doctor: ", Doctor_Name
      ),
      marker = list(size = 8, opacity = 0.7)
    ) |>
      plotly::layout(
        title = "Treatment Cost vs Recovery Score",
        xaxis = list(title = "Treatment Cost ($)"),
        yaxis = list(title = "Recovery Score", range = c(0, 100))
      )
  })
  
  output$recovery_distribution_hist <- renderPlotly({
    plotly::plot_ly(filtered_data(), x = ~Recovery_Score, type = "histogram", nbinsx = 20) |>
      plotly::layout(
        title = "Recovery Score Distribution",
        xaxis = list(title = "Recovery Score", range = c(0, 100)),
        yaxis = list(title = "Number of Patients")
      )
  })
  
  output$recovery_status_piechart <- renderPlotly({
    df <- filtered_data() |>
      dplyr::group_by(Recovery_Status) |>
      dplyr::summarise(Count = dplyr::n(), .groups = "drop")
    
    status_levels <- sort(unique(clean_data$Recovery_Status))
    status_cols <- make_palette(status_levels, "Set1")
    
    plotly::plot_ly(
      df,
      labels = ~Recovery_Status,
      values = ~Count,
      type = "pie",
      textinfo = "label+percent",
      marker = list(colors = unname(status_cols[as.character(df$Recovery_Status)]))
    ) |>
      plotly::layout(title = "", showlegend = TRUE)
  })
  
  # Tables
  output$department_table <- renderDT({
    df <- filtered_data() |>
      dplyr::group_by(Department) |>
      dplyr::summarise(
        Patients = dplyr::n(),
        `Avg Recovery` = round(mean(Recovery_Score, na.rm = TRUE), 1),
        `Avg Cost` = paste0("$", format(round(mean(Treatment_Cost, na.rm = TRUE), 0), big.mark = ",")),
        `Avg Stay` = round(mean(Hospital_Stay_Days, na.rm = TRUE), 1),
        `Most Common Treatment` = names(sort(table(Treatment_Type), decreasing = TRUE))[1],
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(`Avg Recovery`))
    
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$doctor_specialization_table <- renderDT({
    df <- filtered_data() |>
      dplyr::group_by(Doctor_Name, Department) |>
      dplyr::summarise(Patients = dplyr::n(), .groups = "drop") |>
      tidyr::pivot_wider(names_from = Department, values_from = Patients, values_fill = 0)
    
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$patient_records_table <- renderDT({
    DT::datatable(
      filtered_data(),
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print")
      ),
      extensions = "Buttons",
      rownames = FALSE,
      filter = "top"
    )
  })
}
# run the application
shinyApp(ui = ui, server = server)