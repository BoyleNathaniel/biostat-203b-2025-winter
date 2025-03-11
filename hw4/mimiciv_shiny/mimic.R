#Shiny app code

#installs

#install.packages("shinydashboard")
#install.packages('DT')
#install.packages("gt")
# Load necessary libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(gt)
library(gtsummary)
library(bigrquery)

# Load the mimic_icu_cohort data (assuming it's saved as .rds file)
mimic_icu_cohort <- readRDS("mimiciv_shiny/mimic_icu_cohort.rds")

# UI of the app
ui <- dashboardPage(
  dashboardHeader(title = "ICU Cohort Data Exploration"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
      menuItem("Patient Info", tabName = "patient_info", icon = icon("user"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Tab 1: Summary Statistics
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "Select Variable for Bar Plot",
                    width = 4, solidHeader = TRUE, status = "info",
                    selectInput("barplot_var", "Choose Variable for Bar Plot",
                                choices = c("Insurance" = "insurance",
                                            "Marital Status" = "marital_status",
                                            "Race" = "race",
                                            "Gender" = "gender"
                                )
                    )
                ),
                box(title = "Bar Plot Summary",
                    width = 8, solidHeader = TRUE, status = "primary", 
                    plotOutput("barplot_summary")
                )
              )
      ),
      
      # Tab 2: Patient Information
      tabItem(tabName = "patient_info",
              fluidRow(
                box(title = "Enter Patient ID",
                    width = 4, solidHeader = TRUE, status = "primary",
                    selectInput("patient_id",
                                "Choose Patient ID",
                                choices = unique(mimic_icu_cohort$subject_id))
                ),
                box(title = "Patient ADT",
                    width = 8, solidHeader = TRUE, status = "primary",
                    plotOutput("patient_plot"))  # Render the plot here
              )
      )
    )
  )
)


# Server logic
server <- function(input, output, session) {
  
  # Tab 1: Numerical Summaries using gtsummary
  output$num_summary <- render_gt({
    # Summarize the data stratified by the `los_long` variable
    mimic_icu_cohort %>%
      tbl_summary(by = los_long) %>%
      as_gt() %>% 
      datatable()
  })
  
  # Tab 1: Bar Plot Summary based on selected variable
  output$barplot_summary <- renderPlot({
    req(input$barplot_var)  # Ensure that a variable is selected
    
    # Select the appropriate variable based on input
    var_name <- input$barplot_var
    plot_data <- mimic_icu_cohort %>%
      count(!!sym(var_name)) %>%
      rename(Variable = !!sym(var_name), Count = n)
    
    # Generate the bar plot
    ggplot(plot_data, aes(x = Variable, y = Count, fill = Variable)) +
      geom_bar(stat = "identity", color = "black") + # Unique fill color
      # print count above bar plot
      geom_text(aes(label = Count), vjust = -0.5, size = 4) +  
      theme_minimal() +
      labs(title = paste(var_name, "Summary"), x = var_name, y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Filter the data for the selected patient ID
  patient_data_filtered <- reactive({
    req(input$patient_id)  # Ensure that a patient_id is selected
    mimic_icu_cohort %>% filter(subject_id == input$patient_id)
  })
  
  # Get patient demographic info (for title)
  patient_info <- reactive({
    req(input$patient_id)
    patient_data <- patient_data_filtered()
    # Extract patient demographics
    demographic_info <- patient_data %>%
      summarise(
        gender = first(gender),
        age = first(age),
        race = first(race),
        insurance = first(insurance)
      )
    return(demographic_info)
  })
  
  
  # Tab 2: Render the ADT or ICU plot based on user selection
  output$patient_plot <- renderPlot({
    
    # Filter the data for the selected patient
    patient_data <- patient_data_filtered()
    
    # Generate the ADT plot
    
    plot_title <- paste("Patient", input$patient_id, ",",
                        patient_info()$gender, ",",
                        patient_info()$age, "years old,",
                        patient_info()$race)
    
    ggplot(patient_data, aes(x = admittime, y = admission_type)) +
      geom_segment(aes(xend = dischtime, yend = admission_type,
                       color = admission_location, size = 4)) +
      geom_point(aes(x = admittime, y = admission_type, shape = "ADT"),
                 size = 4) +
      geom_point(aes(x = dischtime, y = admission_type, shape = "ADT"),
                 size = 4) +
      scale_color_manual(values = c("ICU" = "red", "CCU" = "blue",
                                    "Other" = "gray"),
                         name = "Care Unit") +
      scale_size_continuous(range = c(1, 3)) +
      # Circle shape for ADT events
      scale_shape_manual(values = c("ADT" = 16)) +  
      labs(title = plot_title,
           x = "Calendar Time", y = "Event Type",
           color = "Care Unit", size = "ICU/CCU Size") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  )}

# Run the app
shinyApp(ui, server)