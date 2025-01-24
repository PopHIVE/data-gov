library(shiny)
library(tidyverse)
library(janitor)

data_url <- "https://raw.githubusercontent.com/ysph-dsde/data-gov/refs/heads/main/Opioid%20OD%20Data/Harmonized%20Opioid%20Overdose%20Datasets_01.23.2025.csv"

df <- read_csv(data_url) |>
  clean_names() |> 
  filter(!(count %in% c(7777, 8888, 9999))) |> 
  filter(!(crude_rate %in% c(7777, 8888, 9999))) |> 
  filter(!(age_adjusted_rate %in% c(7777, 8888, 9999)))
 
  



# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Prototype App"),
  hr(),
  tabsetPanel(
    tabPanel("Opioids",
             br(),
             fluidRow(
               column(4,
                      selectInput("by_year_quarter",
                                  "Plot By:",
                                  c("Year" = "year",
                                    "Quarter" = "quarter")),
                      checkboxGroupInput("data_source",
                                         "Data Source(s):",
                                         choices = NULL),
                      selectInput("state",
                                  "State:",
                                  choices = NULL),
                      selectInput("setting",
                                  "Setting:",
                                  choices = NULL),
                      selectInput("underlying_cause_of_death",
                                  "Underlying Cause of Death:",
                                  choices = NULL),
                      selectInput("drug",
                                  "Drug:",
                                  choices = NULL),
                      selectInput("stratify_by", 
                                  "Stratify By:",
                                  choices = NULL),
                      selectInput("measure_type", "Measure Type",
                                  c("Count" = "count",
                                    "Crude Rate" = "crude_rate",
                                    "Age-Adjusted Rate" = "age_adjusted_rate"))
               ),
               column(8,
                      plotOutput("time_series")
               )
               
             ),
             includeHTML("footnotes.html")
    ),
    tabPanel("Respiratory Syncytial Virus (RSV)")
  ),
  
)

# Define server logic
server <- function(input, output, session) {
  
  # Observer to update data_source based on by_year_quarter
  observeEvent(input$by_year_quarter, {
    if (input$by_year_quarter == "quarter") {
      updateCheckboxGroupInput(session, "data_source",
                               choices = c("AHRQ", "CDC WONDER"))
    } else {
      updateCheckboxGroupInput(session, "data_source",
                               choices = unique(df$dataset))
    }
  })
  
  # Data Source
  data_source <- reactive({
    filter(df, dataset %in% input$data_source)
  })
  observeEvent(data_source(), {
    choices <- unique(data_source()$state)
    updateSelectInput(inputId = "state", choices = choices)
  })
  
  # State
  state <- reactive({
    req(input$state)
    filter(data_source(), state == input$state)
  })
  observeEvent(state(), {
    choices <- unique(state()$setting)
    updateSelectInput(inputId = "setting", choices = choices)
  })
  
  # Setting
  setting <- reactive({
    req(input$setting)
    filter(state(), setting == input$setting)
  })
  observeEvent(setting(), {
    choices <- unique(setting()$underlying_cause_of_death)
    updateSelectInput(inputId = "underlying_cause_of_death", choices = choices)
  })
  
  # Underlying Cause of Death
  underlying_cause_of_death <- reactive({
    req(input$underlying_cause_of_death)
    filter(setting(), underlying_cause_of_death == input$underlying_cause_of_death)
  })
  observeEvent(underlying_cause_of_death(), {
    choices <- unique(underlying_cause_of_death()$drug)
    updateSelectInput(inputId = "drug", choices = choices)
  })
  
  # Drug
  drug <- reactive({
    req(input$drug)
    filter(underlying_cause_of_death(), drug == input$drug)
  })
  observeEvent(drug(), {
    choices <- unique(drug()$characteristic)
    updateSelectInput(inputId = "stratify_by", choices = choices)
  })
  
  # Characteristic (stratify_by)
  stratify_by <- reactive({
    req(input$stratify_by)
    filter(drug(), characteristic == input$stratify_by)
  })
  # stratify_by = Final data to plot: 
  
  output$time_series <- renderPlot({
    req(input$data_source)
    req(input$stratify_by)
    
    measure_type_title <- stringr::str_to_title(gsub("_", " ", input$measure_type))
    
    # Plot By Quarter  
    if (input$by_year_quarter == "quarter") {
      # Not Stratified
      if (input$stratify_by == "Not Stratified") {
        
        df |> 
          filter(dataset %in% input$data_source,
                 state == input$state,
                 setting == input$setting,
                 underlying_cause_of_death == input$underlying_cause_of_death,
                 drug == input$drug,
                 characteristic == input$stratify_by) |> 
          filter(!is.na(quarter)) |> 
          mutate(year_quarter = paste(year, quarter)) |> 
          ggplot(aes(x = year_quarter, y = .data[[input$measure_type]], 
                     group = dataset, color = dataset)) +
          geom_line() +
          theme_light() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.title = element_blank()) +
          labs(title = paste0("Distribution of ", measure_type_title), 
               x = NULL,
               y = measure_type_title)
        
        # Stratified  
      } else {
        
        df |> 
          filter(dataset %in% input$data_source,
                 state == input$state,
                 setting == input$setting,
                 underlying_cause_of_death == input$underlying_cause_of_death,
                 drug == input$drug,
                 characteristic == input$stratify_by) |> 
          filter(!is.na(quarter)) |> 
          mutate(year_quarter = paste(year, quarter)) |> 
          ggplot(aes(x = year_quarter, y = .data[[input$measure_type]], 
                     group = dataset, color = dataset)) +
          geom_line() +
          facet_wrap(vars(level)) +
          theme_light() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.title = element_blank()) +
          labs(title = paste0("Distribution of ", measure_type_title), 
               x = NULL,
               y = measure_type_title)
        
      }
      
      
      # Plot By Year  
    } else { 
      # Not Stratified
      if (input$stratify_by == "Not Stratified") {
        
        df |> 
          filter(dataset %in% input$data_source,
                 state == input$state,
                 setting == input$setting,
                 underlying_cause_of_death == input$underlying_cause_of_death,
                 drug == input$drug,
                 characteristic == input$stratify_by) |> 
          filter(is.na(quarter)) |> 
          ggplot(aes(x = year, y = .data[[input$measure_type]], 
                     group = dataset, color = dataset)) +
          geom_line() +
          theme_light() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.title = element_blank()) +
          labs(title = paste0("Distribution of ", measure_type_title), 
               x = NULL,
               y = measure_type_title)
        
        # Stratified
      } else {
        
        df |> 
          filter(dataset %in% input$data_source,
                 state == input$state,
                 setting == input$setting,
                 underlying_cause_of_death == input$underlying_cause_of_death,
                 drug == input$drug,
                 characteristic == input$stratify_by) |> 
          filter(is.na(quarter)) |> 
          ggplot(aes(x = year, y = .data[[input$measure_type]], 
                     group = dataset, color = dataset)) +
          geom_line() +
          facet_wrap(vars(level)) +
          theme_light() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.title = element_blank()) +
          labs(title = paste0("Distribution of ", measure_type_title), 
               x = NULL,
               y = measure_type_title)
        
      }
      
    }
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
