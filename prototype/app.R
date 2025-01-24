library(shiny)
library(tidyverse)
library(janitor)

data_url <- "https://raw.githubusercontent.com/ysph-dsde/data-gov/refs/heads/main/Opioid%20OD%20Data/Harmonized%20Opioid%20Overdose%20Datasets_01.23.2025.csv"

df <- read_csv(data_url) |>
  clean_names()

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
                      checkboxGroupInput("data_source",
                                         "Data Source(s):",
                                         unique(df$dataset)),
                      selectInput("state",
                                  "State:",
                                  unique(df$state)),
                      selectInput("setting",
                                  "Setting:",
                                  unique(df$setting)),
                      selectInput("drug",
                                  "Drug:",
                                  unique(df$drug)),
                      selectInput("stratify_by", "Stratify By:",
                                  unique(df$characteristic)),
                      selectInput("measure_type", "Measure Type",
                                  c("Count" = "count",
                                    "Crude Rate" = "crude_rate",
                                    "Age-Adjusted Rate" = "age_adjusted_rate"))
               ),
               column(8,
                      plotOutput("time_series")
               )
               
             )
    ),
    tabPanel("Respiratory Syncytial Virus (RSV)")
  ),
  includeHTML("footnotes.html")
)

# Define server logic
server <- function(input, output) {
  output$time_series <- renderPlot({
    req(input$data_source)
    
    measure_type_title <- stringr::str_to_title(gsub("_", " ", input$measure_type))
    
    df |> 
      filter(dataset %in% input$data_source,
             state == input$state,
             setting == input$setting,
             drug == input$drug,
             characteristic == input$stratify_by) |> 
      filter(!is.na(quarter)) |> 
      mutate(year_quarter = paste(year, quarter)) |> 
      ggplot(aes(x = year_quarter, y = .data[[input$measure_type]], group = 1)) +
      geom_line() +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste0("Distribution of ", measure_type_title), 
           x = NULL,
           y = measure_type_title)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
