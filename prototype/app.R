library(shiny)
library(tidyverse)
library(readxl)

df <- read_xlsx("test-data.xlsx", sheet = "Data")

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
                                         c("CDC WONDER" = "cdc_wonder",
                                           "CDC DOSE" = "cdc_dose",
                                           "CDC SUDORS" = "cdc_sudors",
                                           "AHRQ" = "ahrq")),
                      selectInput("stratify_by", "Stratify By:",
                                  c("Age" = "age",
                                    "Race" = "race",
                                    "State" = "state"))
               ),
               column(8,
                      plotOutput("hist")
               )
               
             )
    ),
    tabPanel("Respiratory Syncytial Virus (RSV)")
  )
)

# Define server logic
server <- function(input, output) {
  output$hist <- renderPlot({
    df |> 
      filter(Jurisdiction == "Connecticut") |> 
      ggplot(aes(year, alldrug_deaths)) +
      geom_point(size = 2.5) +
      geom_line() +
      labs(title = "Demo Plot",
           x = NULL,
           y = "Number of drug overdose deaths") +
      theme_light()
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
