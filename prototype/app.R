library(shiny)
library(tidyverse)
library(scales)
library(usmap)
library(cowplot)
library(janitor)

"%!in%" <- function(x,y)!("%in%"(x,y))

# Interactive
data_url <- "https://raw.githubusercontent.com/ysph-dsde/data-gov/refs/heads/main/Opioid%20OD%20Data/Harmonized%20Opioid%20Overdose%20Datasets_01.23.2025.csv"

df <- read_csv(data_url) |>
  clean_names() |> 
  filter(!(count %in% c(7777, 8888, 9999))) |> 
  filter(!(crude_rate %in% c(7777, 8888, 9999))) |> 
  filter(!(age_adjusted_rate %in% c(7777, 8888, 9999)))

# Static
opioid_od <- read_csv("../Opioid OD Data/Harmonized Opioid Overdose Datasets_01.23.2025.csv") %>%
  as.data.frame()




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
                      plotOutput("time_series_interactive")
               )
               
             ),
             br(),
             h2("Static Plots"),
             br(),
             plotOutput("time_series_static"),
             tabsetPanel(
               tabPanel("2020", plotOutput("bar_graph_2020")),
               tabPanel("2021", plotOutput("bar_graph_2021")),
               tabPanel("2022", plotOutput("bar_graph_2022"))
             ),
             plotOutput("us_map"),
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
  
  output$time_series_interactive <- renderPlot({
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
  
  output$time_series_static <- renderPlot({
    
    opioid_od %>%
      # Filter the placeholder numerical values.
      filter(`Crude Rate` %!in% 7777 & `Crude Rate` %!in% 8888 & `Crude Rate` %!in% 9999) %>%
      
      # Filter the metadata settings.
      filter(State %in% "US", Quarter %in% NA, Drug %in% "All Opioids",
             Setting %in% "Medical Facility - Inpatient",
             `Underlying Cause of Death` %in% "All",
             Characteristic %in% "Not Stratified", Level %in% "N/A") %>%
      
      # Plot settings and features.
      ggplot(data = ., aes(x = Year, y = `Crude Rate`)) +
      geom_line(aes(color = Dataset)) +
      labs(title = "National Opioid Overdose Rate for All Types of Opioids",
           subtitle = "Underlying Cause of Death: All. Setting: Medical Facility - Inpatient.",
           x = "Year", y = "Crude Rate (per 100,000)") +
      theme_minimal()
    
  })
  
  output$bar_graph_2020 <- renderPlot({
    
    opioid_od %>%
      # Filter the placeholder numerical values.
      filter(`Age Adjusted Rate` %!in% 7777 & `Age Adjusted Rate` %!in% 8888 & `Age Adjusted Rate` %!in% 9999) %>%
      
      # Change the date named in the plot title.
      filter(Year %in% 2020) %>%
      
      # Filter the metadata settings.
      filter(State %in% "US", Quarter %in% NA, Setting %in% "All",
             `Underlying Cause of Death` %in% "Unintentional",
             Characteristic %in% "Not Stratified", Level %in% "N/A") %>%
      
      # Plot settings and features.
      ggplot(data = ., aes(x = Drug, y = `Age Adjusted Rate`)) +
      geom_bar(stat = "identity", position = "dodge", aes(fill = Dataset, color = Dataset)) +
      labs(title = "National Opioid Overdose Rate by Types of Opioid and Polysubstance in 2020",
           subtitle = "Underlying Cause of Death: Unintentional Setting: All.",
           x = "", y = "Age-Adjusted Rate (per 100,000)") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 55,  hjust = 1))
    
    
    
    
  })
  
  output$bar_graph_2021 <- renderPlot({
    
    
    opioid_od %>%
      # Filter the placeholder numerical values.
      filter(`Age Adjusted Rate` %!in% 7777 & `Age Adjusted Rate` %!in% 8888 & `Age Adjusted Rate` %!in% 9999) %>%
      
      # Change the date named in the plot title.
      filter(Year %in% 2021) %>%
      
      # Filter the metadata settings.
      filter(State %in% "US", Quarter %in% NA, Setting %in% "All",
             `Underlying Cause of Death` %in% "Unintentional",
             Characteristic %in% "Not Stratified", Level %in% "N/A") %>%
      
      # Plot settings and features.
      ggplot(data = ., aes(x = Drug, y = `Age Adjusted Rate`)) +
      geom_bar(stat = "identity", position = "dodge", aes(fill = Dataset, color = Dataset)) +
      labs(title = "National Opioid Overdose Rate by Types of Opioid and Polysubstance in 2020",
           subtitle = "Underlying Cause of Death: Unintentional Setting: All.",
           x = "", y = "Age-Adjusted Rate (per 100,000)") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 55,  hjust = 1))
    
    
    
    
  })
  
  output$bar_graph_2022 <- renderPlot({
    
    
    opioid_od %>%
      # Filter the placeholder numerical values.
      filter(`Age Adjusted Rate` %!in% 7777 & `Age Adjusted Rate` %!in% 8888 & `Age Adjusted Rate` %!in% 9999) %>%
      
      # Change the date named in the plot title.
      filter(Year %in% 2022) %>%
      
      # Filter the metadata settings.
      filter(State %in% "US", Quarter %in% NA, Setting %in% "All",
             `Underlying Cause of Death` %in% "Unintentional",
             Characteristic %in% "Not Stratified", Level %in% "N/A") %>%
      
      # Plot settings and features.
      ggplot(data = ., aes(x = Drug, y = `Age Adjusted Rate`)) +
      geom_bar(stat = "identity", position = "dodge", aes(fill = Dataset, color = Dataset)) +
      labs(title = "National Opioid Overdose Rate by Types of Opioid and Polysubstance in 2020",
           subtitle = "Underlying Cause of Death: Unintentional Setting: All.",
           x = "", y = "Age-Adjusted Rate (per 100,000)") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 55,  hjust = 1))
    
  })
  
  
  
  output$us_map <- renderPlot({
    
    
    # -----------------------------
    # US Map plot.
    
    # Code to find mathes between AHRQ and CDC WONDER
    #opioid_od[opioid_od$Dataset %in% "AHRQ", "Setting"] %>% unique()
    
    # Generate a side-by-side set of plots showing "Drug = All Opioids" counts
    # by state in 2022. Toggle the setting to show either inpatient or ER.
    
    
    # Counts in AHRQ dataset.
    ahrq_map_plot <- opioid_od %>%
      # Filter the placeholder numerical values.
      filter(Count %!in% 7777 & Count %!in% 8888 & Count %!in% 9999) %>%
      
      # Switch between the two settings, and change the subtitle name.
      filter(Setting %in% "Medical Facility - Inpatient") %>%
      #filter(Setting %in% "Medical Facility - Outpatient or ER") %>%
      
      # Filter the metadata settings.
      filter(Dataset %in% "AHRQ", State %in% datasets::state.name, 
             Year %in% 2022, Quarter %in% NA,
             `Underlying Cause of Death` %in% "All", Drug %in% "All Opioids",
             Characteristic %in% "Not Stratified", Level %in% "N/A") %>%
      
      # plot_usmap() requires specific nomenclature for the column with states.
      rename(state = State) %>%
      
      # Plot settings and features.
      plot_usmap(data = ., values = "Count", color = "red") + 
      scale_fill_continuous(
        low = "white", high = "red", name = "Count (2022)", label = scales::comma
      ) + 
      labs(title = "AHRQ") +
      theme(legend.position = "")
    
    
    
    # Counts in CDC WONDER dataset.
    wonder_map_plot <- opioid_od %>%
      # Filter the placeholder numerical values.
      filter(Count %!in% 7777 & Count %!in% 8888 & Count %!in% 9999) %>%
      
      # Switch between the two settings, and change the subtitle name.
      filter(Setting %in% "Medical Facility - Inpatient") %>%
      #filter(Setting %in% "Medical Facility - Outpatient or ER") %>%
      
      # Filter the metadata settings.
      filter(Dataset %in% "CDC WONDER", State %in% datasets::state.name, 
             Year %in% 2022, Quarter %in% NA,
             `Underlying Cause of Death` %in% "All", Drug %in% "All Opioids",
             Characteristic %in% "Not Stratified", Level %in% "N/A") %>%
      
      # plot_usmap() requires specific nomenclature for the column with states.
      rename(state = State) %>%
      
      # Plot settings and features.
      plot_usmap(data = ., values = "Count", color = "red") + 
      scale_fill_continuous(
        low = "white", high = "red", name = "Count (2022)", label = scales::comma
      ) + 
      labs(title = "\ \ \ \ \ \ CDC WONDER") +
      theme(legend.position = "right")
    
    
    
    # Compile plots to display side-by-side.
    plot_together <- plot_grid(ahrq_map_plot, wonder_map_plot, labels = "AUTO")
    
    # Generate the main title.
    title <- ggdraw() + 
      draw_label("National Opioid Overdose Counts for All Opioid Types in 2022", x = 0, y = 0.2, hjust = 0, vjust =1) +
      draw_label("Underlying Cause of Death: All Setting: Medical Facility - Inpatient.", x = 0, y = 0.1, hjust = 0, size = 12) +
      theme(plot.margin = margin(0, 0, 0, 7))
    
    # Display the plot with title.
    plot_grid(title, plot_together, ncol = 1, rel_heights = c(1, 1))
    
    
    
    
    
    
    
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
