library(shiny)
library(tidyverse)
library(plotly)
library(scales)
library(usmap)
library(cowplot)
library(janitor)

"%!in%" <- function(x,y)!("%in%"(x,y))

# Interactive
opioid_data <- "data/Harmonized Opioid Overdose Datasets_01.23.2025.csv"

df <- read_csv(opioid_data) |>
  clean_names() |> 
  filter(!(count %in% c(7777, 8888, 9999))) |> 
  filter(!(crude_rate %in% c(7777, 8888, 9999))) |> 
  filter(!(age_adjusted_rate %in% c(7777, 8888, 9999))) |> 
  # Filter out rows without Quarter
  filter(!is.na(quarter)) |> 
  # Create year_quarter column
  mutate(year_quarter = paste(year, quarter))

# Static
opioid_od <- read_csv(opioid_data) %>%
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
               column(3,
                      selectInput("state",
                                  "State:",
                                  choices = unique(df$state),
                                  selected = "US")
               ),
               column(6,
                      plotlyOutput("time_series_interactive")
               )
             ),
             br(),
             br(),
             fluidRow(
               column(6, 
                      plotOutput("time_series_static")),
               column(6, 
                      tabsetPanel(
                        tabPanel("2020", 
                                 br(),
                                 plotOutput("bar_graph_2020", width = "100%", height = "500px")),
                        tabPanel("2021",
                                 br(),
                                 br(),
                                 plotOutput("bar_graph_2021")),
                        tabPanel("2022",
                                 br(),
                                 br(),
                                 plotOutput("bar_graph_2022"))
                      )
               )
             ),
             fluidRow(
               h3("National Opioid Overdose Counts for All Opioid Types in 2022"),
               h4("Underlying Cause of Death: All Setting Medical Facility - Inpatient"),
               br(),
               column(6, plotlyOutput("ahrq_map")),     # AHRQ plot on the left
               column(6, plotlyOutput("wonder_map"))   # CDC WONDER plot on the right
             ),
             hr(),
             br(),
             includeHTML("footnotes.html")
    ),
    tabPanel("Respiratory Syncytial Virus (RSV)")
  ),
  
)

# Define server logic
server <- function(input, output, session) {
  
  df_time_series_interactive <- reactive({
    df %>%
      filter(drug=='All Opioids' & 
               characteristic=='Age' &
               state == input$state) |> 
      group_by(dataset, state, year_quarter, level) %>%
      summarize(count=sum(count, na.rm = TRUE)) |>
      ungroup() |> 
      pivot_wider(names_from = level, values_from = count) |> 
      mutate(Overall = `25-44 Years` + `45-64 Years` + `65+ Years` + `<24 Years`) |> 
      pivot_longer(
        cols = where(is.numeric),
        names_to = "level",
        values_to = "count"
      ) |> 
      mutate(level = factor(level),
             level = fct_relevel(level, c("Overall", "65+ Years",
                                          "45-64 Years", "25-44 Years",
                                          "<24 Years"))) |> 
      filter(count > 0) 
  })
  
  output$time_series_interactive <- renderPlotly({ 
    # User must provide inputs
    req(input$state)
    # Dataset must have rows
    req(nrow(df_time_series_interactive()) > 0)
    
    p1 <- df_time_series_interactive() %>%
      ggplot(aes(x = year_quarter, y = count, 
                 group = level, color = level,
                 text = paste0("Quarter: ", year_quarter, "\n",
                               "Count: ", count))) +
      geom_line() +
      labs(x = NULL,
           y = "Count",
           color = "Age Category") +
      scale_x_discrete(breaks = c("2016 Q1", "2017 Q1", "2018 Q1",
                                  "2019 Q1", "2020 Q1", "2021 Q1", 
                                  "2022 Q1")) +
      scale_y_continuous(labels = scales::comma) +
      facet_wrap(~dataset) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p1, tooltip = c("text"))
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
      theme_minimal(base_size = 17)
    
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
      mutate(Drug = if_else(Drug == "Mental and behavioural disorders due to use of opioids, acute intoxication",
                            "Mental and behavioural disorders",
                            Drug)) %>%
      
      # Plot settings and features.
      ggplot(data = ., aes(x = Drug, y = `Age Adjusted Rate`)) +
      geom_bar(stat = "identity", position = "dodge", aes(fill = Dataset, color = Dataset)) +
      labs(title = "National Opioid Overdose Rate by Types of Opioid and Polysubstance in 2020",
           subtitle = "Underlying Cause of Death: Unintentional Setting: All.",
           x = "", y = "Age-Adjusted Rate (per 100,000)") +
      theme_minimal(base_size = 17) + 
      theme(axis.text.x = element_text(angle = 90,  hjust = 1))
    
    
    
    
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
      mutate(Drug = if_else(Drug == "Mental and behavioural disorders due to use of opioids, acute intoxication",
                            "Mental and behavioural disorders",
                            Drug)) %>%
      
      # Plot settings and features.
      ggplot(data = ., aes(x = Drug, y = `Age Adjusted Rate`)) +
      geom_bar(stat = "identity", position = "dodge", aes(fill = Dataset, color = Dataset)) +
      labs(title = "National Opioid Overdose Rate by Types of Opioid and Polysubstance in 2021",
           subtitle = "Underlying Cause of Death: Unintentional Setting: All.",
           x = "", y = "Age-Adjusted Rate (per 100,000)") +
      theme_minimal(base_size = 16) +
      theme(axis.text.x = element_text(angle = 90,  hjust = 1))
    
    
    
    
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
      mutate(Drug = if_else(Drug == "Mental and behavioural disorders due to use of opioids, acute intoxication",
                            "Mental and behavioural disorders",
                            Drug)) %>%
      
      # Plot settings and features.
      ggplot(data = ., aes(x = Drug, y = `Age Adjusted Rate`)) +
      geom_bar(stat = "identity", position = "dodge", aes(fill = Dataset, color = Dataset)) +
      labs(title = "National Opioid Overdose Rate by Types of Opioid and Polysubstance in 2022",
           subtitle = "Underlying Cause of Death: Unintentional Setting: All.",
           x = "", y = "Age-Adjusted Rate (per 100,000)") +
      theme_minimal(base_size = 16) + 
      theme(axis.text.x = element_text(angle = 90,  hjust = 1))
    
  })
  
  
  
  output$ahrq_map <- renderPlotly({
    
    
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
      theme(legend.position = "",
            plot.title = element_text(size = 14),  # Increase title size
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.text = element_text(size = 12),               # Adjust legend text
            legend.title = element_text(size = 14))
    
    ggplotly(ahrq_map_plot)
    
  })
  
  
  output$wonder_map <- renderPlotly({
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
      theme(legend.position = "right",
            plot.title = element_text(size = 14),  # Increase title size
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.text = element_text(size = 14),               # Adjust legend text
            legend.title = element_text(size = 16))
    
    ggplotly(wonder_map_plot)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
