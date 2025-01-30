library(shiny)
library(tidyverse)
library(plotly)
library(scales)
library(usmap)
library(cowplot)
library(janitor)

"%!in%" <- function(x,y)!("%in%"(x,y))

# Define Yale branding colors
yale_colors <- c(
  "#00356B",
  "#286DC0",
  "#63AAFF",
  "#C4DDFC",
  "#DDDDDD"
)

yale_gradient <- c("#DDDDDD",
                   "#C4DDFC",
                   "#63AAFF",
                   "#286DC0",
                   "#00356B") # Adjust order for your desired gradient




# Opioid Dataset---
# Interactive
opioid_path <- "data/Harmonized Opioid Overdose Datasets_01.23.2025.csv"

df_opioid <- read_csv(opioid_path) |>
  clean_names() |> 
  filter(!(count %in% c(7777, 8888, 9999))) |> 
  filter(!(crude_rate %in% c(7777, 8888, 9999))) |> 
  filter(!(age_adjusted_rate %in% c(7777, 8888, 9999))) |> 
  # Filter out rows without Quarter
  filter(!is.na(quarter)) |> 
  # Create year_quarter column
  mutate(year_quarter = paste(year, quarter))

# Static
opioid_od <- read_csv(opioid_path) %>%
  as.data.frame()

# RSV-Net Dataset---
rsv_net_path <- "data/Harmonized RSV-NET_01.29.2025.csv"

df_rsv_net <- read_csv(rsv_net_path) |> 
  clean_names()


# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  # Application title
  titlePanel(title = img(src="ysph-logo.png", height = "50px"),
             windowTitle = "Prototype App"),
  hr(),
  tabsetPanel(
    tabPanel("Opioids Overdose",
             br(),
             fluidRow(
               column(3,
                      selectInput("state",
                                  "State:",
                                  choices = unique(df_opioid$state),
                                  selected = "US")
               ),
               column(6,
                      plotlyOutput("opioid_time_series_interactive_1")
               )
             ),
             br(),
             br(),
             fluidRow(
               column(6, 
                      plotlyOutput("opioid_time_series_interactive_2")),
               column(6, 
                      tabsetPanel(
                        tabPanel("2020", 
                                 br(),
                                 plotlyOutput("opioid_bar_graph_2020", width = "100%", height = "500px")),
                        tabPanel("2021",
                                 plotlyOutput("opioid_bar_graph_2021")),
                        tabPanel("2022",
                                 plotlyOutput("opioid_bar_graph_2022"))
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
    tabPanel("Respiratory Syncytial Virus (RSV)",
             br(),
             fluidRow(
               column(3,
                      selectInput("region_type",
                                  "Region Type:",
                                  choices = unique(df_rsv_net$region_type),
                                  selected = "State"),
                      selectInput("region",
                                  "Region:",
                                  choices = NULL,
                                  selected = "US")
               ),
               column(6,
                      plotlyOutput("rsv_net_time_series_interactive")
               )
             )
    )
  ),
  
)

# Define server logic
server <- function(input, output, session) {
  
  # TAB: Opioids Overdose----
  df_time_series_interactive <- reactive({
    df_opioid %>%
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
  
  output$opioid_time_series_interactive_1 <- renderPlotly({ 
    # User must provide inputs
    req(input$state)
    # Dataset must have rows
    req(nrow(df_time_series_interactive()) > 0)
    
    p1 <- df_time_series_interactive() %>%
      ggplot(aes(x = year_quarter, y = count, 
                 group = level, color = level,
                 text = paste0("Quarter: ", year_quarter, "\n",
                               "Count: ", scales::comma(count)))) +
      geom_line() +
      labs(x = NULL,
           y = "Count",
           color = "Age Category") +
      scale_x_discrete(breaks = c("2016 Q1", "2017 Q1", "2018 Q1",
                                  "2019 Q1", "2020 Q1", "2021 Q1", 
                                  "2022 Q1")) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(values = yale_colors) + # Apply Yale branding colors
      facet_wrap(~dataset) +
      theme_minimal(base_size = 15) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p1, tooltip = c("text"))
  }) 
  
  
  
  output$opioid_time_series_interactive_2 <- renderPlotly({
    
    p <- df_opioid %>%
      filter(drug=='All Opioids' & 
               characteristic=='Sex' &
               state == "US") %>%
      group_by(dataset, state, year_quarter, level) %>%
      summarize(count=sum(count, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(count > 0) %>%
      ggplot(aes(x = year_quarter, y = count, 
                 group = level, color = level,
                 text = paste0("Quarter: ", year_quarter, "\n",
                               "Count: ", scales::comma(count)))) +
      geom_line() +
      labs(x = NULL,
           y = "Count",
           color = NULL,
           title = "National Opioid Overdose Count") +
      scale_x_discrete(breaks = c("2016 Q1", "2017 Q1", "2018 Q1",
                                  "2019 Q1", "2020 Q1", "2021 Q1", 
                                  "2022 Q1")) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(values = yale_colors) + # Apply Yale branding colors
      facet_wrap(~dataset) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45,  hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$opioid_bar_graph_2020 <- renderPlotly({
    
    p <- opioid_od %>%
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
      ggplot(data = ., aes(x = Drug, y = `Age Adjusted Rate`,
                           text = paste0("Drug: ", Drug, "<br>",
                                         "Age Adjusted Rate: ", `Age Adjusted Rate`))) +
      geom_bar(stat = "identity", position = "dodge", aes(fill = Dataset, color = Dataset)) +
      labs(title = "National Opioid Overdose Rate\nby Types of Opioid and Polysubstance in 2020",
           subtitle = "Underlying Cause of Death: Unintentional Setting: All.",
           x = "", y = "Age-Adjusted Rate (per 100,000)") +
      scale_color_manual(values = yale_colors) + # Apply Yale branding colors
      scale_fill_manual(values = yale_colors) + # Apply Yale branding colors
      theme_minimal(base_size = 13) + 
      theme(axis.text.x = element_text(angle = 90,  hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$opioid_bar_graph_2021 <- renderPlotly({
    
    
    p <- opioid_od %>%
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
      ggplot(data = ., aes(x = Drug, y = `Age Adjusted Rate`,
                           text = paste0("Drug: ", Drug, "<br>",
                                         "Age Adjusted Rate: ", `Age Adjusted Rate`))) +
      geom_bar(stat = "identity", position = "dodge", aes(fill = Dataset, color = Dataset)) +
      labs(title = "National Opioid Overdose Rate by\nTypes of Opioid and Polysubstance in 2021",
           subtitle = "Underlying Cause of Death: Unintentional Setting: All.",
           x = "", y = "Age-Adjusted Rate\n(per 100,000)") +
      scale_color_manual(values = yale_colors) + # Apply Yale branding colors
      scale_fill_manual(values = yale_colors) + # Apply Yale branding colors
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 90,  hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$opioid_bar_graph_2022 <- renderPlotly({
    
    
    p <- opioid_od %>%
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
      ggplot(data = ., aes(x = Drug, y = `Age Adjusted Rate`,
                           text = paste0("Drug: ", Drug, "<br>",
                                         "Age Adjusted Rate: ", `Age Adjusted Rate`))) +
      geom_bar(stat = "identity", position = "dodge", aes(fill = Dataset, color = Dataset)) +
      labs(title = "National Opioid Overdose Rate\nby Types of Opioid and Polysubstance in 2022",
           subtitle = "Underlying Cause of Death: Unintentional Setting: All.",
           x = "", y = "Age-Adjusted Rate\n(per 100,000)") +
      scale_color_manual(values = yale_colors) + # Apply Yale branding colors
      scale_fill_manual(values = yale_colors) + # Apply Yale branding colors
      theme_minimal(base_size = 13) + 
      theme(axis.text.x = element_text(angle = 90,  hjust = 1))
    
    ggplotly(p, tooltip = "text")
    
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
      plot_usmap(data = ., values = "Count", color = "#00356B",) + 
      scale_fill_gradientn(
        colors = yale_gradient,   # Use Yale branding colors in gradient
        name = "Count (2022)", 
        labels = scales::comma    # Format labels with commas
      ) + 
      labs(title = "AHRQ") +
      theme(legend.position = "",
            plot.title = element_text(size = 14),  # Increase title size
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.text = element_text(size = 12),               # Adjust legend text
            legend.title = element_text(size = 14))
    
    # Add a custom tooltip with comma formatting
    ahrq_map_plot <- ahrq_map_plot +
      aes(text = paste0("State: ", state, "<br>",
                        "Count: ", scales::comma(Count)))
    
    # Convert to plotly
    ggplotly(ahrq_map_plot, tooltip = "text")
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
      plot_usmap(data = ., values = "Count", color = "#00356B",) + 
      scale_fill_gradientn(
        colors = yale_gradient,   # Use Yale branding colors in gradient
        name = "Count (2022)", 
        labels = scales::comma    # Format labels with commas
      ) + 
      labs(title = "\ \ \ \ \ \ CDC WONDER") +
      theme(legend.position = "right",
            plot.title = element_text(size = 14),  # Increase title size
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.text = element_text(size = 14),               # Adjust legend text
            legend.title = element_text(size = 16))
    
    # Add a custom tooltip with comma formatting
    wonder_map_plot <- wonder_map_plot +
      aes(text = paste0("State: ", state, "<br>",
                        "Count: ", scales::comma(Count)))
    
    ggplotly(wonder_map_plot, tooltip = "text")
    
  })
  
  
  # TAB: Respiratory Syncytial Virus (RSV)----
  region_type <- reactive({
    filter(df_rsv_net, region_type == input$region_type)
  })
  observeEvent(region_type(), {
    choices <- unique(region_type()$region)
    updateSelectInput(inputId = "region", choices = choices)
  })
  
  rsv_net_reactive <- reactive({
    df_rsv_net |> 
      filter(region_type == input$region_type, 
             region      == input$region,
             characteristic == 'Age') |> 
      select(region, region_type,
             season, week_observed,
             level, count) |>
      filter(!is.na(count)) |> 
      pivot_wider(names_from = level, values_from = count) |> 
      mutate(Overall = rowSums(across(c(`18-49 Years`, `50-64 Years`, 
                                        `65-74 Years`, `75+ Years`,
                                        `<1 Years`, `1-4 Years`,
                                        `5-17 Years`)), na.rm = TRUE)) |> 
      pivot_longer(
        cols = where(is.numeric),
        names_to = "level",
        values_to = "count"
      ) |> 
      filter(!is.na(count)) |> 
      mutate(level = factor(level),
             level = fct_relevel(level, c("Overall", "75+ Years",
                                          "65-74 Years", "50-64 Years",
                                          "18-49 Years", "5-17 Years",
                                          "1-4 Years", "<1 Years")))
  })
  
  output$rsv_net_time_series_interactive <- renderPlotly({ 
    # User must provide inputs
    req(input$region)
    # Dataset must have rows
    req(nrow(rsv_net_reactive()) > 0)
    
    p <- rsv_net_reactive() |> 
      ggplot(aes(x = week_observed, y = count, 
                 group = level, color = level,
                 text = paste0("Week: ", week_observed, "\n",
                               "Count: ", scales::comma(count)))) +
      geom_line() +
      scale_color_manual(values = c(yale_colors, 
                                    "#7634A6",
                                    "#00C288",
                                    "#D6EF4A")) + # Apply Yale branding colors
      labs(x = NULL,
           y = "Count",
           color = "Age Category") +
      theme_minimal(base_size = 15)
    
    ggplotly(p, tooltip = c("text"))
  }) 
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
