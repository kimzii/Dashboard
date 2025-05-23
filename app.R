library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(caret)
library(forecast)
library(lubridate)
library(readr)
library(janitor)
library(dplyr)

# Load and clean data
library(readr)
library(janitor)
library(dplyr)

zara_data <- read.csv("zara.csv", sep = ";", quote = "\"", stringsAsFactors = FALSE) %>%
  janitor::clean_names()



# UI
ui <- dashboardPage(
  dashboardHeader(title = "Zara Business Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Analytics", tabName = "analytics"),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    tabItems(
      # Tab 1: Overview
      tabItem(tabName = "overview",
              fluidRow(
                box(selectInput("category", "Select Section", choices = unique(zara_data$section), selected = unique(zara_data$section)[1]), width = 4),
                box(plotlyOutput("sales_trend"), width = 8)
              ),
              fluidRow(
                box(plotlyOutput("top_products"), width = 12)
              )
      ),
      
      # Tab 2: Analytics
      tabItem(tabName = "analytics",
              fluidRow(
                box(title = "Descriptive Analytics", width = 6, status = "primary",
                    verbatimTextOutput("desc_summary")),
                box(title = "Diagnostic Analytics", width = 6, status = "warning",
                    plotlyOutput("correlation_plot"))
              ),
              fluidRow(
                box(title = "Predictive Analytics", width = 6, status = "success",
                    plotlyOutput("forecast_plot")),
                box(title = "Prescriptive Analytics", width = 6, status = "danger",
                    verbatimTextOutput("recommendations"))
              )
      ),
      
      # Tab 3: About
      tabItem(tabName = "about",
              h3("Zara Sales Dashboard"),
              p("This dashboard explores Zara's sales data with the goal of enhancing business decision-making."),
              p("Key questions answered:"),
              tags$ul(
                tags$li("Which sections and products perform best?"),
                tags$li("How do sales trends evolve over time?"),
                tags$li("What sales can we expect in the near future?"),
                tags$li("What strategies can improve future performance?")
              ),
              p("Dataset source:"),
              a("Kaggle - Zara Sales Data", href = "https://www.kaggle.com/datasets/xontoloyo/data-penjualan-zara")
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    zara_data %>% filter(section == input$category)
  })
  
  output$sales_trend <- renderPlotly({
    p <- filtered_data() %>%
      group_by(scraped_at) %>%
      summarise(total_sales = sum(sales_volume, na.rm = TRUE)) %>%
      ggplot(aes(x = scraped_at, y = total_sales)) +
      geom_line(color = "darkred") +
      labs(title = "Sales Trend Over Time", x = "Date", y = "Units Sold")
    
    ggplotly(p)
  })
  
  
  output$top_products <- renderPlotly({
    p <- filtered_data() %>%
      group_by(name) %>%
      summarise(total = sum(sales_volume, na.rm = TRUE)) %>%
      top_n(10, total) %>%
      ggplot(aes(x = reorder(name, total), y = total)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 10 Best-Selling Products", x = "Product", y = "Units Sold")
    
    ggplotly(p)
  })
  
  
  output$desc_summary <- renderPrint({
    summary(zara_data %>% select(sales_volume, price))
  })
  
  output$correlation_plot <- renderPlotly({
    corr_val <- cor(zara_data$sales_volume, zara_data$price, use = "complete.obs")
    
    p <- ggplot(data.frame(Feature = "Price", Correlation = corr_val), aes(x = Feature, y = Correlation)) +
      geom_col(fill = "tomato") +
      ylim(-1, 1) +
      labs(title = "Correlation between Price and Sales Volume")
    
    ggplotly(p)
  })
  
  
  output$forecast_plot <- renderPlotly({
    ts_data <- filtered_data() %>%
      group_by(scraped_at) %>%
      summarise(total = sum(sales_volume, na.rm = TRUE))
    
    if (nrow(ts_data) < 15) {
      return(NULL)
    }
    
    ts_series <- ts(ts_data$total, frequency = 7)
    model <- auto.arima(ts_series)
    forecast_data <- forecast(model, h = 10)
    
    p <- autoplot(forecast_data) +
      labs(title = "Forecast of Sales", y = "Units Sold", x = "Future Days")
    
    ggplotly(p)
  })
  
  
  output$recommendations <- renderPrint({
    cat("Recommendations for Section:", input$category, "\n")
    cat("- Increase stock for top-selling items.\n")
    cat("- Consider promotions for low-selling or high-priced items.\n")
    cat("- Place top items at high-traffic store positions like End-cap.\n")
    cat("- Leverage forecast data to prepare for future demand.\n")
  })
}

shinyApp(ui, server)
