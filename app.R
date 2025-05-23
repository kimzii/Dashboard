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

zara_data <- read.csv("zara.csv", sep = ";", quote = "\"", stringsAsFactors = FALSE) %>%
  janitor::clean_names()

# Preprocess data for predictive modeling
model_data <- zara_data %>%
  select(sales_volume, price, product_category, section, promotion, seasonal, brand) %>%
  drop_na()

# Convert character columns to factor
model_data <- model_data %>%
  mutate(across(where(is.character), as.factor))

# Remove factor columns with only one level
model_data <- model_data %>%
  select(where(function(col) {
    if (is.factor(col)) {
      nlevels(col) > 1
    } else {
      TRUE
    }
  }))


set.seed(123)
model <- train(sales_volume ~ ., data = model_data, method = "lm")
model_data$predicted_sales <- predict(model, newdata = model_data)

# UI
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center; justify-content: center; width: 100%; padding-top: 5px",
      tags$img(src = "zaralogo.png", height = "40px")
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Analytics", tabName = "analytics"),
      menuItem("About", tabName = "about")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .main-sidebar {
        position: fixed;
        height: 100%;
        overflow: hidden;
      }
      .sidebar {
        overflow-y: hidden !important;
      }
      .content-wrapper, .right-side {
        margin-left: 230px;
        overflow-y: auto;
        height: 100vh;
      }
    "))
    ),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Sales by Store Section",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  collapsible = TRUE,
                  plotlyOutput("sales_by_section")
                ),
                box(
                  title = "Sales by Product Category",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  collapsible = TRUE,
                  plotlyOutput("sales_by_category")
                )
              ),
              fluidRow(
                box(
                  title = "Top 10 Best-Selling Products",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  plotlyOutput("top_products")
                )
              )
      ),
        
      
      tabItem(tabName = "analytics",
              fluidRow(
                box(
                  title = "Descriptive Analytics Summary",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  pre(verbatimTextOutput("desc_summary"))
                )
              ),
              fluidRow(
                box(
                  title = "Diagnostic Analytics: Price vs Sales Correlation",
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("correlation_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Predictive Analytics: Predicted vs Actual Sales",
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("predicted_vs_actual")
                )
              ),
              fluidRow(
                box(
                  title = "Prescriptive Recommendations",
                  width = 12,
                  status = "danger",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  pre(verbatimTextOutput("recommendations"))
                )
              )
      ),
      
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
  
  output$sales_by_section <- renderPlotly({
    df <- zara_data %>%
      group_by(section) %>%
      summarise(total_sales = sum(sales_volume, na.rm = TRUE)) %>%
      arrange(desc(total_sales))
    
    plot_ly(df, labels = ~section, values = ~total_sales, type = 'pie') %>%
      layout(title = "Sales Distribution by Section",
             legend = list(orientation = 'h'))
  })
  
  output$sales_by_category <- renderPlotly({
    df <- zara_data %>%
      group_by(product_category) %>%
      summarise(total_sales = sum(sales_volume, na.rm = TRUE)) %>%
      arrange(desc(total_sales))
    
    p <- ggplot(df, aes(x = reorder(product_category, total_sales), y = total_sales)) +
      geom_col(fill = "coral") +
      coord_flip() +
      labs(title = "Sales by Product Category", x = "Category", y = "Units Sold")
    
    ggplotly(p)
  })
  
  output$top_products <- renderPlotly({
    p <- zara_data %>%
      group_by(name) %>%
      summarise(total = sum(sales_volume, na.rm = TRUE)) %>%
      slice_max(total, n = 10) %>%
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
  
  output$predicted_vs_actual <- renderPlotly({
    p <- ggplot(model_data, aes(x = sales_volume, y = predicted_sales)) +
      geom_point(color = "steelblue") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Predicted vs Actual Sales Volume", x = "Actual", y = "Predicted")
    
    ggplotly(p)
  })
  
  output$recommendations <- renderPrint({
    cat("Recommendations based on snapshot data:\n")
    cat("- Increase stock for top-selling items.\n")
    cat("- Consider promotions for low-selling or high-priced items.\n")
    cat("- Place top items at high-traffic store positions like End-cap.\n")
    cat("- Use descriptive and diagnostic insights for inventory planning.\n")
    cat("- Consider predictive model insights to forecast demand based on product features.\n")
  })
}

shinyApp(ui, server)
