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
                  title = "Sales & Price Distribution",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("desc_plot")
                )
                
              ),
              fluidRow(
                box(
                  title = "Price vs Sales Correlation",
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("correlation_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Predicted vs Actual Sales",
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("predicted_vs_actual")
                )
              ),
              fluidRow(
                box(
                  title = "Recommendations",
                  width = 12,
                  status = "danger",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  pre(verbatimTextOutput("recommendations"))
                )
              )
      ),
      
      tabItem(tabName = "about",
              h2("About the Zara Sales Dashboard"),
              br(),
              p("This interactive dashboard provides a comprehensive snapshot of Zara's product performance using sales data."),
              p("It was designed to support better business decisions by making data insights more visual, predictive, and actionable."),
              
              h4("🔍 Key Features:"),
              tags$ul(
                tags$li(strong("Overview Tab: "), "Displays interactive charts including: 
                   a pie chart titled 'Sales by Store Section', 
                   a bar chart titled 'Sales by Product Category', 
                   and a ranked list titled 'Top 10 Best-Selling Products'."),
                tags$li(strong("Sales & Price Distribution: "), 
                        "Visualizes the distribution of sales volume and product price using boxplots grouped by store section."),
                tags$li(strong("Price vs Sales Correlation: "), 
                        "Illustrates the correlation between product pricing and sales volume using a bar plot to highlight strength and direction."),
                tags$li(strong("Predicted vs Actual Sales: "), 
                        "Displays the predictive performance of a linear regression model by comparing predicted and actual sales in a scatter plot."),
                tags$li(strong("Recommendations: "), 
                        "Provides actionable recommendations based on observed performance, patterns, and prediction results.")
              ),
              
              h4("🎯 Purpose:"),
              p("This dashboard was created to empower Zara’s retail strategy by providing clear visual cues, intelligent predictions, and actionable recommendations based on real-world data."),
              
              h4("📊 Dataset Information:"),
              p("The dataset includes product-level information such as price, brand, promotional status, section, and recorded sales volume."),
              p("All data is based on a single snapshot date (February 19), meaning analysis reflects a cross-sectional view."),
              
              h4("📁 Data Source:"),
              tags$p(
                "Dataset used in this dashboard was sourced from ",
                a("Kaggle - Zara Sales Data", href = "https://www.kaggle.com/datasets/xontoloyo/data-penjualan-zara", target = "_blank"),
                "."
              )
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
      layout(title = "",
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
      labs(title = "", x = "Category", y = "Units Sold")
    
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
      labs(title = "", x = "Product", y = "Units Sold")
    
    ggplotly(p)
  })
  
  output$desc_plot <- renderPlotly({
    df <- zara_data %>%
      select(sales_volume, price, section) %>%
      drop_na()
    
    p <- ggplot(df, aes(x = section)) +
      geom_boxplot(aes(y = sales_volume, fill = "Sales Volume"), alpha = 0.6) +
      geom_boxplot(aes(y = price, fill = "Price"), alpha = 0.6, position = position_dodge(width = 0.75)) +
      scale_fill_manual(values = c("Sales Volume" = "skyblue", "Price" = "salmon")) +
      labs(title = "",
           x = "Section",
           y = "Value",
           fill = "") +
      theme_minimal()
    
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
      labs(title = "")
    
    ggplotly(p)
  })
  
  output$predicted_vs_actual <- renderPlotly({
    p <- ggplot(model_data, aes(x = sales_volume, y = predicted_sales)) +
      geom_point(color = "steelblue") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(title = "", x = "Actual", y = "Predicted")
    
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
