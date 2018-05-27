library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

input_dataset <- function(dataset) {
  db <- read.csv(dataset, header = TRUE, sep = ",") %>%
    gather(year, value, -c(Country.Name, Country.Code, Indicator.Name, Indicator.Code)) %>%
    mutate(year = as.numeric(str_extract(year, "[0-9]+"))) %>%
    na.omit()
}

gdp <- input_dataset("GDP.csv")
le <- input_dataset("Life Expectancy.csv")

ui <- fluidPage(
  
  titlePanel("Economic Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("var_select", "Select Variable:",
                   list("GDP Constant USD 2010" = "gdp_usd",
                        "Life Expectancy" = "ble")),
      sliderInput("year_range",
                  "1960-2017:",
                  min = 1960,
                  max = 2017,
                  value = c(1990,2017)),
      selectInput("country", "Select country:",
                  choices = gdp$Country.Name,
                  selected = 1)),
    
    mainPanel(
      plotOutput("distPlot"),
      dataTableOutput("mytable")
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({  
    var_selected <- switch(input$var_select,
                           gdp_usd = gdp,
                           ble = le,
                           gdp)
    x <- input$year_range
    
    var_selected <- filter(var_selected, Country.Name == input$country, year >= min(x), year <= max(x))
  })
  
  output$distPlot <- renderPlot({
    ptitle <- ifelse(input$var_select == "ble","Life Expectancy","GDP in Constant 2010 USD")
    
    p <- data() %>%
      ggplot(aes(x = year, y = value)) +
      geom_point() +
      geom_line() +
      labs(title = ptitle,
           subtitle = input$country,
           x = "Years",
           y = ptitle,
           caption = "Based on data from the World Bank")
    print(p)
  })
  
  output$mytable = renderDataTable({
    data()
  })
}

shinyApp(ui = ui, server = server)

