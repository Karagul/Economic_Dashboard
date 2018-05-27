library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

data_setting <- function(a){
  x <- a %>%
    read.csv(header = TRUE, sep = ",") %>%
    gather(year, value, -c(Country.Code, Country.Name, Indicator.Name, Indicator.Code)) %>%
    mutate(year = as.numeric(str_extract(year, "[0-9]+"))) %>%
    filter(Country.Code == "DOM")
}

le <- data_setting("Life Expectancy.csv")
gdp <- data_setting("GDP.csv")

ui <- fluidPage(
  
  titlePanel("Gross Domestic Product (GDP)"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range",
                  "1990-2017:",
                  min = 1960,
                  max = 2017,
                  value = c(1960,2017)
      )
    ),
    
    mainPanel(
      plotOutput("main_plot",
                 height = "500px",
                 hover = "plot_hover"),
      verbatimTextOutput("info")
    )
  )
)

server <- function(input, output) {
  
  output$main_plot <- renderPlot({
    
    x <- input$year_range
    
    hp <- nearPoints(gdp, input$plot_hover, xvar = "year", yvar = "value")
    point <- hp$year
    
    if (is.null(point)) { point <- 2017.00}
    
    gdp_sub <- gdp[gdp$year == point, ]
    gdp_sub$value <- ifelse(gdp_sub$year == point, gdp_sub$value, "")
    
    #Plotting
    p <- gdp %>%
      filter(year >= min(x), year <= max(x))%>%
      ggplot(aes(x = year, y = value)) +
      geom_line(linetype = "dashed",
                color = "blue",
                size = 0.75) +
      geom_point() +
      labs(title = "GDP - Dominican Republic",
           subtitle = "Constant USD 2010",
           y = "GDP, constant USD 2010",
           x = "Year")+ 
      geom_text(aes(label = value), size = 5, data = gdp_sub)
    p
  })
  
  output$info <- renderPrint({
    #hp <- nearPoints(gdp, input$plot_hover, xvar = "year", yvar = "value")
    #hp$value
  })
}

shinyApp(ui = ui, server = server)