library(shiny)
library(gapminder)
library(dplyr)
library(ggplot2)
library(scales)

# App link:
# https://p0liz.shinyapps.io/gapminder-data/

# Mapping labels to axes
axis_labels <- c(
  "lifeExp"   = "Life Expectancy (years)",
  "pop"       = "Population",
  "gdpPercap" = "GDP per capita ($ USD)"
)

ui <- fluidPage(
  titlePanel("Gapminder Data"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Year",
                  min = min(gapminder$year),
                  max = max(gapminder$year),
                  step = 5,
                  value = min(gapminder$year),
                  sep = ""),
      
      selectInput("continent", "Continent", 
                  choices = c("All", "Africa", "Americas", "Asia", "Europe", "Oceania"), 
                  selected = "All"),
      
      radioButtons("xinput", "X Axis",
                   choices = list("Life expectancy" = "lifeExp", 
                                  "Population" = "pop", 
                                  "GDP per capita" = "gdpPercap"), 
                   selected = "gdpPercap"),
      
      radioButtons("yinput", "Y Axis",
                   choices = list("Life expectancy" = "lifeExp", 
                                  "Population" = "pop", 
                                  "GDP per capita" = "gdpPercap"), 
                   selected = "lifeExp")
    ),
    
    mainPanel(
      plotOutput("scatterPlot", height = "550px")
    )
  )
)

server <- function(input, output) {
  
  # Reactive filtered dataset
  dataset <- reactive({
    df <- gapminder %>% filter(year == input$year)
    if (input$continent != "All") {
      df <- df %>% filter(continent == input$continent)
    }
    df
  })
  
  output$scatterPlot <- renderPlot({
    df <- dataset()
    
    p <- ggplot(df, aes(
        x = .data[[input$xinput]], 
        y = .data[[input$yinput]], 
        size = pop, 
        colour = country)) +
      geom_point(alpha = 0.7, show.legend = FALSE) +
      scale_colour_manual(values = country_colors) +
      scale_size(range = c(2, 22), guide = "none") +
      labs(
        title = paste("Year:", input$year),
        x = axis_labels[input$xinput],
        y = axis_labels[input$yinput]
      ) +
      theme_minimal(base_size = 14)
    
    # Apply log scale only when appropriate
    if (input$xinput != "lifeExp") {
      p <- p + scale_x_log10(labels = scales::comma)
    }
    if (input$yinput != "lifeExp") {
      p <- p + scale_y_log10(labels = scales::comma)
    }
    p
  })
}

shinyApp(ui = ui, server = server)
