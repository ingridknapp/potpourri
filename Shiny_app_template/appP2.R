#setwd("C:/Users/gudge16/CloudStation/R_class_2018/Lecture_10/Shiny_app_template")
#getwd()
library(shiny)
library(dplyr)
library(ggplot2)
bcl <- read.csv('bclData.csv', stringsAsFactors = F)
ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(sidebarPanel(
    sliderInput(
      "priceInput",
      "Price",
      min = 0,
      max = 100,
      value = c(25, 40),
      pre = "$"
    ),
    radioButtons(
      "typeInput",
      "Product type",
      choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
      selected = "WINE"
    ),
    uiOutput("countryOutput")
  ),
  mainPanel(plotOutput("coolplot"),
            br(), br(),
            tableOutput("results"))
))
server <- function(input, output, session) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)), selected = "CANADA")
  })
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    bcl %>%
      filter(
        Price >= input$priceInput[1],
        Price <= input$priceInput[2],
        Type == input$typeInput,
        Country == input$countryInput
      )
  })
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return() #  If our filtered list has nulls, don't include those
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })
  output$results <- renderTable({
    filtered <-
      bcl %>%
      filter(
        Price >= input$priceInput[1],
        Price <= input$priceInput[2],
        Type == input$typeInput,
        Country == input$countryInput
      )
    filtered
  })
}
shinyApp(ui = ui, server = server)
