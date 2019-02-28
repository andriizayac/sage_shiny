#library(shiny)
#library(ggplot2)

ui=fluidPage(  
  titlePanel("Title name"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", label = "Price", min=1,max=100,value=c(25,40),pre="$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      #selectInput("countryInput", "Country",
      #            choices = c("CANADA", "FRANCE", "ITALY")),
      uiOutput("countryOutput")
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
    )
  ))

server=function(input,output){
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  filtered <- reactive({
    bcl %>%
      filter(CURRENT_DISPLAY_PRICE >= input$priceInput[1],
             CURRENT_DISPLAY_PRICE <= input$priceInput[2],
             PRODUCT_CLASS_NAME == input$typeInput,
             PRODUCT_COUNTRY_ORIGIN_NAME == input$countryInput
      )
  })
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(PRODUCT_ALCOHOL_PERCENT)) +
      geom_histogram()
  })
  output$results = renderTable({
    filtered()
  })
}
shinyApp(ui=ui,server=server)




