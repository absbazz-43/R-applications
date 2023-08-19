library(shiny)

############.  


ui <- fluidPage(
  titlePanel("Two sample t-test Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("mean1", "Mean 1:", value = 2.92),
      numericInput("mean2", "Mean 2:", value = 3.50),
      numericInput("sd1", "Standard Deviation 1:", value = 1.31),
      numericInput("sd2", "Standard Deviation 2:", value = 1.43),
      numericInput("n1", "Sample Size 1:", value = 10),
      numericInput("n2", "Sample Size 2:", value = 11),
      actionButton("calculate", "Calculate"),
      hr(),
      h4("t test  Results:"),
      textOutput("pvalue")
    ),
    
    mainPanel(
      
    )
  )
)

server <- function(input, output) {
  observeEvent(input$calculate, {
    # Step 2: Calculate variance
    var1 <- input$sd1^2
    var2 <- input$sd2^2
    
    ####### trial
    pooled = ((input$n1 - 1)*var1 + (input$n2 - 1) * var2)/(input$n1 + input$n2 - 2)
    sp = sqrt(pooled*(1/input$n1 + 1/input$n2))
    dfn = (input$n1 + input$n2 - 2)
    tstat = (input$mean1 -input$mean2 ) / sp
    pval = 2* pt(abs(tstat), df = dfn , lower.tail = FALSE )
    
    # Output the results
    output$pvalue <- renderText(paste("p-value:", round(pval,4)))
  })
}

shinyApp(ui = ui, server = server)





