
ui <- fluidPage(
  titlePanel("ANOVA Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("mean1", "Mean 1:", value = 16),
      numericInput("mean2", "Mean 2:", value = 14),
      numericInput("mean3", "Mean 3:", value = 15),
      numericInput("sd1", "Standard Deviation 1:", value = 2.3),
      numericInput("sd2", "Standard Deviation 2:", value = 2.4),
      numericInput("sd3", "Standard Deviation 3:", value = 2.7),
      numericInput("n1", "Sample Size 1:", value = 3),
      numericInput("n2", "Sample Size 2:", value = 2),
      numericInput("n3", "Sample Size 3:", value = 15),
      actionButton("calculate", "Calculate"),
      hr(),
      h4("ANOVA Results:"),
      textOutput("fvalue"),
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
    var3 <- input$sd3^2
    
    
    
    
    #################.     anova p value 
    
    allmean = mean(c(input$mean1,input$mean2, input$mean3))
    ssr = input$n1*( input$mean1 - allmean)^2 + input$n2 *(input$mean2- allmean)^2 + input$n3 * (input$mean3 - allmean)^2
    sse = (input$n1 - 1)*var1 + (input$n2 - 1)* var2 + (input$n3	- 1)*var3
    sst = ssr + sse
    tr = length(c(input$n1,input$n2, input$n3)) -1
    err = sum(c(input$n1,input$n2, input$n3)) - tr
    mst = ssr/tr
    mserr = sse/ err
    f = mst/mserr
    pval = pf(f, tr, err, lower.tail = FALSE)
    
    ##################
    output$pvalue <- renderText(paste("p-value:", round(pval,4)))
  })
}

shinyApp(ui = ui, server = server)
