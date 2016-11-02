library(rhandsontable)
library(shiny)

ui.app <- shinyUI(fluidPage(
  titlePanel('Gel Quantification'),
  
  sidebarLayout(
    sidebarPanel(
      h1('Marker'),
      numericInput('n.rows',
                   label = 'Number of rows',
                   value = 1),
      
      h1('Quantification'),
      numericInput('sample', label = 'Enter area of to be predicted band',
                   value = 0)
    ),
  
  mainPanel(
    h3('Marker'),
    p('First, define how many rows your measurement of the marker has and enter it on the left.'),
    p('Second, copy and paste from Fiji your measured data in the table below.'),
    rHandsontableOutput('marker'),
    br(),
    
    h3('Predicted mass'),
    textOutput('pred.mass'),
    
    br(),
    br(),
    h3('Plot'),
    plotOutput('linreg'),
    br(),
    
    tableOutput('lin.res'),
    br()
  )
  )
))

server.app <- shinyServer(function(input, output){
  
  marker.ini <- reactive({
    mark <- data.frame('Row' = as.numeric(rep(NA, input$n.rows)),
                       'Area' = as.numeric(rep(NA, input$n.rows)),
                       'Mass' = as.numeric(rep(NA, input$n.rows)))
    return(mark)
  })
  
  output$marker <- renderRHandsontable(
  rhandsontable(marker.ini())
  )
  
  marker <- reactive({
    mark <- hot_to_r(input$marker)
  })
  
  output$linreg <- renderPlot({
    plot(marker()$Area~marker()$Mass, ylab = 'Area', xlab = 'Mass')
    abline(lm(marker()$Area~marker()$Mass))
  })
  
  lm.marker <- reactive({
    lm.m <- lm(marker()$Area~marker()$Mass)
    print(summary(lm.m))
    return(lm.m$coefficients)
  })
  
  output$lin.res <- renderTable({
    coeffs <- lm.marker()
    data.frame(Val = c('Intercept', 'Slope'),
               Coeff = coeffs)
  })
  
  output$pred.mass <- renderText({
    if(input$sample > 0){
      sample <- (input$sample - lm.marker()[1])/lm.marker()[2]
      sample <- round(sample, digits = 0)
      return(paste('Mass of: ', as.character(sample), sep=''))
    }else{
      return(NULL)
    }
  })
  
})

shinyApp(ui = ui.app, server = server.app)