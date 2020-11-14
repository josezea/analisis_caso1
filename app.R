#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(TeachingSampling)
data(Lucy)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Análisis empresas Lucy"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "boton1", label = "Seleccione la variable:",
                        choices = c("Income", "Employees", "Taxes")),
            sliderInput("boton2",
                        "Numero de clases del histograma:",
                        min = 1,
                        max = 50,
                        value = 30),
            br(), 
            br(),
            tableOutput(outputId = "TablaLucy") 
),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "salida1"), 
            verbatimTextOutput(outputId = "salida2"),
            plotOutput(outputId = "salida3")
            
        )
    )
)





server <- function(input, output) {
    
    output$salida1 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- Lucy[[input$boton1]]     # Lucy[["Income"]] equivalente a Lucy$Income
        bins <- seq(min(x), max(x), length.out = input$boton2 + 1) # lengt.out es el numero de clases del histograma
        #bins <- seq(min(Lucy$Income), max(Lucy$Income), length.out = 5)
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white', xlab = input$boton1,
             ylab = "Frecuencia")
    })
    
    
    output$salida2 <- renderPrint({
        summary(Lucy[[input$boton1]])
    })    
    
    
    output$salida3 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- Lucy[[input$boton1]]     # Lucy[["Income"]] equivalente a Lucy$Income
        #bins <- seq(min(Lucy$Income), max(Lucy$Income), length.out = 5)
        # draw the histogram with the specified number of bins
        plot(x, Lucy$Income, col = 'blue', border = 'white', ylab = "Income", xlab = input$boton1)
    })

    
    output$TablaLucy <- renderTable({
    head(Lucy)    
    })
        
}
# Run the application 
shinyApp(ui = ui, server = server)
