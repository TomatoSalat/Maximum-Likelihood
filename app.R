#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Example: Maximum-Likelihood-Estimation"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("N",
                        "Number of observations:",
                        min = 1,
                        max = 100,
                        value = 15),

            sliderInput('MLMean', 'ML-Mean', min = 0, max = 35, value =  0, step = 0.1),
            sliderInput('MLSd', 'ML-Sd', min = 0.1, max = 8, value = 1, step = 0.1)
        ),

        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("DataPlot"),
           plotOutput("MLEstimatePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    g_data <- reactive({        
        # generate some Data 

        })

    output$DataPlot <- renderPlot({
        set.seed(1534784)
        
        beta = c(5, 2)
        e = rnorm(input$N, sd = 0.5)
        
        X = matrix(c(rep(1,input$N), rchisq(input$N, 5)), ncol = 2)
        y = X %*% beta + e
        
        
        plot(y = rep(0,input$N), x = y, pch = 16, col = 2, lwd = 2, xlab = "Data", ylab = "", ylim = c(-0.1, 1))
        abline(h = 0, col = "lightgrey")
        points(y = rep(0,input$N), x = y, pch = 16, col = 2, cex=2)
        
        lines(density(y, bw = "SJ", kernel = "gaussian"), col = "grey95")
        
        borders = max(abs(c(min(y), max(y))))+1
        
        xML <- seq(-borders, borders, length=1000)
        yML <- dnorm(xML, mean=input$MLMean, sd=input$MLSd)
        
        lines(y = yML, x = xML, col = "green", lwd = 2)

    })
    
    output$MLEstimatePlot <- renderPlot({
        set.seed(1534784)
        
        beta = c(5,2)
        e = rnorm(input$N, sd = 0.5)
        
        X = matrix(c(rep(1,input$N), rchisq(input$N, 5)), ncol = 2)
        y = X %*% beta + e
        

        LL <- function(x = y, mu = input$MLMean, sigma = input$MLSd) {
            R = dnorm(x, mu, sigma)
            sum(log(R))
        }

        LL1 = LL(x = y, mu = input$MLMean, sigma = input$MLSd)
        
        borders = max(abs(c(min(y), max(y))))+1
        
        LLs = apply(matrix(seq(-borders,borders,by = 0.01)),1, FUN = LL, x = y,  sigma = input$MLSd)
        
        plot(y = rep(0,input$N), x = y, type = "n", xlab = "Data", ylab = "", ylim = c(-input$N*10, 20))
        lines(y = LLs, x = seq(-borders,borders,by = 0.01), col = "grey90" , lwd = 2)
        
        points(y = LL1 , x = input$MLMean, pch = 16 , cex = )
        

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
