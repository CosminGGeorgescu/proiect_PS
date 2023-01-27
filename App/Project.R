library(shiny)
#Define UI ----

ui <- fluidPage(
  
  titlePanel("Repartiile in practica"),
  
  selectInput("repart", "Selectati repartitia", choices= c("Binomiala", 
                                                           "Bernoulli",
                                                           "Negativ Binomiala",
                                                           "Poisson",
                                                           "Geometrica",
                                                           "Hipergeometrica")
  ),
  
  conditionalPanel(condition = "input.repart == 'Binomiala'",
                   sidebarLayout(
                     sidebarPanel(
                       
                       numericInput("n", "Numarul de experimente : ", value = 10, min = 1, max = 1000),
                       numericInput("k", "Numarul de realizari : ", value = 5, min = 1, max = 1000),
                       sliderInput("p", "Probabilitatea de realizare :", min = 0, max = 1, value = 0.5, step = 0.01),
                       radioButtons("func", "Selectati Histograma", choices = c("Functia de repartitie", 
                                                                                "Functia de masa")
                       ),
                       numericInput("a", "a = ", value = 10),
                       numericInput("b", "b = ", value = 10)
                       
                       
                     ),
                     
                   
                   mainPanel(
                     conditionalPanel(condition = "input.func == 'Functia de repartitie'",
                     plotOutput("cdf")
                     ),
                     conditionalPanel(condition = "input.func == 'Functia de masa'",
                                      plotOutput("pmf")
                                      
                   )
                 )
  )
)
)



server <- function(input, output) {
  output$cdf <- renderPlot({
    n <- input$n
    p <- input$p
    k <- input$k
    x <- 0:n
    cdf <- pbinom(x, size = n, prob = p)
    plot(x, cdf, type = "l", lwd = 2, 
         xlab = "Number of Successes", 
         ylab = "Cumulative Probability",
         main = paste("Binomial CDF (n =", n, ", p =", p, ")")
    )
    abline(v = k, lty = 2)     })
  output$pmf <- renderPlot({
    n <- input$n
    p <- input$p
    k <- input$k
    x <- 0:n
    pmf <- dbinom(x, size = n, prob = p)
    plot(x, pmf, type = "h", lwd = 2, 
         xlab = "Number of Successes", 
         ylab = "Probability",
         main = paste("Binomial PMF (n =", n, ", p =", p, ")")
    )
    abline(v = k, lty = 2)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)