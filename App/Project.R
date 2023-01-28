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
                       
                       numericInput("binom_n", "Numarul de experimente : ", value = 10, min = 1, max = 1000),
                       numericInput("binom_k", "Numarul de realizari : ", value = 5, min = 1, max = 1000),
                       sliderInput("binom_p", "Probabilitatea de realizare :", min = 0, max = 1, value = 0.5, step = 0.01),
                       radioButtons("binom_func", "Selectati Histograma", choices = c("Functia de repartitie", 
                                                                                "Functia de masa")
                       ),
                       numericInput("a", "a = ", value = 10),
                       numericInput("b", "b = ", value = 10)
                       
                       
                     ),
                     
                   
                   mainPanel(
                     conditionalPanel(condition = "input.binom_func == 'Functia de repartitie'",
                     plotOutput("binom_cdf")
                     ),
                     conditionalPanel(condition = "input.binom_func == 'Functia de masa'",
                                      plotOutput("binom_pmf")
                                      
                   )
                 )
  )
),
  conditionalPanel(condition = "input.repart == 'Bernoulli'",
                 sidebarLayout(
                   sidebarPanel(

                     sliderInput("bern_p", "Probabilitatea de realizare :", min = 0, max = 1, value = 0.5, step = 0.01),
                     radioButtons("bern_func", "Selectati Histograma", choices = c("Functia de repartitie",
                                                                              "Functia de masa")
                     ),
                     numericInput("a", "a = ", value = 10),
                     numericInput("b", "b = ", value = 10)


                   ),


                   mainPanel(
                     conditionalPanel(condition = "input.bern_func == 'Functia de repartitie'",
                                      plotOutput("bern_cdf")
                     ),
                     conditionalPanel(condition = "input.bern_func == 'Functia de masa'",
                                      plotOutput("bern_pmf")

                     )
                   )
                 )
)

)



server <- function(input, output) {
  
  #Binomial distribution
  output$binom_cdf <- renderPlot({
    n <- input$binom_n
    p <- input$binom_p
    k <- input$binom_k
    x <- 0:n
    cdf <- pbinom(x, size = n, prob = p)
    plot(x, cdf, type = "l", lwd = 2, 
         xlab = "Number of Successes", 
         ylab = "Cumulative Probability",
         main = paste("Binomial CDF (n =", n, ", p =", p, ")")
    )
    abline(v = k, lty = 2)     })
  output$binom_pmf <- renderPlot({
    n <- input$binom_n
    p <- input$binom_p
    k <- input$binom_k
    x <- 0:n
    pmf <- dbinom(x, size = n, prob = p)
    plot(x, pmf, type = "h", lwd = 2, 
         xlab = "Number of Successes", 
         ylab = "Probability",
         main = paste("Binomial PMF (n =", n, ", p =", p, ")")
    )
    abline(v = k, lty = 2)
  })
  
  #Bernoulli distribution
  output$bern_cdf <- renderPlot({
    prob <- input$bern_p
    x <- c(0,1)
    y <- c(1-prob, prob)
    matplot(x, y, type = 's', xlab = "x", ylab = "P(X <= x)", main = "Bernoulli CDF")
  })
  output$bern_pmf <- renderPlot({
    prob <- input$bern_p
    x <- c(0,1)
    y <- c(1-prob, prob)
    barplot(y, names.arg=x, xlab = "x", ylab = "P(X = x)", main = "Bernoulli PMF")
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)