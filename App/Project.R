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
                       sliderInput("binom_p", "Probabilitatea de realizare :", min = 0, max = 1, value = 0.5, step = 0.01),
                       numericInput("binom_a", "a = ", value = 10),
                       numericInput("binom_b", "b = ", value = 10)
                       

                       
                     ),
                     
                   
                   mainPanel(
                     
                     plotOutput("binom_cdf"),
                     plotOutput("binom_pmf"),
                     plotOutput("binom_prob")
                                      
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
),

conditionalPanel(condition = "input.repart == 'Hipergeometrica'",
                 sidebarLayout(
                   sidebarPanel(
                     numericInput("hg_alb", "Numarul de bile albe : ", value = 50, min = 1, max = 100),
                     numericInput("hg_n", "Numarul de bile negre : ", value = 50, min = 1, max = 100),
                     numericInput("hg_k", "Numarul de bile extrase : ", value = 30, min = 1, max = 100),
                     numericInput("hg_a", "a = ", value = 50),
                     numericInput("hg_b", "b = ", value = 70)
                     
                     
                   ),
                   
                   
                   mainPanel(
                     
                     plotOutput("hg_cdf"),
                     plotOutput("hg_pmf"),
                     plotOutput("hg_prob")
                     
                   )
                 )
)


# conditionalPanel(condition = "input.repart == 'Negativ Binomiala'",
#                  sidebarLayout(
#                    sidebarPanel(
#                      
#                      numericInput("nbinom_r", "Numarul de esecuri : ", value = 10, min = 1, max = 1000),
#                      numericInput("nbinom_n", "Numarul de succese : ", value = 10, min = 1, max = 1000),
#                      sliderInput("nbinom_p", "Probabilitatea de realizare :", min = 0, max = 1, value = 0.5, step = 0.01),
#                      radioButtons("nbinom_func", "Selectati Histograma", choices = c("Functia de repartitie", 
#                                                                                     "Functia de masa")
#                      ),
#                      numericInput("a", "a = ", value = 10),
#                      numericInput("b", "b = ", value = 10)
#                      
#                      
#                    ),
#                    
#                    
#                    mainPanel(
#                      conditionalPanel(condition = "input.nbinom_func == 'Functia de repartitie'",
#                                       plotOutput("nbinom_cdf")
#                      ),
#                      conditionalPanel(condition = "input.nbinom_func == 'Functia de masa'",
#                                       plotOutput("nbinom_pmf")
#                                       
#                      )
#                    )
#                  )
# ),


)


plot_poligon <- function(x, a, b, fun){
  polygon(c(0, x[x<=a],a), 
          c(0, fun[x<=a],0), density = 15, angle = 135 , lty = 5, lwd = 2,
          col="black")
  polygon(c(a, x[x>=a & x<=b],b), 
          c(0, fun[x>=a & x<=b],0), density = 15, lty = 2, lwd = 2,
          col="black")
  polygon(c(b, x[x>=b], x[length(x)]), 
          c(0, fun[x>=b],0), density = 15, angle = 135, lwd = 2,
          col="black")
}

plot_continua <- function(x, fun, xlab, ylab, main){
  plot(x, fun, type = "l", lty=2, lwd=2,
       xlab=xlab, ylab=ylab, main=main)
}

plot_bars <- function(x, fun, a, b, l){
  barplot(names.arg = x, height = fun, xlab = "x", ylab = "Fx(x)", 
          main = "P(x<=a) P(a<=x<=b) P(x>=b)", 
          col = c(rep("blue", a+1), 
                  rep("yellow", b - a),
                  rep("red", l) ) )
}

server <- function(input, output) {
  
  #Binomial distribution
  
  #Functia de repartitie
  output$binom_cdf <- renderPlot({
    x <- seq(0, input$binom_n)
    cdf <- pbinom(x, input$binom_n, input$binom_p)
    plot(x, cdf, 
         type = "s", 
         xlab = "x", 
         ylab = "Fx(x)", 
         main = "Functia de repartitie"
    )
  })
  
  #Functia de masa 
  
  output$binom_pmf <- renderPlot({
    x <- seq(0, input$binom_n)
    pmf <- dbinom(x, input$binom_n, input$binom_p)
    plot(x, pmf, type = "h", 
         xlab = "k", 
         ylab = "P(X=k)",
         main = "Functia de masa"
    )
  })
  
  output$binom_prob <- renderPlot({

    x <- seq(0, input$binom_n)
    cdf <- pbinom(x, input$binom_n, input$binom_p)
    plot_bars(x, cdf, input$binom_a, input$binom_b, input$binom_n)
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
  
  # Hipergeomtric distribution 
  
  output$hg_cdf <- renderPlot({
    x <- seq(0, input$hg_k)
    cdf <- dhyper(x, input$hg_alb, input$hg_n, input$hg_k)
    plot(x, cdf , type = "h", 
         xlab = "k", 
         ylab = "P(X=k)", 
         main = "Functia de masa")
  })
  
  output$hg_pmf <- renderPlot({
    x <- seq(0, input$hg_k)
    pmf <- phyper(x, input$hg_alb, input$hg_n, input$hg_k)
    plot(x, pmf, type = "s", 
         xlab = "x", 
         ylab = "Fx(x)", 
         main = "Functia de repartitie")
  })
  
  output$hg_prob <- renderPlot({
    x <- seq(0, input$hg_k)
    pmf <- phyper(x, input$hg_alb, input$hg_n, input$hg_k)
    plot_bars(x, pmf,input$hg_a ,input$hg_b,input$hg_k)
  })

  
  # Negative Binomial distribution
  
  # output$nbinom_cdf <- renderPlot({
  #   r <- input$nbinom_r
  #   p <- input$nbinom_p
  #   x <- 0:20
  #   y <- pbeta(1 - p, x + 1, r)
  #   curve(y, from = 0, to = max(x), xlab = "x", ylab = "F(X <= x)", main = "Negative Binomial CDF")
  # })
  # output$nbinom_pmf <- renderPlot({
  #   r <- input$nbinom_r
  #   p <- input$nbinom_p
  #   n <- input$nbinom_n
  #   y <- pnegbin(x, r, p)
  #   plot(x, y, type = "h", xlab = "Nr succese", ylab = "Probabilitatea", main = "Negative Binomial CDF")  })
  # 
}

# Run the app ----
shinyApp(ui = ui, server = server)