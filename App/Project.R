library(shiny)
library(ggplot2)
library(extraDistr)
#Define UI ----

ui <- fluidPage(
  
  titlePanel("Repartiile in practica"),
  
  selectInput("repart", "Selectati repartitia", choices= c("Binomiala", 
                                                           "Uniforma discreta",
                                                           "Poisson",
                                                           "Geometrica",
                                                           "Hipergeometrica",
                                                           "Log Normala",
                                                           "Normala",
                                                           "Cauchy",
                                                           "Fisher",
                                                           "Weibull")
  ),
  
  conditionalPanel(condition = "input.repart == 'Cauchy'",
                   sidebarLayout(
                     sidebarPanel(
                       
                       sliderInput(inputId ="location_cauchy", label = "Alege punctul", min = -10, max = 10, value = 0),
                       sliderInput(inputId ="scale_cauchy", label = "Alege scala", min = 0, max = 10, value =   5),
                       sliderInput(inputId = "number_cauchy", label = "Alege nr de experimente",min = 20, max = 100, value = 20),
                       numericInput(inputId = "a_cauchy", label = "a = ", value = 5, min = 0),
                       numericInput(inputId = "b_cauchy", label = "b = ", value = 5, min = 0)
                       
                       
                       
                     ),
                     
                     
                     mainPanel(
                       
                       plotOutput("density_cauchy"),
                       plotOutput("repartition_cauchy"),
                       
                     )
                   )
  ),
  
  conditionalPanel(condition = "input.repart == 'Log Normala'",
                   sidebarLayout(
                     sidebarPanel(
                       
                       sliderInput(inputId = "number_lognorm", label = "Bound", min = 10, max = 200, value = 100),
                       sliderInput(inputId = "medie_lognorm", label = "Medie", min = 0, max = 200, value = 50),
                       sliderInput(inputId = "var_lognorm", label = "Varianta", min = 1, max = 25, value = 10, step=0.25),
                       numericInput("a_lognorm", "a = ", value = 10),
                       numericInput("b_lognorm", "b = ", value = 10)
                       
                       
                       
                     ),
                     
                     
                     mainPanel(
                       
                       plotOutput("density_lognorm"),
                       plotOutput("repartition_lognorm"),
                       
                     )
                   )
  ),
  
  conditionalPanel(condition = "input.repart == 'Weibull'",
                   sidebarLayout(
                     sidebarPanel(
                       
                       sliderInput(inputId ="shape_weibull", label = "Alege forma", min = 0.1, max = 10, value = 1),
                       sliderInput(inputId ="scale_weibull", label = "Alege scala", min = 0.1, max = 10, value = 1),
                       numericInput(inputId = "a_weibull", label = "a = ", value = 1, min = 0),
                       numericInput(inputId = "b_weibull", label = "b = ", value = 1, min = 0)
                       
                       
                       
                     ),
                     
                     
                     
                     mainPanel(
                       
                       plotOutput("density_weibull"),
                       plotOutput("repartition_weibull"),
                       
                     )
                   )
  ),
  conditionalPanel(condition = "input.repart == 'Fisher'",
                   sidebarLayout(
                     sidebarPanel(
                       
                       sliderInput(inputId ="grad1_fisher", label = "Alege gradul de libertate 1", min = 0, max = 50, value = 25),
                       sliderInput(inputId ="grad2_fisher", label = "Alege gradul de libertate 2", min = 0, max = 50, value = 25),
                       numericInput(inputId = "a_fisher", label = "a = ", value = 25, min = 0),
                       numericInput(inputId = "b_fisher", label = "b = ", value = 25, min = 0)
                       
                       
                       
                     ),
                     
                     
                     mainPanel(
                       
                       plotOutput("density_fisher"),
                       plotOutput("repartition_fisher"),
                       
                     )
                   )
  ),
  
  conditionalPanel(condition = "input.repart == 'Normala'",
                   sidebarLayout(
                     sidebarPanel(
                       
                       sliderInput(inputId = "number_normal", label = "Bound", min = 10, max = 200, value = 100),
                       sliderInput(inputId = "medie_normal", label = "Medie", min = -100, max = 100, value = 50),
                       sliderInput(inputId = "var_normal", label = "Varianta", min = 1, max = 100, value = 10),
                       numericInput(inputId = "a_normal", label = "a = ", value = 55, min = -100),
                       numericInput(inputId = "b_normal", label = "b = ", value = 75, min = 0)
                       
                       
                       
                     ),
                     
                     
                     mainPanel(
                       
                       plotOutput("density_normal"),
                       plotOutput("repartition_normal"),
                       
                     )
                   )
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
),

conditionalPanel(condition = "input.repart == 'Uniforma discreta'",
                 sidebarLayout(
                   sidebarPanel(
                     numericInput("ud_min", "Minimul : ", value = 5, min = 1, max = 20),
                     numericInput("ud_max", "Maximul : ", value = 15, min = 1, max = 20),
                     numericInput("ud_a", "a = ", value = 9, min = 0),
                     numericInput("ud_b", "b = ", value = 17, min = 0)
                     
                   ),
                   
                   
                   mainPanel(
                     
                     plotOutput("ud_cdf"),
                     plotOutput("ud_pmf"),
                     plotOutput("ud_prob")
                     
                   )
                 )
),

conditionalPanel(condition = "input.repart == 'Geometrica'",
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("geom_p", "Probabilitatea de realizare :", min = 0, max = 1, value = 0.5, step = 0.01),
                     numericInput("geom_a", "a = ", value = 3, min = 0),
                     numericInput("geom_b", "b = ", value = 5, min = 0)
                     
                   ),
                   
                   
                   mainPanel(
                     
                     plotOutput("geom_cdf"),
                     plotOutput("geom_pmf"),
                     plotOutput("geom_prob")
                     
                   )
                 )
),
conditionalPanel(condition = "input.repart == 'Poisson'",
                 sidebarLayout(
                   sidebarPanel(
                     numericInput("pois_lambda", "Rata aparitiei : ", value = 50, min = 1, max = 100),
                     numericInput("pois_a", "a = ", value = 80),
                     numericInput("pois_b", "b = ", value = 70)
                     
                     
                   ),
                   
                   
                   mainPanel(
                     
                     plotOutput("pois_cdf"),
                     plotOutput("pois_pmf"),
                     plotOutput("pois_prob")
                     
                   )
                 )
)



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
  
  #log normala continua------------------------------------------------------------------------------------------------------------------------------
  
  output$density_lognorm <- renderPlot(
    {
      x <- seq(0.001, input$number_lognorm, by=0.01)
      fun <- dlnorm(x, input$medie_lognorm, input$var_lognorm)
      plot_continua(x, fun, xlab = "X", ylab = "fx(x)", main = "Densitate repartitiei log normala")
    })
  
  output$repartition_lognorm <- renderPlot(
    {
      x <- seq(0.001, input$number_lognorm, by=0.01)
      fun <- plnorm(x, mean = input$medie_lognorm, sd = input$var_lognorm)
      plot_continua(x, fun, xlab = "X", ylab = "Fx(x)", main = "Functia de repartitie log normala && P(x<=a) P(a<=x<=b) P(x>=b)")
      plot_poligon(x = x, a = input$a_lognorm, b = input$b_lognorm, fun = fun)
    })
  
  
  
  
  #normala continua------------------------------------------------------------------------------------------------------------------------------
  
  output$density_normal <- renderPlot(
    {
      x <- seq(-input$number_normal, input$number_normal)
      plot(x, dnorm(x, mean = input$medie_normal, sd = input$var_normal), type = "l", lty=2, lwd=2,
           xlab = "X", ylab = "fx(x)", main = "Densitate repartitiei normale")
    })
  
  output$repartition_normal <- renderPlot(
    {
      x <- seq(-input$number_normal, input$number_normal)
      fun <- pnorm(x, mean = input$medie_normal, sd = input$var_normal)
      plot(x, fun, type = "l", lty=2, lwd=2, xlab = "X", ylab = "Fx(x)", main = "Functia de repartitie normala && P(x<=a) P(a<=x<=b) P(x>=b)")
      plot_poligon(x = x,
                   a = input$a_normal,
                   b = input$b_normal,
                   fun = fun)
    })
  
  
  
  
  #cauchy continua------------------------------------------------------------------------------------------------------------------------------
  
  output$density_cauchy <- renderPlot(
    {
      x <- seq(-1*input$number_cauchy,input$number_cauchy, by = 0.1)
      plot(x, dcauchy(x, input$location_cauchy, input$scale_cauchy), type = "l", xlab ="x", ylab = "f(x)", main = "Densitatea repartitiei Cauchy")
    })
  
  output$repartition_cauchy <- renderPlot(
    {
      x <- seq(-1*input$number_cauchy, input$number_cauchy, by = 0.1)
      plot(x, pcauchy(x, input$location_cauchy, input$scale_cauchy), type = "l", xlab = "x", ylab = "F(x)", main = "Functia de repartitie Cauchy && P(x<=a) P(a<=x<=b) P(x>=b)")
      plot_poligon(x, input$a_cauchy, input$b_cauchy, pcauchy(x, input$location_cauchy, input$scale_cauchy))
    })
  
  
  
  
  #fisher continua------------------------------------------------------------------------------------------------------------------------------
  
  output$density_fisher <- renderPlot(
    {
      x <- seq(-1*(input$grad1_fisher + input$grad2_fisher),input$grad1_fisher + input$grad2_fisher, by = 0.1)
      plot(x, df(x, input$grad1_fisher, input$grad2_fisher), type = "l", xlab ="x", ylab = "f(x)", main = "Densitatea repartitiei Fisher")
    })
  
  output$repartition_fisher <- renderPlot(
    {
      x <- seq(-1*(input$grad1_fisher + input$grad2_fisher), input$grad1_fisher + input$grad2_fisher, by = 0.1)
      plot(x, pf(x, input$grad1_fisher, input$grad2_fisher), type = "l", xlab = "x", ylab = "F(x)", main = "Functia de repartitie Fisher && P(x<=a) P(a<=x<=b) P(x>=b)")
      plot_poligon(x, input$a_fisher, input$b_fisher, pf(x, input$grad1_fisher, input$grad2_fisher))
    })
  
  
  
  #weibull continua------------------------------------------------------------------------------------------------------------------------------
  
  output$density_weibull <- renderPlot(
    {
      x <- seq(0,input$shape_weibull + input$scale_weibull, by = 0.01)
      plot(x, dweibull(x, input$shape_weibull, input$scale_weibull), type = "l", xlab ="x", ylab = "f(x)", main = "Densitatea repartitiei Weibull")
    })
  
  output$repartition_weibull <- renderPlot(
    {
      x <- seq(0, input$shape_weibull + input$scale_weibull, by = 0.1)
      plot(x, pweibull(x, input$shape_weibull, input$scale_weibull), type = "l", xlab = "x", ylab = "F(x)", main = "Functia de repartitie Weibull && P(x<=a) P(a<=x<=b) P(x>=b)")
      plot_poligon(x, input$a_weibull, input$b_weibull, pweibull(x, input$shape_weibull, input$scale_weibull))
    })
  
  
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
  
  
  # Hipergeomtric distribution 
  
  
  #Functia de masa
  output$hg_pmf <- renderPlot({
    x <- seq(0, input$hg_k)
    pmf <- dhyper(x, input$hg_alb, input$hg_n, input$hg_k)
    plot(x, pmf , type = "h", 
         xlab = "k", 
         ylab = "P(X=k)", 
         main = "Functia de masa")
  })
  
  #Functia de repartitie
  output$hg_cdf <- renderPlot({
    x <- seq(0, input$hg_k)
    cdf <- phyper(x, input$hg_alb, input$hg_n, input$hg_k)
    plot(x, cdf, type = "s", 
         xlab = "x", 
         ylab = "Fx(x)", 
         main = "Functia de repartitie")
  })
  
  #Probabilitati
  output$hg_prob <- renderPlot({
    x <- seq(0, input$hg_k)
    pmf <- phyper(x, input$hg_alb, input$hg_n, input$hg_k)
    plot_bars(x, pmf,input$hg_a ,input$hg_b,input$hg_k)
  })

  #Poisson 
  
  #Functia de masa
  
  
  output$pois_pmf <- renderPlot({
    x <- seq(0, input$pois_lambda * 5)
    cdf <- dpois(x, input$pois_lambda)
    plot(x, cdf, type = "h", 
         xlab = "k", 
         ylab = "P(X=k)", 
         main = "Functia de masa")
  })
  
  #Functia de repartitie
  
  output$pois_cdf <- renderPlot({
    x <- seq(0, input$pois_lambda * 5)
    pmf <- ppois(x, input$pois_lambda)
    plot(x, pmf, type = "s", 
         xlab = "x", 
         ylab = "Fx(x)", 
         main = "Functia de repartitie")
  })
  
  # Probabilitati
  output$pois_prob <- renderPlot({
    x <- seq(0, input$pois_lambda * 5)
    pmf <- ppois(x, input$pois_lambda)
    plot_bars(x,pmf,input$pois_b, input$pois_a, input$pois_lambda * 5)
  })
  
  #Uniforma discreta
  
  output$ud_pmf <- renderPlot({
    x <- seq(0, input$ud_max + input$ud_min)
    pmf <- ddunif(x, input$ud_min, input$ud_max)
    plot(x, pmf, 
         type = "h", 
         xlab = "k", 
         ylab = "P(X=k)", 
         main = "Functia de masa")
  })
  
  output$ud_cdf <- renderPlot({
    x <- seq(0, input$ud_max + input$ud_min)
    cdf <- pdunif(x, input$ud_min, input$ud_max)
    plot(x, cdf, 
         type = "s", 
         xlab = "x", 
         ylab = "Fx(x)", 
         main = "Functia de repartitie")
  })
  
  output$ud_prob <- renderPlot({
    x <- seq(0, input$ud_max + input$ud_min)
    cdf <- pdunif(x, input$ud_min, input$ud_max)
    plot_bars(x, cdf, input$ud_a, input$ud_b, input$ud_max + input$ud_min)
  })
    
  output$geom_pmf <- renderPlot({
    x <- seq(0, 40)
    pmf <- dgeom(x, input$geom_p)
    plot(x, pmf, 
         type = "h", 
         xlab = "k", 
         ylab = "P(X=k)", 
         main = "Functia de masa")
  })
  output$geom_cdf <- renderPlot({
    x <- seq(0, 40)
    cdf <- pgeom(x, input$geom_p)
    plot(x, cdf, 
         type = "s", 
         xlab = "x", 
         ylab = "Fx(x)", 
         main = "Functia de repartitie")
  })
  output$geom_prob <- renderPlot({
    x <- seq(0, 40)
    cdf <- pgeom(x, input$geom_p)
    plot_bars(x, cdf, input$geom_a,input$geom_b,40)
  })
  

}

# Run the app ----
shinyApp(ui = ui, server = server)