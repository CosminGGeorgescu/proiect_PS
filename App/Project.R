library(shiny)
library(ggplot2)
library(extraDistr)

ui <- fluidPage(
  
  titlePanel("Repartiile in practica"),
  
  selectInput("repart", "Selectati repartitia", choices= c("Log Normala",
                                                           "Normala",
                                                           "Cauchy",
                                                           "Fisher",
                                                           "Weibull")
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
  
)

#================================================================================================================================================================


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
                  rep("gray", b - a),
                  rep("purple", l) ) )
}

server <- function(input, output, session) {
  
  
  
  
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
}

shinyApp(ui, server)
