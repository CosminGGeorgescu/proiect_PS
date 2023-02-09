library(shiny)
#Define UI ----

ui <- fluidPage(
  
  titlePanel("Repartiile in practica"),
  
  selectInput("repart", "Selectati repartitia", choices= c("Binomiala", 
                                                           "Bernoulli",
                                                           "Negativ Binomiala",
                                                           "Poisson",
                                                           "Geometrica",
                                                           "Hipergeometrica",
                                                           "Student",
                                                           "Chi-Squared",
                                                           "Uniforma continua",
                                                           "Gamma",
                                                           "Beta")),
  conditionalPanel(condition = "input.repart == 'Student'",
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("student_b", "Bound :", min = 10, max = 50, value = 20),
                     sliderInput("student_dof", "Degrees of freedom :", min = 0, max = 10, value = 1, step = 0.1),
                     sliderInput("student_c", "Center :", min = 0, max = 25, value = 0),
                     numericInput("student_a", "a = ", value = 10),
                     numericInput("student_b", "b = ", value = 10)
                   ),
                   mainPanel(
                     plotOutput("student_dens"),
                     plotOutput("student_rep"),
                   )
                 )
),
  conditionalPanel(condition = "input.repart == 'Chi-Squared'",
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("chisq_b", "Bound :", min = 10, max = 50, value = 20),
                     sliderInput("chisq_dof", "Degrees of freedom :", min = 0, max = 10, value = 1, step = 0.1),
                     sliderInput("chisq_c", "Center :", min = 0, max = 25, value = 0),
                     numericInput("chisq_a", "a = ", value = 10),
                     numericInput("chisq_b", "b = ", value = 10)
                   ),
                   mainPanel(
                     plotOutput("chisq_dens"),
                     plotOutput("chisq_rep"),
                   )
                 )
),
  conditionalPanel(condition = "input.repart == 'Uniforma continua'",
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput(inputId ="cunif_min", label = "Minimul :", min = 0, max = 20, value = 5),
                     sliderInput(inputId ="cunif_max", label = "Maximul :", min = 0, max = 20, value = 15),
                     numericInput(inputId = "cunif_a", label = "a = ", value = 10, min = 0),
                     numericInput(inputId = "cunif_b", label = "b = ", value = 15, min = 0),
                   ),
                   mainPanel(
                     plotOutput("cunif_dens"),
                     plotOutput("cunif_rep"),
                   )
                 )
),
  conditionalPanel(condition = "input.repart == 'Gamma'",
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput(inputId ="gamma_s", label = "Forma :", min = 0, max = 20, value = 5),
                     sliderInput(inputId ="gamma_r", label = "Rata :", min = 0, max = 5, value = 15, step = 0.1),
                     numericInput(inputId = "gamma_a", label = "a = ", value = 10, min = 0),
                     numericInput(inputId = "gamma_b", label = "b = ", value = 15, min = 0),
                   ),
                   mainPanel(
                     plotOutput("gamma_dens"),
                     plotOutput("gamma_rep"),
                   )
                 )
),
  conditionalPanel(condition = "input.repart == 'Beta'",
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput(inputId ="beta_alpha", label = "Alpha :", min = 0, max = 10, value = 5),
                     sliderInput(inputId ="beta_beta", label = "Beta :", min = 0, max = 30, value = 15, step = 0.1),
                     numericInput(inputId = "beta_a", label = "a = ", value = 0.5, min = 0),
                     numericInput(inputId = "beta_b", label = "b = ", value = 0.8, min = 0),
                   ),
                   mainPanel(
                     plotOutput("beta_dens"),
                     plotOutput("beta_rep"),
                   )
                 )
),
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
                  rep("gray", b - a),
                  rep("purple", l) ) )
}

server <- function(input, output) {
  
  #Student
  output$student_dens <- renderPlot({
    plot(function(x) dt(x, df=input$student_dof, ncp=input$student_c), input$student_b / -2, input$student_b,
         xlab = "X", ylab = "fx(x)", main = "Densitatea repartitiei student")
  })
  output$student_rep <- renderPlot({
    plot(function(x) pt(x, df=input$student_dof, ncp=input$student_c), input$student_b / -2, input$student_b,
         xlab = "X", ylab = "Fx(x)", main = "Functia de repartitie student && P(x<=a) P(a<=x<=b) P(x>=b)")
    x <- seq(input$student_b / -2, input$student_b, by=0.01)
    plot_poligon(x = x, a = input$student_a, b = input$student_b, fun = pt(x, df=input$student_dof, ncp=input$student_c))
  })
  
  # Chi-Squared
  output$chisq_dens <- renderPlot({
    x <- seq(0, input$chisq_b, by=0.01)
    plot(x, dchisq(x, df=input$chisq_dof, ncp=input$chisq_c), type="l", xlab = "X", ylab = "fx(x)", main = "Densitate Chi square")
  })
  output$chisq_rep <- renderPlot({
    x <- seq(0, input$chisq_b, by=0.01)
    plot(x, pchisq(x, df=input$chisq_dof, ncp=input$chisq_c), 
         type="l", xlab = "X", ylab = "Fx(x)", main = "Functia de repartitie Chi-Squared && P(x<=a) P(a<=x<=b) P(x>=b)")
    plot_poligon(x = x, a = input$chisq_a, b = input$chisq_b, fun = pchisq(x, df=input$chisq_dof, ncp=input$chisq_c))
  })
  
  # Uniforma continua
  output$cunif_dens <- renderPlot({
    x <- seq(0,input$cunif_max + input$cunif_min, by = 0.1)
    plot(x, dunif(x, input$cunif_min, input$cunif_max), type = "l", xlab ="x", ylab = "fx(x)", main = "Densitatea repartitiei uniforme continue")
  })
  output$cunif_rep <- renderPlot({
    x <- seq(0,input$cunif_max + input$cunif_min, by = 0.1 )
    plot(x, punif(x, input$cunif_min, input$cunif_max), type = "l", xlab = "x", ylab = "Fx(x)", main = "Functia de repartitie uniforma continua && P(x<=a) P(a<=x<=b) P(x>=b)")
    plot_poligon(x, input$cunif_a, input$cunif_b, punif(x, input$cunif_min, input$cunif_max))
  })
  
  # Gamma
  output$gamma_dens <- renderPlot({
    x <- seq(0, 20)
    plot(x,dgamma(x, input$gamma_s, input$gamma_r), type = "b", xlab = "k", ylab = "fx(x)", main = "Densitatea repartitiei gamma")
  })
  output$gamma_rep <- renderPlot({
    x <- seq(0, 20)
    fun <- pgamma(x, input$gamma_s, input$gamma_r)
    plot(x,fun, xlab = "x", ylab = "Fx(x)", main = "P(x<=a) P(a<=x<=b) P(x>=b)")
    plot_poligon(x = x,
                 a = input$gamma_a,
                 b = input$gamma_b,
                 fun = fun)
  })
  
  # Beta
  output$beta_dens <- renderPlot({
    x <- seq(0, 1,by = 0.02)
    plot(x,dbeta(x, input$beta_alpha, input$beta_beta), type = "b", xlab = "k", ylab = "fx(x)",  main = "Densitatea repartitiei beta")
  })
  output$beta_rep <- renderPlot({
    x <- seq(0, 1,by = 0.02)
    plot(x,pbeta(x, input$beta_alpha, input$beta_beta), xlab = "x", ylab = "Fx(x)", main = "P(x<=a) P(a<=x<=b) P(x>=b)")
    plot_poligon(x = x,
                 a = input$beta_a,
                 b = input$beta_b,
                 fun = pbeta(x, input$beta_alpha, input$beta_beta))  
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)