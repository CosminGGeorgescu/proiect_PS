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
                                                           "Student")),
  conditionalPanel(condition = "input.repart == 'Student'",
                 sidebarLayout(
                   sidebarPanel(
                     
                     sliderInput("student_b", "Bound", min = 10, max = 50, value = 20),
                     sliderInput("student_dof", "Degrees of freedom :", min = 0, max = 10, value = 1, step = 0.1),
                     sliderInput("student_c", "Center", min = 0, max = 25, value = 0),
                     
                     numericInput("student_a", "a = ", value = 10),
                     numericInput("student_b", "b = ", value = 10)
                     
                     
                     
                   ),
                   
                   
                   mainPanel(
          
                     plotOutput("student_dens"),
                     plotOutput("student_rep"),
                     
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
  output$student_dens <- renderPlot({
    #x <- seq(input$student_b / -2, input$student_b, by=0.1)
    #plot(x, dt(x, df=input$student_dof, ncp=input$student_c), type="l")
    plot(function(x) dt(x, df=input$student_dof, ncp=input$student_c), input$student_b / -2, input$student_b,
         xlab = "X", ylab = "fx(x)", main = "Densitatea repartitiei student")
  })
  output$student_rep <- renderPlot({
    plot(function(x) pt(x, df=input$student_dof, ncp=input$student_c), input$student_b / -2, input$student_b,
         xlab = "X", ylab = "Fx(x)", main = "Functia de repartitie student && P(x<=a) P(a<=x<=b) P(x>=b)")
    x <- seq(input$student_b / -2, input$student_b, by=0.1)
    plot_poligon(x = x, a = input$student_a, b = input$student_b, fun = pt(x, df=input$student_dof, ncp=input$student_c))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)