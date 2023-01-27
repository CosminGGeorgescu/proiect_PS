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
                       
                       numericInput("n", "Numarul de experimente : ", value = 10, min = 1),
                       sliderInput("p", "Probabilitatea de realizare :", min = 0, max = 1, value = 0.5, step = 0.01),
                       radioButtons("func", "Selectati Histograma", choices = c("Functia de repartitie", 
                                                                                "Functia de masa")
                       ),
                       numericInput("a", "a = ", value = 10),
                       numericInput("b", "b = ", value = 10)
                       
                       
                     ),
                     
                   
                   mainPanel(
                     plotOutput("cdf")
                   )
                 )
  )
)



server <- function(input, output) {
  output$cdf <- renderPlot({
    x <- 0:input$n
    y <- dbinom(x, size = input$n, prob = input$p)
    barplot(y, names.arg = x, 
            xlab = "Numarul de succese", 
            ylab = "Probabilitatea", 
            main = "Functia de masa a distributiei binomiale")
     })
}

# Run the app ----
shinyApp(ui = ui, server = server)