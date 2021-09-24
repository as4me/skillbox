#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

tit <- read.csv("Titanic.csv")

#View(tit)
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Titanic Passengers"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("nkol",
                       "Barplot:",
                       choices = c("Survived", "PClass","Sex"))
    ),
    column(4,
           selectInput("kol",
                       "Histogram:",
                       choices = c("Age", "Fare"))
    )
    
  ),
  fluidRow(
    column(4,
           selectInput("ucoolor1",
                       "Color 1:",
                       choices = c(`Aquamarine` = "aquamarine",
                       `Royal blue` = "royalblue",
                       `Dark blue` = "darkblue")
                       )
    ),
    column(4,
           selectInput("ucoolor2",
                       "Color 2:",
                       choices = c(`Aquamarine` = "aquamarine",
                                   `Royal blue` = "royalblue",
                                   `Dark blue` = "darkblue")
           )
    ),
  ),
  fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("distPlot1"), plotOutput("distPlot2"))),
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot1 <- renderPlot({
    if (input$nkol == "Survived"){
      # Barplot
      Bar <- ggplot(data = tit,aes(x = tit$Survived)) + geom_bar(fill = input$ucoolor1,binwidth=1.2,bins=20,color ="black")
      print(Bar + labs(y="Count", x = "Survived"))
      
    } else if (input$nkol == "PClass"){
      Bar <- ggplot(data = tit,aes(x = tit$Pclass)) + geom_bar(fill = input$ucoolor1,binwidth=1.2,bins=20,color ="black")
      print(Bar + labs(y="Count", x = "Pclass"))
    } else if (input$nkol == "Sex") {
      Bar <- ggplot(data = tit,aes(x = tit$Sex)) + geom_bar(fill = input$ucoolor1,binwidth=1.2,bins=20,color ="black")
      print(Bar + labs(y="Count", x = "Sex"))
    }
    
  })
  output$distPlot2 <- renderPlot({
    
    
    if (input$kol == "Age"){
      Bar <- ggplot(data = tit,aes(x = tit$Age)) + geom_histogram(fill = input$ucoolor2,binwidth=1.2,bins=20,color ="black")
      print(Bar + labs(y="Count", x = "Age"))
    } else if ((input$kol == "Fare")){
      Bar <- ggplot(data = tit,aes(x = unlist(tit["Fare"]))) + geom_histogram(fill = input$ucoolor2,binwidth=10,bins=5,color ="black")
      print(Bar + labs(y="Count", x = "Fare"))
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

