#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(rpart)
library(rpart.plot)
redwine <-
  #read.csv("/Users/Vidya/Downloads/winequality-red.csv", sep = ";")
  read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep = ";")

str(redwine)
redwine$quality <- as.numeric(redwine$quality)
# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Red Wine Characteristics"),
  
  # Sidebar with 2 dropdown selections
  sidebarLayout(
    sidebarPanel(
      selectInput("xcol", "X" , choice = names(redwine)),
      selectInput("ycol", "Y", choice = names(redwine))
    ),
    
    # Show a plot of the generated plot
    mainPanel(tabsetPanel(
      tabPanel("Plot", h2("Scatter Plot"), plotOutput("plot")),
      tabPanel(
        "Decision Tree Model",
        h3("Decision Tree Classification"),
        plotOutput("treeplot"),
        verbatimTextOutput("model1")
      )
    ))
  ))
# Define server logic required to draw a scatter plot
server <- function(input, output) {
  output$plot <- renderPlot({
    plot(redwine[, c(input$xcol, input$ycol)], main = "Scatter plot of wine characteristics")
    
  })
  
  output$model1 <- renderPrint({
    n <- nrow(redwine)
    n_train <- round(0.75 * n)
    
    #Split data into test and training sets

    train_indices <- sample(1:n, n_train)
    wine_train <- redwine[train_indices , ]
    wine_test <- redwine[-train_indices , ]
    dim(wine_train)
    dim(wine_test)
    
    #Train a gini-based model
    wine_tree <- rpart(formula = quality~.,
                       data = wine_train,
                       method = "class",
                       parms = list(split = "gini"))
    
    #printcp(wine_tree)
    
    #Plot the decison tree
    #rpart.plot(wine_tree)
    
    output$treeplot <- renderPlot({
     rpart.plot(wine_tree)
      
    })
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
