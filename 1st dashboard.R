## install pkgs
install.packages("DT")


### import Libraries
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(caret)
library(randomForest)

##Load The data
#### By default, iris is in R
data = iris
head(data)

## About the data
## Description: This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.
## Format: iris is a data frame with 150 cases (rows) and 5 variables (columns) named Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species.
## References: Fisher, R. A. (1936) The use of multiple measurements in taxonomic problems. Annals of Eugenics, 7, Part II, 179–188.
# The data were collected by Anderson, Edgar (1935). The irises of the Gaspe Peninsula, Bulletin of the American Iris Society, 59, 2–5.

min(data$Sepal.Length)
### UI Function
ui <- fluidPage(theme = shinytheme("united"),
                titlePanel("Iris Analysis"),
                sidebarLayout(sidebarPanel(sliderInput("SLrange", "Sepal Length Range ", min = min(data$Sepal.Length), max = max(data$Sepal.Length), value = c(4.4,6.55) ),
                                           sliderInput("SWrange", "Sepal Width Range ", min = min(data$Sepal.Width), max = max(data$Sepal.Width), value = c(3,4) ),
                                           sliderInput("PLrange", "Petal Length Range ", min = min(data$Petal.Length), max = max(data$Petal.Length), value = c(4,5) ),
                                           sliderInput("PWrange", "Petal Width Range ", min = min(data$Petal.Width), max = max(data$Petal.Width), value = c(1,2) )
                ),
                              mainPanel(
                                h2(tags$b("About The Data")),
                                h3("Description"),
                                p("This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length 
                                  and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor,
                                  and virginica."),
                                h3("Format"),
                                p("iris is a data frame with 150 cases (rows) and 5 variables (columns) named Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species."),
                                ## Data Table
                                DT::dataTableOutput("table"),
                                h3("References"),
                                tags$ul("Fisher, R. A. (1936) The use of multiple measurements in taxonomic problems. Annals of Eugenics, 7, Part II, 179–188.", 
                                        "The data were collected by Anderson, Edgar (1935). The irises of the Gaspe Peninsula, Bulletin of the American Iris Society, 59, 2–5.")
                                )
                              )
                )


### Server function
server <- function(input, output) {
  # Reactive filtering of the iris dataset based on slider values
  filtered_data <- reactive({
    subset(data,
           Sepal.Length >= input$SLrange[1] & Sepal.Length <= input$SLrange[2] &
             Sepal.Width >= input$SWrange[1] & Sepal.Width <= input$SWrange[2] &
             Petal.Length >= input$PLrange[1] & Petal.Length <= input$PLrange[2] &
             Petal.Width >= input$PWrange[1] & Petal.Width <= input$PWrange[2]
    )
  })
  
  # Render the filtered data table
  output$table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 20))
  })
}


## Shiny app function
shinyApp(ui, server)

names(tags)

### Modelling
set.seed(42) # for reproducibility
## Train - Test Split
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

## Logistics Reg
logistic_model <- train(Species ~ ., data = trainData, method = "glm")#, trControl = trainControl(method = "cv", number = 5))
logistic_pred <- predict(logistic_model, newdata = testData)
logistic_accuracy <- confusionMatrix(logistic_pred, testData$Species)$overall['Accuracy']

## Random Forest
rf_model <- train(Species ~ ., data = trainData, method = "rf", trControl = trainControl(method = "cv", number = 5))
rf_pred <- predict(rf_model, newdata = testData)
rf_accuracy <- confusionMatrix(rf_pred, testData$Species)$overall['Accuracy']



# Print the accuracies of both models
cat("Logistic Regression Accuracy:", logistic_accuracy, "\n")
cat("Random Forest Accuracy:", rf_accuracy, "\n")



### Deployment
#digital ocean
#google drive
# shinyapps.io


