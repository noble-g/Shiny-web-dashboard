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

#######################
# Load Models
#####################
lr_model = readRDS("C:/Users/USER/Desktop/GitHub/Shiny-web-dashboard/rf_model.rds")
rf_model = readRDS("C:/Users/USER/Desktop/GitHub/Shiny-web-dashboard/rf_model.rds")

## Live prediction of one row

data[1, -5] ## live independent variables
data[1,] ## live data (dependent var inclusive)
# Live data frame creation
live_data <- data.frame(
  Sepal.Length = 5.1,
  Sepal.Width = 3.5,
  Petal.Length = 1.4,
  Petal.Width = 0.2
)

list_data = list(5.1,3.5,1.4,0.2)

lr_pred <- predict(lr_model, newdata = live_data, type = "raw")
rf_pred <- predict(rf_model, newdata = live_data, type = "raw")
# Print the predictions
print(as.character(lr_pred[1]))
print(rf_pred)




### UI Function
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage("Iris Analysis", 
                           tabPanel("Iris Description", 
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
                                    ),
                           tabPanel("Dashboard", 
                                    sidebarLayout(sidebarPanel(sliderInput("SLrange", "Sepal Length Range ", min = min(data$Sepal.Length), max = max(data$Sepal.Length), value = c(4.4,6.55)),
                                                               sliderInput("SWrange", "Sepal Width Range ", min = min(data$Sepal.Width), max = max(data$Sepal.Width), value = c(3,4)),
                                                               sliderInput("PLrange", "Petal Length Range ", min = min(data$Petal.Length), max = max(data$Petal.Length), value = c(4,5)),
                                                               sliderInput("PWrange", "Petal Width Range ", min = min(data$Petal.Width), max = max(data$Petal.Width), value = c(1,2)),
                                                               actionButton("Predict", "Predict", class = "btn btn-primary")
                                    ),
                                    mainPanel(
                                      p("the Dashboard Goes Here")
                                    )
                                    )),
                           tabPanel("Prediction", 
                                    sidebarLayout(sidebarPanel(sliderInput("SLrange", "Sepal Length Range ", min = min(data$Sepal.Length), max = max(data$Sepal.Length), value = 6.55),
                                                               sliderInput("SWrange", "Sepal Width Range ", min = min(data$Sepal.Width), max = max(data$Sepal.Width), value = 4),
                                                               sliderInput("PLrange", "Petal Length Range ", min = min(data$Petal.Length), max = max(data$Petal.Length), value = 5),
                                                               sliderInput("PWrange", "Petal Width Range ", min = min(data$Petal.Width), max = max(data$Petal.Width), value = 2),
                                                               actionButton("Predict", "Predict", class = "btn btn-primary")
                                    ),
                                    mainPanel(
                                      h2(tags$b("Predict The Data")),
                                      p("Here you choose the data for the four independent variables - ..., and predict the "),
                                      h3("Logistics Regression"),
                                      h3("Random Forest"),
                                      ## Model
                                      tableOutput('tabledata') #Prediction of the result table
                                    )
                                    )
                                    ),
                           tabPanel("Model Diagonistics", "intentionally left blank")),
                
                )


                
                
### Server function
server <- function(input, output, session) {
    ## INPUT DATA
    # Reactive filtering of the iris dataset based on slider values
  filtered_data <- reactive({
    subset(data,
           Sepal.Length >= input$SLrange[1] & Sepal.Length <= input$SLrange[2] &
             Sepal.Width >= input$SWrange[1] & Sepal.Width <= input$SWrange[2] &
             Petal.Length >= input$PLrange[1] & Petal.Length <= input$PLrange[2] &
             Petal.Width >= input$PWrange[1] & Petal.Width <= input$PWrange[2]
    )
  })
  
  ## OUTPUT DATA
  ## the Data Description Page
  # Render the filtered data table
  output$table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 20))
  })
  
  #### Live data on the web app model prediction page
  live_data <- reactive({
    
    df<- data.frame(
    Sepal.Length = input$SLrange[2],
    Sepal.Width = input$SWrange[2],
    Petal.Length = input$PLrange[2],
    Petal.Width = input$PWrange[2]
  )
  })
  ###
  Species <- 0
  df <- rbind(df, Species)
  df_input<- transpose(df)
  write.table(df_input, "input.csv", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)
  
  test<- read.csv(paste("input", ".csv", sep=""), header = TRUE)
  
  Output <- data.frame(Prediction=predict(rf_model,test), round(predict(rf_model,test,type="prob"), 3))
  print(Output)

  
  
  ## The Model Page
  output$contents <- renderPrint({
    if (input$Predict>0) { 
      isolate("Your Prediction is Now Ready 🤩😎") 
    } else {
      return("Server 🛰 is ready to predict")
    }
  })
  
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$Predict>0) { 
      isolate(live_data()) 
    } 
  })
  
  
}


## Shiny app function
shinyApp(ui = ui, server = server)

names(tags)



## confusion matrix and its derivatives - accuracy, loss fn, ...
## Model Diagonistivcs
## AUC and ROC curve
## comparing the two models
## Ensembling the two models
## 
##


### Deployment
#digital ocean
#google drive
# shinyapps.io


