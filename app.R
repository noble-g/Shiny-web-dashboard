## install pkgs
#install.packages("DT")


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
#head(data)

## About the data
## Description: This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.
## Format: iris is a data frame with 150 cases (rows) and 5 variables (columns) named Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species.
## References: Fisher, R. A. (1936) The use of multiple measurements in taxonomic problems. Annals of Eugenics, 7, Part II, 179–188.
# The data were collected by Anderson, Edgar (1935). The irises of the Gaspe Peninsula, Bulletin of the American Iris Society, 59, 2–5.

#min(data$Sepal.Length)

#######################
# Load Models
#####################
lr_model = readRDS("C:/Users/USER/Desktop/GitHub/Shiny-web-dashboard/rf_model.rds")
rf_model = readRDS("C:/Users/USER/Desktop/GitHub/Shiny-web-dashboard/rf_model.rds")

## Live prediction of one row

#data[1, -5] ## live independent variables
#data[1,] ## live data (dependent var inclusive)
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
#print(as.character(lr_pred[1]))
#print(rf_pred)




### UI Function
ui <- fluidPage(theme = shinytheme("darkly"),
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
                                      tags$pre(verbatimTextOutput("data_structure")), ## to print the structure of the data
                                      p("iris is a data frame with 150 cases (rows) and 5 variables (columns) named Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species."),
                                      ## Data Table
                                      DT::dataTableOutput("table"),
                                      h3("References"),
                                      tags$ul("Fisher, R. A. (1936) The use of multiple measurements in taxonomic problems. Annals of Eugenics, 7, Part II, 179–188.", 
                                              "The data were collected by Anderson, Edgar (1935). The irises of the Gaspe Peninsula, Bulletin of the American Iris Society, 59, 2–5.")
                                    )
                                    )## close sidebar layout
                                    ), ## close tabpanel
                           tabPanel("Dashboard", 
                                    sidebarLayout(sidebarPanel(sliderInput("SLdash", "Sepal Length Range ", min = min(data$Sepal.Length), max = max(data$Sepal.Length), value = c(4.4,6.55)),
                                                               sliderInput("SWdash", "Sepal Width Range ", min = min(data$Sepal.Width), max = max(data$Sepal.Width), value = c(3,4)),
                                                               sliderInput("PLdash", "Petal Length Range ", min = min(data$Petal.Length), max = max(data$Petal.Length), value = c(4,5)),
                                                               sliderInput("PWdash", "Petal Width Range ", min = min(data$Petal.Width), max = max(data$Petal.Width), value = c(1,2)),
                                                               actionButton("Predict", "Predict", class = "btn btn-primary")  
                                                               ),
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("DashBoard",
                                      dashboardPage(
                                        dashboardHeader(),
                                        dashboardSidebar(),
                                      dashboardBody(
                                        fluidRow( #### The average cards
                                          box(
                                            title = "Sepal Length Mean", verbatimTextOutput("sepal_length_mean")
                                          ),
                                          box(
                                            title = "Sepal Width Mean", verbatimTextOutput("sepal_width_mean")
                                          ),
                                          box(
                                            title = "Petal Length Mean", verbatimTextOutput("petal_length_mean")
                                          ),
                                          box(
                                            title = "Petal Width Mean", verbatimTextOutput("petal_width_mean")
                                          )
                                        ),
                                        fluidRow( #### The Scatter Plots
                                          box(
                                            title = "Sepal Length vs Sepal Width", plotOutput("scatter_plot_sepal")
                                          ),
                                          box(
                                            title = "Petal Length vs Petal Width", plotOutput("scatter_plot_petal")
                                          )
                                        ),
                                        fluidRow( #### The correlation plot and Box plot
                                          box(
                                            title = "Correlation Plot", plotOutput("correlation_plot")
                                          ),
                                          box(
                                            title = "Boxplot", plotOutput("boxplot")
                                          )
                                        ),
                                        fluidRow( #### The Report
                                          box(
                                            title = "Findings", verbatimTextOutput("findings")
                                          )
                                        )
                                    )
                                    )))))),
                           tabPanel("Prediction", 
                                    sidebarLayout(sidebarPanel(sliderInput("SLpoint", "Sepal Length", min = min(data$Sepal.Length), max = max(data$Sepal.Length), value = 6.55),
                                                               sliderInput("SWpoint", "Sepal Width", min = min(data$Sepal.Width), max = max(data$Sepal.Width), value = 4),
                                                               sliderInput("PLpoint", "Petal Length", min = min(data$Petal.Length), max = max(data$Petal.Length), value = 5),
                                                               sliderInput("PWpoint", "Petal Width", min = min(data$Petal.Width), max = max(data$Petal.Width), value = 2),
                                                               actionButton("PredictPredict", "Predict", class = "btn btn-primary")
                                    ),
                                    mainPanel(
                                      h2(tags$b("Predict The Data")),
                                      p("Here you choose the data for the four independent variables - ..., and predict the "),
                                      h3("Logistics Regression"),
                                      h3("Random Forest"),
                                      ## Model
                                      tableOutput('tabledata') #Prediction of the result table
                                    ))),
                           tabPanel("Model Diagonistics", "intentionally left blank")),
                )


                
                
### Server function
server <- function(input, output, session) {
  output$data_structure <- renderPrint({
    str(data)
  })
  
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
    Sepal.Length = input$SLpoint,
    Sepal.Width = input$SWpoint,
    Petal.Length = input$PLpoint,
    Petal.Width = input$PWpoint)
  })
  
  ###
  observeEvent(input$PredictPredict, {
    new_row <- c(Species = 0)
    df <- rbind(live_data(), new_row)
    df_input <- as.data.frame(t(df))
    write.table(df_input, "input.csv", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv("input.csv", header = TRUE)
    
    Output <- data.frame(Prediction = predict(rf_model, test), round(predict(rf_model, test, type = "prob"), 3))
    print(Output)
  })
  
  # The Model Page
  output$contents <- renderPrint({
    if (input$Predict > 0) { 
      "Your Prediction is Now Ready 🤩😎"
    } else {
      "Server 🛰 is ready to predict"
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$Predict > 0) { 
      live_data()
    }
  })
  
  ##### DASHBOARD
  # Calculate means for each variable
  output$sepal_length_mean <- renderText({
    mean(data$Sepal.Length)
  })
  output$sepal_width_mean <- renderText({
    mean(data$Sepal.Width)
  })
  output$petal_length_mean <- renderText({
    mean(data$Petal.Length)
  })
  output$petal_width_mean <- renderText({
    mean(data$Petal.Width)
  })
  
  # Create scatter plot for Sepal Length vs Sepal Width
  output$scatter_plot_sepal <- renderPlot({
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
      geom_point() +
      labs(title = "Sepal Length vs Sepal Width")
  })
  # Create scatter plot for Petal Length vs Petal Width
  output$scatter_plot_petal <- renderPlot({
    ggplot(data, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
      geom_point() +
      labs(title = "Petal Length vs Petal Width")
  })
  
  # Create correlation plot
  output$correlation_plot <- renderPlot({
    corrplot(cor(data[, 1:4]))
  })
  
  # Create boxplot
  output$boxplot <- renderPlot({
    # Create a boxplot of all four numeric variables
    boxplot(data[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")],
            main = "Boxplot of Iris Dataset",
            xlab = "Variables",
            ylab = "Measurement")
  })
  
  # Generate Report
  output$findings <- renderPrint({
    cat("Findings:\n")
    cat("1. Sepal Length has a mean value of", round(mean(data$Sepal.Length), 2), "\n")
    cat("2. Sepal Width has a mean value of", round(mean(data$Sepal.Width), 2), "\n")
    cat("3. Petal Length has a mean value of", round(mean(data$Petal.Length), 2), "\n")
    cat("4. Petal Width has a mean value of", round(mean(data$Petal.Width), 2), "\n")
    cat("5. There is a positive correlation between Sepal Length and Sepal Width.\n")
    cat("6. There is a positive correlation between Petal Length and Petal Width.\n")
    cat("7. Species 'setosa' generally has smaller values for all four variables compared to 'versicolor' and 'virginica'.\n")
    cat("8. Sepal Width has the highest variance among the four variables.\n")
    cat("9. There are some outliers present in Petal Length and Petal Width.\n")
    cat("10. The data appears to be normally distributed for all four variables.\n")
  })
}

## Shiny app function
shinyApp(ui = ui, server = server)





## confusion matrix and its derivatives - accuracy, loss fn, ...
## Model Diagonistics
## AUC and ROC curve
## comparing the two models
## Ensembling the two models
## 
##


### Deployment
#digital ocean
#google drive
# shinyapps.io


