## install pkgs
#install.packages("randomForest")
#install.packages("corrplot")  # Install the corrplot package if you haven't already
#library(remotes)
#install.packages("thematic")
#install.packages("htmltools")
#remotes::install_github("rstudio/htmltools")
#packageVersion("htmltools")

### import Libraries
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(corrplot)
library(caret)
library(randomForest)
library(bslib)
library(bsicons)
library(thematic)


#Declare the theme of the plots to be the same with that of the web app
thematic_on()

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

# Social media icons
sm_icons<-  tags$div(
    style = "position: fixed; bottom: 10px; right: 10px;",
    tags$a(href = "https://github.com/noble-g", target = "_blank", bs_icon("github")),
    tags$a(href = "https://www.linkedin.com/in/oloyede-abdulganiyu", target = "_blank", bs_icon("linkedin", class = "fa-3x")),
    tags$a(href = "https://twitter.com/NobleGee6", target = "_blank", bs_icon("twitter"))
  )

### UI Function
ui <- fluidPage(theme = bs_theme(bootswatch = "darkly", 
                                 success = "#95dc4e",
                                 primary = "#f3b433",
                                 secondary = "#e5e5e5",
                                 warning = "#33df89"),
                
                navbarPage("Iris Analysis", 
                           tabPanel("Iris Description", 
                                    page_sidebar(sidebar=  sidebar(tags$h2("NOBLE", 
                                                                           style = "font-weight: bold; font-style: italic; text-align: center; color: #f3b433 ;"),
                                                               sliderInput("SLrange", "Sepal Length Range ", min = min(data$Sepal.Length), max = max(data$Sepal.Length), value = c(4.4,6.55) ),
                                                               sliderInput("SWrange", "Sepal Width Range ", min = min(data$Sepal.Width), max = max(data$Sepal.Width), value = c(3,4) ),
                                                               sliderInput("PLrange", "Petal Length Range ", min = min(data$Petal.Length), max = max(data$Petal.Length), value = c(4,5) ),
                                                               sliderInput("PWrange", "Petal Width Range ", min = min(data$Petal.Width), max = max(data$Petal.Width), value = c(1,2) )
                                                              ),
                                                 tags$style(HTML("
                                                      .table-darkly {
                                                                      color: white;
                                                                      background:#e5e5e5 ;
                                                      }
                                        ")),
                                      h2(tags$b("About The Data")),
                                      h3("Description"),
                                      p("This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length 
                                  and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor,
                                  and virginica."),
                                      h3("Structure of The Data"),
                                      tags$pre(verbatimTextOutput("data_structure")), ## to print the structure of the data
                                      p("iris is a data frame with 150 cases (rows) and 5 variables (columns) named Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species."),
                                      ## Data Table
                                      uiOutput("table", class = "table-darkly"),
                    
                                      h3("References"),
                                      tags$ul("Fisher, R. A. (1936) The use of multiple measurements in taxonomic problems. Annals of Eugenics, 7, Part II, 179–188.", 
                                              "The data were collected by Anderson, Edgar (1935). The irises of the Gaspe Peninsula, Bulletin of the American Iris Society, 59, 2–5."),
                                      # Social media icons
                                      sm_icons
                                    )## close sidebar
                                    ), ## close tabpanel
                           tabPanel("Dashboard", 
                                    page(card(tags$h2("IRIS DASHBOARD", 
                                                      style = "font-weight: bold; font-style: italic; text-align: center;")),
                                        #### The average cards
                                        layout_columns(
                                          value_box(title = "Sepal Length Mean", 
                                                    verbatimTextOutput("sepal_length_mean"),
                                                    theme = "success",
                                                    showcase = bs_icon("align-end")),
                                          value_box(title = "Sepal Width Mean", 
                                                    verbatimTextOutput("sepal_width_mean"),
                                                    theme = "secondary",
                                                    showcase = bs_icon("balloon-fill")),
                                          value_box(title = "Petal Length Mean", 
                                                    verbatimTextOutput("petal_length_mean"),
                                                    theme = "primary",
                                                    showcase = bs_icon("flower3")),
                                          value_box(title = "Petal Width Mean", 
                                                    verbatimTextOutput("petal_width_mean"),
                                                    theme = "warning",
                                                    showcase = bs_icon("flower1")),
                                        #### The Scatter Plots
                                          card(card_header("Sepal Length vs Sepal Width"), 
                                               plotOutput("scatter_plot_sepal")),
                                          card(card_header("Petal Length vs Petal Width"),
                                              plotOutput("scatter_plot_petal")),
                                        #### The correlation plot and Box plot
                                          card(card_header("Correlation Plot"), 
                                                         plotOutput("correlation_plot")),
                                          card(card_header("Boxplot"), 
                                                           plotOutput("boxplot")),
                                        #### The Report
                                        card(card_header("Report"), 
                                                         verbatimTextOutput("findings")),
                                        col_widths = c(3,3,3,3,6,6,4,8, 12),
                                        row_heights = c(0.7, 3,3,3),
                                        # Social media icons
                                        sm_icons
                                        )
                                    )),
                           tabPanel("Prediction", 
                                    page_sidebar(sidebar = sidebar(tags$h2("NOBLE", 
                                                                           style = "font-weight: bold; font-style: italic; text-align: center; color: #f3b433 ;"),
                                                              sliderInput("SLpoint", "Sepal Length", min = min(data$Sepal.Length), max = max(data$Sepal.Length), value = 6.55),
                                                               sliderInput("SWpoint", "Sepal Width", min = min(data$Sepal.Width), max = max(data$Sepal.Width), value = 4),
                                                               sliderInput("PLpoint", "Petal Length", min = min(data$Petal.Length), max = max(data$Petal.Length), value = 5),
                                                               sliderInput("PWpoint", "Petal Width", min = min(data$Petal.Width), max = max(data$Petal.Width), value = 2),
                                                               actionButton("Predict", "Predict", class = "btn btn-primary")
                                    ),
                                      h2(tags$b("Predict The Data")),
                                      tableOutput("live_table"),
                                      p("Here you choose the data for the four independent variables - ..., and predict the "),
                                      h3("Logistics Regression"),
                                      textOutput("content"),
                                      tableOutput("lr_table"),
                                      h3("Random Forest"),
                                      ## Model
                                      tableOutput('rf_table'), #Prediction of the result table
                                    # Social media icons
                                    sm_icons
                                     )),
                           tabPanel("Model Diagonistics",
                                    #page_fil("Evaluation and Diagonistis",
                                      layout_columns(
                                        ## Logistics Regression
                                        tags$h2("Evaluating The Logistics Regression Model"),
                                        card("Logistic regression is a statistical method used for binary classification, which means it predicts the probability of occurrence of an event by fitting data to a logistic function. It's commonly used when the dependent variable is binary (i.e., it takes only two possible outcomes).\n
                                             Logistic regression models the probability that a given input belongs to a particular class. The output of logistic regression is transformed using the sigmoid function (also known as the logistic function), which ensures that the output value falls between 0 and 1. The sigmoid function is defined as: \n
                                             During the training phase, logistic regression learns the relationship between the input features and the binary outcome by estimating the coefficients (weights) that minimize the error between the predicted probabilities and the actual outcomes in the training data. \n,
                                              Logistic regression uses a cost function called the log loss (or cross-entropy) function to measure the error between the predicted probabilities and the true labels. The goal is to minimize this error using optimization algorithms like gradient descent. \n
                                             Once the model is trained, it can be used to predict the probability that a new instance belongs to the positive class (usually labeled as 1). A decision boundary is set at a threshold value (typically 0.5), and instances with predicted probabilities above this threshold are classified as positive, while those below are classified as negative. \n
                                             Logistic regression is widely used in various fields such as healthcare, finance, marketing, and social sciences due to its simplicity, interpretability, and effectiveness for binary classification tasks. However, it's important to note that logistic regression assumes a linear relationship between the input features and the log odds of the outcome."),
                                        # Confusion matrix
                                        card(card_header()),
                                        card(),
                                        # Accuracy
                                        card(card_header()),
                                        card(),
                                        # AUC and ROC curve
                                        card(card_header()),
                                        card(),
                                        ## Random Forest
                                        tags$h2("Evaluating The Random Forest Model"),
                                        card(),
                                        # Feature importance
                                        card(card_header()),
                                        card(),
                                        # Confusion Matrix
                                        card(card_header()),
                                        card(),
                                        # Cross Validation
                                        card(card_header()),
                                        card(),
                                        col_widths = c(12, 12,6,6,6,6,6,6,12, 12,6,6,6,6,6,6),
                                        row_heights = c(0.7, 3,3,3),
                                        # Social media icons
                                        sm_icons
                                      )
                                    )),
                )


                
                
### Server function
server <- function(input, output, session) {
  
  ## the Data Description Page
  
  # The structure of the data
  output$data_structure <- renderPrint({
    str(data)
  })
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
  output$table <- renderUI({
    table <- DT::datatable(filtered_data(), options = list(pageLength = 20))
    table
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
      geom_point(size = 4) +
      theme(legend.position = "bottom")
  })
  # Create scatter plot for Petal Length vs Petal Width
  output$scatter_plot_petal <- renderPlot({
    ggplot(data, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
      geom_point(size = 4)+
      theme(legend.position = "bottom")
  })
  
  # Create correlation plot
  output$correlation_plot <- renderPlot({
    cor_matrix<- cor(data[, 1:4])
    #colors <- c("#95dc4e", "#f3b433", "#e5e5e5", "#33df89")
    corrplot(cor_matrix, 
             method = "square",
             type = "full",
             addCoef.col = "black")
  })
  
  # Create boxplot
  output$boxplot <- renderPlot({
    colors <- c("#95dc4e", "#f3b433", "#e5e5e5", "#33df89")
    # Create a boxplot of all four numeric variables
    boxplot(data[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")],
            main = "Boxplot of Iris Dataset",
            xlab = "Variables",
            ylab = "Measurement", 
            col = colors)
  })
  
  # Generate Report
  output$findings <- renderPrint({
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
  
  # The Model Page
  
  #### Live data on the web app model prediction page
  live_data <- reactive({
    df<- data.frame(
      Sepal.Length = input$SLpoint,
      Sepal.Width = input$SWpoint,
      Petal.Length = input$PLpoint,
      Petal.Width = input$PWpoint)
    return(df)
  })
  # show live data
  output$live_table <- renderTable({
    live_data()
  })
  
  lr_df <- reactive({
    
    if (input$Predict > 0) {
    # Predict on the dataframe
    lr_pred <- predict(lr_model, newdata = live_data(), type = "raw")
    lr_prob <- predict(lr_model, newdata = live_data(), type = "prob")
    
    # Combine predictions and probabilities into a dataframe
    result <- data.frame(
      Prediction = as.character(lr_pred[1]),
      Setosa_Prob = lr_prob[, "setosa"],
      Versicolor_Prob = lr_prob[, "versicolor"],
      Virginica_Prob = lr_prob[, "virginica"]
      )
    return(result)
    } else {
      click<- "Click on the Predict Button 🚀🏳"
      return(click)
    }
  })
  # Output the result dataframe
  output$lr_table <- renderTable({
    lr_df()
  })
  
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$Predict > 0) { 
      ###
    }
  })
  
  
  
  
  rf_df <- reactive({
    
    if (input$Predict > 0) {
      # Predict on the dataframe
      rf_pred <- predict(rf_model, newdata = live_data(), type = "raw")
      rf_prob <- predict(rf_model, newdata = live_data(), type = "prob")
      
      # Combine predictions and probabilities into a dataframe
      result_rf <- data.frame(
        Prediction = as.character(rf_pred[1]),
        Setosa_Prob = rf_prob[, "setosa"],
        Versicolor_Prob = rf_prob[, "versicolor"],
        Virginica_Prob = rf_prob[, "virginica"]
      )
      return(result_rf)
    } else {
      click<- "Click on the Predict Button 🚀🏳"
      return(click)
    }
  })
  # Output the result dataframe
  output$rf_table <- renderTable({
    rf_df()
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$Predict > 0) { 
      live_data()
    }
  })
  
}

## Shiny app function
shinyApp(ui = ui, server = server)





## confusion matrix and its derivatives - accuracy, loss fn, ...
## Model Diagonistics
## AUC and ROC curve
## comparing the two models


### Deployment
#digital ocean
#google drive
# shinyapps.io


