## install pkgs
install.packages("DT")


### import Libraries
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)

##Load The data
#### By default, iris is in R
head(iris)

## About the data
## Description: This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.
## Format: iris is a data frame with 150 cases (rows) and 5 variables (columns) named Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, and Species.
## References: Fisher, R. A. (1936) The use of multiple measurements in taxonomic problems. Annals of Eugenics, 7, Part II, 179–188.
# The data were collected by Anderson, Edgar (1935). The irises of the Gaspe Peninsula, Bulletin of the American Iris Society, 59, 2–5.

min(iris$Sepal.Length)
### UI Function
ui <- fluidPage(theme = shinytheme("united"),
                titlePanel("Iris Analysis"),
                sidebarLayout(sidebarPanel(sliderInput("SLrange", "Sepal Length Range ", min = min(iris$Sepal.Length), max = max(iris$Sepal.Length), value = c(4.4,5) ),
                                           sliderInput("SWrange", "Sepal Width Range ", min = min(iris$Sepal.Width), max = max(iris$Sepal.Width), value = c(3,4) ),
                                           sliderInput("PLrange", "Petal Length Range ", min = min(iris$Petal.Length), max = max(iris$Petal.Length), value = c(4,5) ),
                                           sliderInput("PWrange", "Petal Width Range ", min = min(iris$Petal.Width), max = max(iris$Petal.Width), value = c(1,2) )
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
                                tags$ul("Fisher, R. A. (1936) The use of multiple measurements in taxonomic problems. Annals of Eugenics, 7, Part II, 179–188."), 
                                tags$ul("The data were collected by Anderson, Edgar (1935). The irises of the Gaspe Peninsula, Bulletin of the American Iris Society, 59, 2–5.")
                                )
                              )
                )


### Server function
server <- function(input, output) {
  # Reactive filtering of the iris dataset based on slider values
  filtered_data <- reactive({
    subset(iris,
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

### Deployment
#digital ocean
#google drive
# shinyapps.io