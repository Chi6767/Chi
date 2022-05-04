library(shinythemes)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(recipes)
library(dplyr)
library(tidyr)
library(readxl)

library(readxl)
library(vegan) # for clustering
library(cluster) # for clustering
library(readr)



# Define UI for application
ui = fluidPage(
  navbarPage("Chinenye Chinwego's Clustering App",
             tabPanel("About",
                      tabName = "welcome",
                      icon=icon("home"),
                      
                      fluidPage(theme=shinytheme("united"),
                                h1("Welcome!"),
                                br(),
                                br(),
                                br(),
                                br(),
                                p(strong("This App is about:")),
                                
                                h1("Rare Earth Element Composition Analysis"),
                                br(),
                                p(strong("The Algorithim used for this analysis is CLUSTERING")), 
                                p(tags$cite("Clustering is unsupervised Machine learning, thus the information given to the model becomes very important.")),
                                p(tags$cite("I performed dimensionality reduction on the features by constructing new features using Principal Component Analysis(PCA).")),
                                p(tags$cite("This app will let us apply clustering on either the original features or the principal components (PC's),")),
                                p(tags$cite("which will help us to understand the data better and create new knowledge.")),
                                
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                
                                p("The data set was obtained from a paper titled:"),
                                
                                p(tags$blockquote("Trends in the Rare Earth Element Content of U.S.-Based Coal Combustion Fly Ashes")),
                                p(tags$cite("by")),
                                p(tags$body("Ross K. Taggart, James C. Hower, Gary S. Dwyer, and Heileen Hsu-Kim")), 
                                p(tags$body("Environmental Science & Technology 2016 50 (11), 5919-5926")), 
                                p(tags$body("DOI: 10.1021/acs.est.6b00085")),
                                
                                
                                
                      )),
             
             
             
             tabPanel("Cluster",
                      icon=icon("chart-bar"),
                      
                      
                      # sidebarLayout(
                      ui <- pageWithSidebar(                   
                        headerPanel('clustering (k-means)'),
                        sidebarPanel(
                          selectInput('xcol', 'X Variable', names(REE_PC)),
                          selectInput('ycol', 'Y Variable', names(REE_PC),
                                      selected=names(REE_PC)[[2]]),
                          numericInput('clusters', 'Cluster count', 3,
                                       min = 5, max = 5)
                        ),
                        mainPanel(
                          plotOutput("plot1"),       
                          
                          
                        )
                      ))))
server <- function(input, output, session) {
  # Define server logic 
  #server <- function(input, output) {
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    REE_PC[, c(input$xcol, input$ycol)]
  })
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  output$plot1 <- renderPlot({
    palette(c("#999999", "#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 16, cex = 2, lwd = 4)
  })
} 



# Run the application 
shinyApp(ui = ui, server = server)

