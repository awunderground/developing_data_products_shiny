library(tidyverse)
library(shiny)
library(broom)

players <- read_csv("clean-data/input-data.csv")

ui <- fluidPage(
  fluidRow(
    column(8,
           plotOutput("plot")
    ),
    
    column(4,
           sliderInput(inputId = "seasons", label = "Seasons", 
                       min = 2010, max = 2016, value = c(2000, 2016), step = 1),
           sliderInput(inputId = "clusters", label = "Number of Clusters", 
                       min = 2, max = 10, value = 3, step = 1),           
           radioButtons("distance", "Distribution type:",
                        c("Euclidean" = "euclidean",
                          "Manhattan" = "manhattan")),
           radioButtons("scale", "Type of scaling",
                        c("Raw" = "raw",
                          "Linear" = "linear",
                          "Standardized" = "standardized"))
    )
  )
)

server <- function(input, output) {

  output$plot <- renderPlot({
    
    # filter the data based on user options for seasons
    players_subset <-
      players %>%
      filter(season >= input$seasons[1] & season <= input$seasons[2])
    
    # drop player names
    players_numeric <- 
      players_subset %>%
      select_if(is.numeric)
    
    # scale data
    if (input$scale == "raw") {
      # do nothing
    } else if (input$scale == "linear") {
      players_numeric <- players_numeric %>%
        mutate_all(scale)   
    } else if (input$scale == "standardize") {
      players_numeric <- players_numeric %>%
        mutate_all(stats::rescale, to = c(1, 100))      
    }

    # set seed for kmeans starting points
    set.seed(20180826)
    
    # fit kmeans model
    clusters <- kmeans(players_numeric, centers = input$clusters, nstart = input$clusters)
    
    # create a tibble of data points
    tidy_clusters <- augment(clusters, players_subset)
    
    # perform principal components analysis on scaled data
    pc <- prcomp(players_numeric, scale. = FALSE)
    
    # project clusters onto the pca space
    tidy_centers <- predict(pc, clusters[["centers"]]) %>%
      as_tibble() %>%
      mutate(.cluster = as.character(row_number()))
    
    print(tidy_centers)
    
    # create plot
    pc_data <- pc %>%
      .[["x"]] %>%
      as_tibble()
    
    tidy_clusters <- bind_cols(tidy_clusters, pc_data)
        
    ggplot() +
      geom_point(data = tidy_clusters, 
                 aes(PC1, PC2, color = .cluster), 
                 alpha = 0.5, size = 3) +
      geom_point(data = tidy_centers, 
                 aes(PC1, PC2, fill = .cluster), 
                 alpha = 0.8, size = 5, shape = 24, color = "black", stroke = 2)
      
  })
}

shinyApp(ui = ui, server = server)
