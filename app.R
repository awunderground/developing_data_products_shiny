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
                       min = 2, max = 20, value = 4, step = 1),           
           radioButtons("distance", "Distribution type:",
                        c("Euclidean" = "euclidean",
                          "Manhattan" = "manhattan"))
    )
  )
)

server <- function(input, output) {

  output$plot <- renderPlot({
    
    # filter the data based on user options for seasons
    players_subset <-
      players %>%
      filter(season >= input$seasons[1] & season <= input$seasons[2]) %>%
      select(season, last_name, first_name, average_minutes, usage)
    
    # drop player names
    players_numeric <- 
      players_subset %>%
      select_if(is.numeric)
    
    set.seed(20180826)
    
    clusters <- kmeans(players_numeric, centers = input$clusters, nstart = input$clusters)
    
    print(augment(clusters, players_subset))
    
    output$plot <- renderPlot({ augment(clusters, players_subset) %>%
      ggplot(aes(average_minutes, usage, color = .cluster)) +
      geom_point(alpha = 0.5, size = 3)
    })
      
    print(prcomp(players_numeric, scale. = TRUE))
    
  })
}

shinyApp(ui = ui, server = server)





# ??kmeans
