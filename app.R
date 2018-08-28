library(tidyverse)
library(shiny)
library(broom)

players <- read_csv("clean-data/input-data.csv")

ui <- fluidPage(
  
  theme = "shiny.css",
  
  fluidRow(
    column(8,
           plotOutput("plot", 
                      hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
           uiOutput("hover_info")
    ),

    column(4,
           sliderInput(inputId = "seasons", label = "Seasons", 
                       min = 2010, 
                       max = 2016, 
                       value = c(2000, 2016), 
                       step = 1,
                       sep = ""),
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
  

  players_subset <- reactive({    
    # filter the data based on user options for seasons
      players %>%
      filter(season >= input$seasons[1] & season <= input$seasons[2])
  })
  
  players_numeric <- reactive({
    # drop player names
    players_numeric <- 
      players_subset() %>%
      select_if(is.numeric) %>%
      select(-season)
    
    # scale data
    if (input$scale == "raw") {
      players_numeric
    } else if (input$scale == "linear") {
      players_numeric %>%
        mutate_all(scale)   
    } else if (input$scale == "standardize") {
      players_numeric %>%
        mutate_all(stats::rescale, to = c(1, 100))      
    }
  })
  
  clusters <- reactive({
    # set seed for kmeans starting points
    set.seed(20180826)
    
    # fit kmeans model
    kmeans(players_numeric(), centers = input$clusters, nstart = input$clusters)
  })
    
  tidy_clusters <- reactive({
    # create a tibble of data points
    augment(clusters(), players_subset())
  })
  
  
    # # get fit information
    # tidy_fit <- glance(clusters)
   
  pc <- reactive({
    # perform principal components analysis on scaled data
    prcomp(players_numeric(), scale. = FALSE)
  })
  
  tidy_centers <- reactive({
    # project clusters onto the pca space
    predict(pc(), clusters()[["centers"]]) %>%
      as_tibble() %>%
      mutate(.cluster = as.character(row_number()))
  })
   
  plot_data <- reactive({ 
    # create plot
    pc_data <- pc() %>%
      .[["x"]] %>%
      as_tibble()
    
    bind_cols(tidy_clusters(), pc_data)
  })
  
  output$plot <- renderPlot({      
    ggplot() +
      geom_point(data = plot_data(), 
                 aes(PC1, PC2, color = .cluster), 
                 alpha = 0.5, size = 3) +
      geom_point(data = tidy_centers(), 
                 aes(PC1, PC2, fill = .cluster), 
                 alpha = 0.8, size = 5, shape = 24, color = "black", stroke = 2)
      
  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    
    point <- nearPoints(plot_data(), hover, threshold = 20, maxpoints = 1, addDist = TRUE)
    
    print(plot_data())
    
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    tooltip_width <- ifelse(hover$range$right - left_px > 300, "width: 300px;", "")
    
    if (left_pct < 0.75) {
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 10, "px; top:", top_px + 30, "px; cursor: crosshair; padding: 5px;", tooltip_width)
    } else {
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px - 150, "px; top:", top_px + 30, "px; cursor: crosshair; padding: 5px;", tooltip_width)
    }
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Player: </b>", paste(point$season, point$first_name, point$last_name), "<br/>",
                    "<b> Usage: </b>", round(point$usage, 2), "<br/>",
                    "<b> Points per 30 minutes: </b>", round(point$points_per30, 2), "<br/>",
                    "<b> Height: </b>", round(point$height, 2), " inches", 
                    "<b> Pounds per inch: </b>", round(point$pounds_per_inch, 2), "<br/>"
        )))
    )
  })

}

shinyApp(ui = ui, server = server)
