library(tidyverse)
library(shiny)
library(broom)

players <- read_csv("clean-data/input-data.csv")

ui <- fluidPage(
  
  theme = "shiny.css",
  
  fluidRow(
    
    column(8,
           h2("Clustering VCU basketball players (2000-2016)"),
           p("This application uses k-means clustering to cluster VCU basketball 
             players based on their size and statistics. Users can manipulate 
             the pre-processing and parameters of the clustering process. Players 
             sharing the same cluster have similar playing styles. Hover over 
             points or scroll to the bottom to learn more.")
    )
  ),
  
  fluidRow(
    
    column(8,
           plotOutput("plot", 
                      hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
           uiOutput("hover_info"),
           p("PC1 and PC2 are the first and second principle components from a 
             principle component analysis (PCA). PCA is a transformation that 
             converts many variables into a set of linearly uncorrelated 
             variables."),
           p(HTML("<b>Type of scaling</b> - k-means clustering is sensitive to the distributions of variables. 
                  In particular, variables with large ranges behave differently than variables with small 
                  ranges. <b>Raw</b> uses data that aren't pre-processed. <b>Linear</b> transforms all variables so they 
                  have a 1 to 100 scale. <b>Standardized</b> subtracts the mean from each variable and then divides 
                  by the standard deviation.")),
           p(HTML("<b>Point or names</b> - toggles between points and names on the plots."))
    ),

    
    column(4,
           sliderInput(inputId = "seasons", label = "Seasons", 
                       min = 2000, 
                       max = 2016, 
                       value = c(2010, 2016), 
                       step = 1,
                       sep = ""),
           sliderInput(inputId = "clusters", label = "Number of Clusters", 
                       min = 2, max = 10, value = 3, step = 1),           
           radioButtons("scale", "Type of scaling",
                        c("Raw" = "raw",
                          "Linear" = "linear",
                          "Standardized" = "standardized")),
           radioButtons("point_type", "Points or names",
                        c("Points" = "points",
                          "Names" = "names"))
    )
  ),
  
  fluidRow(
    column(12, 
           h2("Data"),
           DT::DTOutput("data_table"),
           
           p(HTML("<p><b>MPG</b> - minutes per game</p>
             <p><b>Usage</b> - usage rate. A measure of the number of possessions used by a player.</p>
             <p><b>Points/30</b> - points per 30 minutes</p>
             <p><b>TS</b> - true shooting proportion. </p> 
             <p><b>FTR</b> - free throw rate. Number of free throw attempts divided by true shooting attempts.</p> 
             <p><b>3PR</b> - three point rate Number of three point attempts divided by true shooting attempts.</p>
             <p><b>Assists/30</b> - assists per 30 minutes</p>
             <p><b>TO/30</b> - turnovers per 30 minutes</p>
             <p><b>OR/30</b> - offensive rebounds per 30 minutes</p>
             <p><b>DR/30</b> - defensive rebounds per 30 minutes</p>
             <p><b>Steals/30</b> - steals per 30 minutes</p>
             <p><b>Blocks/30</b> - blocks per 30 minutes</p>
             <p><b>Fouls/30</b> - fouls per 30 minutes</p>
             <p><b>Pounds/inch</b> - pounds per inch of height</p>
             <p><b>Height</b> - height in inches</p>")),
           br(),
           p(HTML("All code is available on <a href = 'https://github.com/awunderground/developing_data_products_shiny'>GitHub</a>")),
           p(HTML("<a href = 'https://twitter.com/awunderground'>Twitter</a>"))
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
    } else if (input$scale == "standardized") {
      players_numeric %>%
        mutate_all(scales::rescale, to = c(1, 100))      
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
    gg <- ggplot() +
      geom_point(data = tidy_centers(), 
                 aes(PC1, PC2, fill = .cluster), 
                 alpha = 0.8, size = 5, shape = 24, color = "black", stroke = 2)
     
    if (input$point_type == "points") { 
      gg +
        geom_point(data = plot_data(), 
                   aes(PC1, PC2, color = .cluster), 
                   alpha = 0.5, size = 3)
    } else if (input$point_type == "names") {
      gg +
        geom_text(data = plot_data(), 
                   aes(PC1, PC2, color = .cluster, label = paste(season, last_name)), 
                   alpha = 0.8, size = 3)   
    }
  
  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    
    point <- nearPoints(plot_data(), hover, threshold = 20, maxpoints = 1, addDist = TRUE)
  
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
  
  output$data_table <- DT::renderDT({
    players_subset() %>%
      select(-first_name) %>%
      mutate_if(is.numeric, round, digits = 2) %>%
      rename(Year = season,
             Name = last_name,
             MPG = average_minutes,
             Usage = usage,
             Games = games_played,
             `Points/30` = points_per30,
             TS = true_shooting_proportion,
             FTR = free_throw_rate,
             `3PR` = three_point_rate,
             `Assists/30` = assists_per30,
             `TO/30` = turnovers_per30,
             `OR/30` = offensive_rebounds_per30,
             `DR/30` = defensive_rebounds_per30,
             `Steals/30` = steals_per30,
             `Blocks/30` = blocks_per30,
             `Fouls/30` = fouls_per30,
             `Pounds/inch` = pounds_per_inch,
             Height = height_inches)
  })

}

shinyApp(ui = ui, server = server)
