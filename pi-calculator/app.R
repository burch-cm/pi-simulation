library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

##### helper functions #####
dist_from_origin <- function(x, y) {
    # origin is c(0, 0)
    sqrt(sum(x^2, y^2))
}

circle_fn <- function(center = c(0,0), radius = 1, n_points = 100){
    tt <- seq(0, 2*pi, length.out = n_points)
    xx <- center[1] + radius * cos(tt)
    yy <- center[2] + radius * sin(tt)
    return(data.frame(x = xx, y = yy))
}


# Define UI 
ui <- dashboardPage(

    # Application title
    dashboardHeader(title = "Calculating Pi by Simulation"),

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
            sliderInput("nPoints", 
                        label = "The number of points to use",
                        value = 1000,
                        min = 100,
                        max = 10000, 
                        step = 100, 
                        round = TRUE)#,
            # actionButton("goButton", "Calculate Pi")
            ),
    dashboardBody(
        fluidRow(
            column(width = 6,
                   box(status = "primary", plotOutput("circlePlot", width = '100%'),
                       width = 12)),
            column(width = 4, 
                   box("This app will calculate the value of Pi based on
                       the number of random points which fall inside
                       of a circle within a square of the same diameter.",
                       width = 12),
                   box(status = "primary", htmlOutput("calcOutput"),
                       width = 12))
        )
    )
)

# Define server logic
server <- function(input, output) {

    # randomPoints <- eventReactive(input$goButton, {
    #     # sample random points
    #     n_points <- input$nPoints
    #     x_val <- runif(n_points, min = -1, max = 1)
    #     y_val <- runif(n_points, min = -1, max = 1)
    #     # calculate distance from center
    #     df <- data.frame(x_val, y_val)
    #     origin_dist <- 
    #         Map(dist_from_origin, df$x_val, df$y_val) |> 
    #         unlist()
    #     df['dist'] = origin_dist
    #     df
    # })
    
    randomPoints <- reactive({
        n_points <- input$nPoints
        x_val <- runif(n_points, min = -1, max = 1)
        y_val <- runif(n_points, min = -1, max = 1)
        # calculate distance from center
        df <- data.frame(x_val, y_val)
        origin_dist <-
            Map(dist_from_origin, df$x_val, df$y_val) |>
            unlist()
        df['dist'] = origin_dist
        df
    })
    
    output$circlePlot <- renderPlot({
        randomPoints() |> 
            ggplot(aes(x = x_val, y = y_val)) +
            geom_point(pch = 19) +
            coord_equal() +
            geom_path(data = circle_points,
                      aes(x = x, y = y),
                      col = 'firebrick1',
                      size = 1.2) +
            geom_polygon(data = circle_points, 
                         aes(x = x, y = y), 
                         fill = 'firebrick1',
                         alpha = 0.5)
    })
    
    output$calcOutput <- renderUI({
        n_points_inside <- 
            randomPoints() |> 
            filter(dist <= 1) |> 
            count() |> 
            pull(n)
        percentage_points <- 
            n_points_inside / input$nPoints
        pi_est <- 4 * percentage_points
        
        str1 <- paste("There are", n_points_inside, "points inside the circle.")
        str2 <- paste("There are", input$nPoints, "points in the square.")
        str3 <- paste("Approximately", percentage_points,
                      "percent of the points are inside the circle.")
        str4 <- paste("This means the estimate of PI is approximately",
                      strong(pi_est))
        
        HTML(paste(str1, str2, str3, str4, sep = "<br/><br/>"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
