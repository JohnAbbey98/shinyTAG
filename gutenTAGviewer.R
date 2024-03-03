library(shiny)
library(plotly)
library(ggplot2)

# Assuming you have a function `getIonImage` that takes an m/z value and returns an image
getIonImage <- function(x, mz){  
  
  Cardinal::image(x = x, mz = mz)
  
}


# function for plotting metapeaks
createMetapeaksPlot <- function(processed, metapeaks) {
  
  # get metapeak data
  counts <- metapeaks$count_df
  smooth_counts <- metapeaks$count_smooth_df
  
  # get targeted metapeak locations
  names <- processed$CorrespondenceMatrix$marker
  targeted_metapeak_location <- processed$CorrespondenceMatrix$mz_location
  targeted_expected_location <- processed$CorrespondenceMatrix$expected_mz_location
  targeted_metapeaks <- data.frame(matrix(0, nrow = length(names), ncol = 3))
  colnames(targeted_metapeaks) <- c("name", "metapeak_mz", "expected_mz")
  targeted_metapeaks$name <- names
  targeted_metapeaks$metapeak_mz <- targeted_metapeak_location
  targeted_metapeaks$expected_mz <- targeted_expected_location
  
  # get untargeted metapeak locations
  untargeted_metapeak_location <- processed$Untargeted$UntargetedCorrespondence$mz_location
  untargeted_metapeaks <- data.frame(matrix(0, nrow = length(untargeted_metapeak_location), ncol = 1))
  colnames(untargeted_metapeaks) <- c("untargeted_mz")
  untargeted_metapeaks$untargeted_mz <- untargeted_metapeak_location
  
  # Plot
  mz_range <- c(800, 2000) 
  r <- ggplot() +
    theme_minimal() +
    ggtitle("Metapeaks") +
    geom_line(data = counts, aes(x = mz, y = count), color = 'black', alpha = 1) +
    geom_vline(data = targeted_metapeaks, aes(xintercept = expected_mz, text = name), color = 'firebrick2', size = 0.5) +
    geom_vline(data = untargeted_metapeaks, aes(xintercept = untargeted_mz), color = 'orange', linetype = 'dashed', size = 0.5) +
    geom_vline(data = targeted_metapeaks, aes(xintercept = metapeak_mz), color = 'springgreen3', linetype = 'dashed', size = 0.5) +
    geom_area(data = smooth_counts, aes(x = mz, y = count), fill = '#6062eb', alpha = 0.5) +
    coord_cartesian(xlim = mz_range) +
    ylab('Peak Counts')
  
  #q <- ggplotly(r, dynamicTicks = TRUE, tooltip = "all", source = "metapeaksPlot") %>%
  #  rangeslider() %>%
  #  layout(hovermode = "x") %>% 
  #  event_register(p, "plotly_click")
  
  return(r)
}



ui <- fluidPage(
  plotlyOutput("metapeaksPlot"),
  verbatimTextOutput("clickedMarker")
)

server <- function(input, output, session) {
  output$metapeaksPlot <- renderPlotly({
    # create the ggplot
    p <- createMetapeaksPlot(test_processed, test_metapeaks)
    # source = "metapeaksPlot" seems to be very important for linking the event_data function to this plot, enabling clickable plots
    ggplotly(p, dynamicTicks = TRUE, tooltip = "all", source = "metapeaksPlot") %>%
      rangeslider() %>%
      layout(hovermode = "x")
    })
    

  output$clickedMarker <- renderText({
    event_data <- event_data("plotly_click", source = "metapeaksPlot")
    if (!is.null(event_data)) {
      # extract the m/z that was clicked
      clicked_mz <- event_data$x
      # Find the closest metapeak value and expected m/z location to the clicked location
      closest <- targeted_metapeaks[which.min(abs(targeted_metapeaks$expected_mz - clicked_mz)), ]
      # Find the closest untargeted metapeak m/z value
      closest_untargeted <- untargeted_metapeaks[which.min(abs(untargeted_metapeaks$expected_mz - clicked_mz)), ]
      
      # Display the name of the closest marker
      if (nrow(closest) > 0) {
        paste("Clicked marker name:", closest$name, " Clicked metapeak: ", closest$metapeak_mz, "Clicked m/z: ")
      } else {
        "No marker found close to the clicked location."
      }
    } else {
      "Click on a marker to see its name."
    }
  })
}

# Run the app
shinyApp(ui, server)

