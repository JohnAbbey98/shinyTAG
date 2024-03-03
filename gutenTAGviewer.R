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
  
  q <- ggplotly(r, dynamicTicks = TRUE, tooltip = "all") %>%
    rangeslider() %>%
    layout(hovermode = "x")
  
  return(q)
}



# define the UI
ui <- fluidPage(
  plotlyOutput("metapeaksPlot"),
  uiOutput("selectedIonImage")
)

server <- function(input, output, session) {
  output$metapeaksPlot <- renderPlotly({
    # Convert your ggplot object to a plotly object
    p <- createMetapeaksPlot(test_processed, test_metapeaks)
    
    # Customize the plotly object to add custom data attributes or identifiers if needed
    p
  })
  
  
  output$selectedIonImage <- renderUI({
    req(input$metapeaksPlot_click) # Require a click event to proceed
    
    # Extract the identifier for the clicked line, assuming you've set up custom data attributes
    clicked_mz <- input$metapeaksPlot_click$customdata
    
    # Use your imaging function to get the image for the clicked m/z value
    img_path <- getIonImage(clicked_mz) # Implement this function to return the path of the generated image
    
    # Return an image UI element to display
    tags$img(src = img_path)
  })
  
  # Use event handling to capture clicks on the plot
  observeEvent(input$metapeaksPlot_click, {
    # Handle click event, e.g., display the ion image corresponding to the clicked line
  })
}#

# Run the Shiny App
shinyApp(ui, server)
