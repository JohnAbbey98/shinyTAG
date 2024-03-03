library(shiny)
library(plotly)
library(ggplot2)
library(cytomapper)

source("/home/ubuntu/github/app/utils.R")

ui <- fluidPage(
  plotlyOutput("metapeaksPlot"),
  verbatimTextOutput("clickedMarker"),
  #verbatimTextOutput("ionImage"),
  #imageOutput("ionImage")
  plotOutput("ionImage")
  
)

server <- function(input, output, session) {
  output$metapeaksPlot <- renderPlotly({
    # create the ggplot
    p <- .createMetapeaksPlot(test_processed, test_metapeaks)
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
      closest_mz <- counts$mz[which.min(abs(counts$mz - clicked_mz))]
      
      # Display the name of the closest marker
      if (nrow(closest) > 0) {
        paste0("Closest marker:", closest$name, " Closest metapeak: ", round(closest$metapeak_mz, 3), " Clicked m/z: ", round(closest_mz, 3))
      } else {
        "No marker found close to the clicked location."
      }
    } else {
      "Click on a marker to see its name."
    }
  }) 
  
  clickedIndex <- reactiveVal(NULL)
  
  observeEvent(event_data("plotly_click", source = "metapeaksPlot"), {
    eventdata <- event_data("plotly_click", source = "metapeaksPlot")
    if (!is.null(eventdata)) {
      clicked_mz <- eventdata$x
      closest <- which.min(abs(targeted_metapeaks$expected_mz - clicked_mz))
      clickedIndex(closest)  # Store the index of the clicked channel
    }
  })
  
  # Dynamically generate and display the ion image based on the clicked channel
  output$ionImage <- renderPlot({
    idx <- clickedIndex()  # Retrieve the stored index
    if (!is.null(idx)) {
      imageChannel(x = test_processed, channel_number = idx)  # Generate the plot
    }
  })
    
    
  # Render the ion image based on the clicked m/z value
  #output$ionImage <- renderText({
  #  event_data <- event_data("plotly_click", source = "metapeaksPlot")
  #  if (!is.null(event_data)) {
  #    clicked_mz <- event_data$x
  #    # find the closest metapeak
  #    closest <- targeted_metapeaks[which.min(abs(targeted_metapeaks$expected_mz - clicked_mz)), ]
  #    # get the index of the closest metapeak
  #    idx <- which(colnames(test_processed$IntensityDF) %in% closest$name)
  #    
  #    ### Before we image the channel we want to print the channel
  #    paste0(head(processed$IntensityDF[idx]))
  #    
  #    ### Image channel
  #    
  #    # image that metapeak
  #    img_path <- imageChannel(processed$IntensityDF, processed$SpatialCoords, idx) # Call your function to get the image path
  #    
  #    # Display the image
  #    tags$img(src = img_path, style = "max-width: 100%; height: auto;")
  #  }
  #})
  # output for imaging the channel that was clicked
  #output$ionImage <- renderUI({
  #  event_data <- event_data("plotly_click", source = "metapeaksPlot")
  #  if (!is.null(event_data)) {
  #    clicked_mz <- event_data$x
  #    # find the closest metapeak
  #    closest <- targeted_metapeaks[which.min(abs(targeted_metapeaks$expected_mz - clicked_mz)), ]
  #    # get the index of the closest metapeak
  #    idx <- which(colnames(test_processed$IntensityDF) %in% closest$name)
  #    
  #    ### Image channel
  #    
  #    # image that metapeak
  #    img_path <- imageChannel(processed, idx) # Call your function to get the image path
  #    #img_path <- imagePixels(processed, closest$name)
  #    
  #    # Display the image
  #    tags$img(src = img_path, style = "max-width: 100%; height: auto;")
  #  }
  #})
    

    
}

# Run the app
shinyApp(ui, server)

