# load libraries
library(shiny)
library(plotly)
library(ggplot2)
library(cytomapper)
library(imager)

# load functions from utils.R file
source("/home/ubuntu/github/app/R/utils.R")

# specify ui
ui <- fluidPage(
  plotlyOutput("metapeaksPlot"),
  verbatimTextOutput("clickedMarker"),
  plotOutput("ionImage")

)

# specify the output
server <- function(input, output, session) {

  # 1. Metapeak Segmentation Plot
  output$metapeaksPlot <- renderPlotly({

    # create the metapeak segmentation plot
    p <- .createMetapeaksPlot(processed, metapeaks)

    # source = "metapeaksPlot" seems to be very important for linking the event_data function to this plot, enabling clickable plots
    ggplotly(p, dynamicTicks = TRUE, tooltip = "all", source = "metapeaksPlot") %>%
      rangeslider() %>%
      layout(hovermode = "x")

    })


  # 2. Output text of the clicked location
  output$clickedMarker <- renderText({
    event_data <- event_data("plotly_click", source = "metapeaksPlot")
    if (!is.null(event_data)) {

      # extract the m/z that was clicked
      clicked_mz <- event_data$x
      # Find the closest metapeak value and expected m/z location to the clicked location
      closest_targeted <- targeted_metapeaks[which.min(abs(targeted_metapeaks$expected_mz - clicked_mz)), ]
      # Find the closest m/z value
      closest_mz <- counts$mz[which.min(abs(counts$mz - clicked_mz))]

      # find closest untargeted metapeak
      untargeted_mz <- untargeted_metapeaks$untargeted_mz
      closest_untargeted <- untargeted_mz[which.min(abs(untargeted_mz - clicked_mz))]


      # Display the name of the closest marker
      if (nrow(closest_targeted) > 0) {
        paste0("Closest marker: ", closest_targeted$name, " Closest metapeak: ", round(closest_targeted$metapeak_mz, 3), " Closest untargeted metapeak: ",  round(closest_untargeted, 3), " Clicked m/z: ", round(closest_mz, 3))
      } else {
        "No marker found close to the clicked location."
      }
    } else {
      "Click on a marker to plot the nearest metapeak at that m/z value."
    }
  })


  # 3. Plot the image of the clicked location

  # initialise clickedIndex and useTargetedMetapeaks as a reactive values
  clickedIndex <- reactiveVal(NULL)
  useTargetedMetapeaks <- reactiveVal(TRUE)

  observeEvent(event_data("plotly_click", source = "metapeaksPlot"), {
    eventdata <- event_data("plotly_click", source = "metapeaksPlot")
    if (!is.null(eventdata)) {

      # stores the clicked location
      clicked_mz <- eventdata$x

      # Find the closest targeted metapeak value and expected m/z location to the clicked location
      closest_targeted <- which.min(abs(targeted_metapeaks$expected_mz - clicked_mz))
      distance_targeted <- min(abs(targeted_metapeaks$expected_mz - clicked_mz))

      # Find the closest untargeted metapeak value and expected m/z location to the clicked location
      untargeted_mz <- untargeted_metapeaks$untargeted_mz
      closest_untargeted <- which.min(abs(untargeted_mz - clicked_mz))
      distance_untargeted <- min(abs(untargeted_mz - clicked_mz))


      # if targeted is closer or equal to untargeted, plot targeted
      if (distance_targeted <= distance_untargeted) {
        clickedIndex(closest_targeted)
        useTargetedMetapeaks(TRUE)

      # if untargeted is closer than targeted, plot untargeted
      }else {
        clickedIndex(closest_untargeted)
        useTargetedMetapeaks(FALSE)
      }
    }
  })


  # Dynamically generate and display the ion image based on the clicked channel
  output$ionImage <- renderPlot({
    idx <- clickedIndex()  # Retrieve the stored index
    if (!is.null(idx)) {
      if (useTargetedMetapeaks()){
        .imageChannel(x = processed, channel_number = idx, method = "targeted")
      } else{
        .imageChannel(x = processed, channel_number = idx, method = "untargeted")
      }
    }
  })

}

# Run the app
shinyApp(ui, server)

