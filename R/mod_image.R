#' Image Viewer Module UI
#'
#' @description
#'   Displays a single-channel ion image for the currently selected metapeak.
#'   Channel can be chosen via a dropdown (grouped by targeted/untargeted) or
#'   by clicking on the spectrum panel.
#'
#' @param id Module namespace ID.
#'
#' @return A \code{\link[shiny]{tagList}} containing selectors and a
#'   \code{\link[shiny]{plotOutput}}.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(mod_imageUI("image"))
#'   shiny::shinyApp(ui, function(input, output, session) {})
#' }
mod_imageUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::selectInput(ns("channel"), "Channel", choices = NULL)),
      shiny::column(6, shiny::selectInput(
        ns("palette"), "Palette",
        choices  = c("viridis", "magma", "plasma", "inferno", "cividis"),
        selected = "viridis"
      ))
    ),
    shiny::plotOutput(ns("image"), height = "350px")
  )
}

#' Image Viewer Module Server
#'
#' @description
#'   Populates the channel dropdown when data loads (targeted and untargeted
#'   groups), updates it when the user clicks on the spectrum, and renders the
#'   ion image via \code{\link[gutenTAG]{imageChannel}}.
#'
#' @param id Module namespace ID.
#' @param data A \code{\link[shiny]{reactive}} returning the list produced by
#'   \code{\link{mod_importServer}}.
#' @param clicked_channel A \code{\link[shiny]{reactive}} returning a character
#'   scalar (channel name) or \code{NULL}, produced by
#'   \code{\link{mod_spectraServer}}.
#'
#' @return Invisibly \code{NULL}; called for its side-effect of rendering.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(mod_imageUI("image"))
#'   server <- function(input, output, session) {
#'     data    <- shiny::reactive(NULL)
#'     channel <- shiny::reactive(NULL)
#'     mod_imageServer("image", data, channel)
#'   }
#'   shiny::shinyApp(ui, server)
#' }
mod_imageServer <- function(id, data, clicked_channel) {
  shiny::moduleServer(id, function(input, output, session) {

    # Populate channel dropdown when data loads
    shiny::observeEvent(data(), {
      d <- data()$processed
      targeted   <- colnames(d$IntensityDF)
      all_int    <- as.data.frame(d$AllMetapeaks$AllMetapeaksIntensity)
      all_corr   <- d$AllMetapeaks$AllMetapeaksCorrespondence
      untargeted <- colnames(all_int)[is.na(all_corr$marker)]

      choices <- list(Targeted = targeted, Untargeted = untargeted)
      shiny::updateSelectInput(session, "channel",
                               choices = choices, selected = targeted[1])
    })

    # Update dropdown when spectrum is clicked
    shiny::observeEvent(clicked_channel(), {
      shiny::updateSelectInput(session, "channel", selected = clicked_channel())
    })

    # Render image from whichever channel the dropdown shows
    output$image <- shiny::renderPlot({
      shiny::req(data(), input$channel)
      d        <- data()$processed
      targeted <- colnames(d$IntensityDF)

      if (input$channel %in% targeted) {
        gutenTAG::imageChannel(
          x = d, channel = input$channel, palette = input$palette
        )
      } else {
        all_df <- as.data.frame(d$AllMetapeaks$AllMetapeaksIntensity)
        gutenTAG::imageChannel(
          x = all_df, coords = d$SpatialCoords,
          channel = input$channel, palette = input$palette
        )
      }
    })
  })
}
