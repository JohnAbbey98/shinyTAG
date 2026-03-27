#' Image Viewer Module UI
#'
#' @description
#'   Displays a single-channel ion image for the currently selected metapeak,
#'   with a colour palette selector.
#'
#' @param id Module namespace ID.
#'
#' @return A \code{\link[shiny]{tagList}} containing a palette selector and a
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
    shiny::selectInput(
      ns("palette"), label = NULL,
      choices  = c("viridis", "magma", "plasma", "inferno", "cividis"),
      selected = "viridis",
      width    = "180px"
    ),
    shiny::plotOutput(ns("image"), height = "420px")
  )
}

#' Image Viewer Module Server
#'
#' @description
#'   Renders the ion image for the channel selected in the spectrum panel using
#'   \code{\link[gutenTAG]{imageChannel}}.
#'
#' @param id Module namespace ID.
#' @param data A \code{\link[shiny]{reactive}} returning the list produced by
#'   \code{\link{mod_importServer}}.
#' @param channel A \code{\link[shiny]{reactive}} returning a character scalar
#'   — the selected marker name — produced by \code{\link{mod_spectraServer}}.
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
mod_imageServer <- function(id, data, channel) {
  shiny::moduleServer(id, function(input, output, session) {
    output$image <- shiny::renderPlot({
      shiny::req(data(), channel())
      gutenTAG::imageChannel(
        x       = data()$processed,
        channel = channel(),
        palette = input$palette
      )
    })
  })
}
