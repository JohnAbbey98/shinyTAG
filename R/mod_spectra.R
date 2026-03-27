#' Spectrum Viewer Module UI
#'
#' @description
#'   Renders an interactive \code{\link[gutenTAG]{plotMetapeaks}} plot. Users
#'   click within a metapeak region to select that channel for the image panel.
#'
#' @param id Module namespace ID.
#'
#' @return A \code{\link[plotly]{plotlyOutput}} element.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(mod_spectraUI("spectra"))
#'   shiny::shinyApp(ui, function(input, output, session) {})
#' }
mod_spectraUI <- function(id) {
  ns <- shiny::NS(id)
  plotly::plotlyOutput(ns("spectrum"), height = "400px")
}

#' Spectrum Viewer Module Server
#'
#' @description
#'   Renders the interactive metapeak spectrum and returns a reactive character
#'   giving the marker name of the metapeak the user last clicked. Defaults to
#'   the first channel when no click has occurred.
#'
#' @param id Module namespace ID.
#' @param data A \code{\link[shiny]{reactive}} returning the list produced by
#'   \code{\link{mod_importServer}}.
#'
#' @return A \code{\link[shiny]{reactive}} returning a character scalar — the
#'   selected marker name.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(mod_spectraUI("spectra"))
#'   server <- function(input, output, session) {
#'     data <- shiny::reactive(NULL)
#'     mod_spectraServer("spectra", data)
#'   }
#'   shiny::shinyApp(ui, server)
#' }
mod_spectraServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {

    output$spectrum <- plotly::renderPlotly({
      shiny::req(data())
      d <- data()
      gutenTAG::plotMetapeaks(
        x           = d$processed,
        metapeaks   = d$metapeaks,
        panel       = d$panel,
        interactive = TRUE
      )
    })

    selected_channel <- shiny::reactive({
      shiny::req(data())
      channels <- colnames(data()$processed$IntensityDF)

      click <- plotly::event_data("plotly_click", session = session)
      if (is.null(click)) return(channels[1])

      clicked_mz <- click$x
      limits     <- data()$metapeaks$metapeaks$limits
      peak_max   <- data()$metapeaks$metapeaks$max

      in_range <- clicked_mz >= limits[, 1] & clicked_mz <= limits[, 2]
      hit_idx  <- which(in_range)
      if (length(hit_idx) == 0) return(channels[1])

      hit_mz <- peak_max[hit_idx[1]]
      cm     <- data()$processed$CorrespondenceMatrix
      marker <- cm$marker[cm$mz_location == hit_mz]

      if (length(marker) == 0 || is.na(marker[1])) return(channels[1])
      marker[1]
    })

    selected_channel
  })
}
