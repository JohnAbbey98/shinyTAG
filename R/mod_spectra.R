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
  plotly::plotlyOutput(ns("spectrum"), height = "450px")
}

#' Spectrum Viewer Module Server
#'
#' @description
#'   Renders the interactive metapeak spectrum and returns a reactive
#'   character giving the channel name of the metapeak the user last clicked.
#'   A raw JavaScript click handler is attached to the plot area so that
#'   clicks anywhere — including on filled metapeak rectangles — are captured.
#'   Returns the marker name for targeted metapeaks and the \code{"<mz> m/z"}
#'   label for untargeted ones.
#'
#' @param id Module namespace ID.
#' @param data A \code{\link[shiny]{reactive}} returning the list produced by
#'   \code{\link{mod_importServer}}.
#'
#' @return A \code{\link[shiny]{reactive}} returning a character scalar or
#'   \code{NULL} when no click has occurred.
#'
#' @importFrom htmlwidgets onRender
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
      p <- gutenTAG::plotMetapeaks(
        x           = d$processed,
        metapeaks   = d$metapeaks,
        panel       = d$panel,
        interactive = TRUE
      )
      # Attach click handler to the outer plotly div. Computes the m/z from
      # clientX, the element bounding-rect, and the left margin so the
      # mapping stays correct after zoom / relayout. Clicks outside the
      # plot area (legend, axes) are ignored.
      p %>% htmlwidgets::onRender(sprintf("
        function(el) {
          el.addEventListener('click', function(evt) {
            var layout = el._fullLayout;
            if (!layout || !layout.xaxis) return;
            var bb     = el.getBoundingClientRect();
            var xPx    = evt.clientX - bb.left - layout.margin.l;
            var yPx    = evt.clientY - bb.top  - layout.margin.t;
            if (xPx < 0 || xPx > layout.xaxis._length) return;
            if (yPx < 0 || yPx > layout.yaxis._length) return;
            var xData  = layout.xaxis.p2d(xPx);
            Shiny.setInputValue('%s',
              {x: xData, nonce: Math.random()},
              {priority: 'event'});
          });
        }
      ", session$ns("clicked_mz")))
    })

    clicked <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$clicked_mz, {
      shiny::req(data())

      clicked_mz <- input$clicked_mz$x
      d        <- data()
      limits   <- d$metapeaks$metapeaks$limits
      peak_max <- d$metapeaks$metapeaks$max

      # only act if click falls inside a metapeak boundary
      in_range <- clicked_mz >= limits[, 1] & clicked_mz <= limits[, 2]
      hit_idx  <- which(in_range)
      if (length(hit_idx) == 0) return()

      hit_mz   <- peak_max[hit_idx[1]]
      all_corr <- d$processed$AllMetapeaks$AllMetapeaksCorrespondence
      match_row <- which(all_corr$mz_location == hit_mz)

      if (length(match_row) > 0 && !is.na(all_corr$marker[match_row[1]])) {
        clicked(all_corr$marker[match_row[1]])
      } else {
        clicked(paste0(round(hit_mz, 2), " m/z"))
      }
    })

    shiny::reactive(clicked())
  })
}
