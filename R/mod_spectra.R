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
#'   Renders the interactive metapeak spectrum and returns a reactive
#'   character giving the channel name of the metapeak the user last clicked.
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
      p %>% plotly::event_register("plotly_click")
    })

    clicked <- shiny::reactiveVal(NULL)

    shiny::observe({
      # priority = "event" reads from root session, needed inside modules
      click <- plotly::event_data("plotly_click", source = "A",
                                  priority = "event")
      shiny::req(click, data())

      clicked_mz <- click$x
      d        <- data()
      limits   <- d$metapeaks$metapeaks$limits
      peak_max <- d$metapeaks$metapeaks$max

      # check if click falls inside a metapeak boundary
      in_range <- clicked_mz >= limits[, 1] & clicked_mz <= limits[, 2]
      hit_idx  <- which(in_range)

      # fallback: snap to nearest metapeak centre
      if (length(hit_idx) == 0) {
        hit_idx <- which.min(abs(peak_max - clicked_mz))
      }

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
