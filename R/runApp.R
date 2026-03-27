#' Launch the shinyTAG application
#'
#' @description
#'   Starts the shinyTAG Shiny web application for interactive exploration of
#'   gutenTAG MALDI-MSI results. The interface provides a data import sidebar,
#'   a single-channel ion image viewer, an interactive metapeak spectrum panel,
#'   and a tabbed QC dashboard.
#'
#' @param ... Arguments passed to \code{\link[shiny]{shinyApp}}.
#'
#' @return A \code{shiny.appobj}; typically run for its side-effect.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   runShinyTAG()
#' }
runShinyTAG <- function(...) {
  shiny::shinyApp(ui = .shinytag_ui(), server = .shinytag_server, ...)
}

# ── Internal UI ───────────────────────────────────────────────────────────────

.shinytag_ui <- function() {
  bslib::page_fluid(
    title = "shinyTAG",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    shiny::fluidRow(
      shiny::column(3,
        bslib::card(
          bslib::card_header("Data Import"),
          mod_importUI("import")
        ),
        bslib::card(
          bslib::card_header("Quality Control"),
          mod_qcUI("qc")
        )
      ),
      shiny::column(9,
        bslib::card(
          bslib::card_header("Ion Image"),
          mod_imageUI("image")
        ),
        bslib::card(
          bslib::card_header("Spectrum"),
          mod_spectraUI("spectra")
        )
      )
    )
  )
}

# ── Internal Server ───────────────────────────────────────────────────────────

.shinytag_server <- function(input, output, session) {
  data    <- mod_importServer("import")
  channel <- mod_spectraServer("spectra", data)
  mod_imageServer("image", data, channel)
  mod_qcServer("qc", data)
}
