#' Launch the shinyTAG application
#'
#' @description
#'   Starts the shinyTAG Shiny web application for interactive exploration of
#'   gutenTAG MALDI-MSI results. The interface offers two modes: Processing
#'   Mode (full pipeline run with ion image, spectrum, and QC panels) and
#'   Preview Mode (fast, downsampled parameter tuning across the six pipeline
#'   steps). The mode toggle lives at the top of the Data Import panel.
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
      shiny::column(2,
        bslib::card(
          bslib::card_header("Data Import"),
          mod_importUI("import")
        )
      ),
      shiny::column(10,
        shiny::conditionalPanel(
          condition = "input['import-app_mode'] !== 'preview'",
          shiny::fluidRow(
            shiny::column(6,
              bslib::card(
                bslib::card_header("Ion Image"),
                mod_imageUI("image")
              )
            ),
            shiny::column(6,
              bslib::card(
                bslib::card_header("Quality Control"),
                mod_qcUI("qc")
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(12,
              bslib::card(
                bslib::card_header("Spectrum"),
                mod_spectraUI("spectra")
              )
            )
          )
        ),
        shiny::conditionalPanel(
          condition = "input['import-app_mode'] === 'preview'",
          bslib::card(
            bslib::card_header("Preview Mode \u2014 Parameter Exploration"),
            mod_previewUI("preview")
          )
        )
      )
    )
  )
}

# ── Internal Server ───────────────────────────────────────────────────────────

.shinytag_server <- function(input, output, session) {
  preview_accepted <- mod_previewServer("preview")
  data <- mod_importServer("import", preview_params = preview_accepted)
  channel <- mod_spectraServer("spectra", data)
  mod_imageServer("image", data, channel)
  mod_qcServer("qc", data)
}
