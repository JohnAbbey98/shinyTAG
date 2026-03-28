#' QC Dashboard Module UI
#'
#' @description
#'   Tabbed panel showing static quality control plots produced by
#'   \pkg{gutenTAG}: intensity distribution, mean-variance, Geary's C, SNR,
#'   QC overview, and TIC spatial map.
#'
#' @param id Module namespace ID.
#'
#' @return A \code{\link[shiny]{tabsetPanel}} UI element.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(mod_qcUI("qc"))
#'   shiny::shinyApp(ui, function(input, output, session) {})
#' }
mod_qcUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabsetPanel(
    shiny::tabPanel("Intensity Distribution", shiny::plotOutput(ns("intensity_dist"))),
    shiny::tabPanel("Mean-Variance", shiny::plotOutput(ns("mean_var"))),
    shiny::tabPanel("Geary's C", shiny::plotOutput(ns("gearys_c"))),
    shiny::tabPanel("SNR", shiny::plotOutput(ns("snr"))),
    shiny::tabPanel("QC Overview", shiny::plotOutput(ns("qc_overview"))),
    shiny::tabPanel("TIC Spatial", shiny::plotOutput(ns("tic_spatial")))
  )
}

#' QC Dashboard Module Server
#'
#' @description
#'   Renders static quality control plots from \pkg{gutenTAG} into the
#'   tabbed QC dashboard.
#'
#' @param id Module namespace ID.
#' @param data A \code{\link[shiny]{reactive}} returning the list produced by
#'   \code{\link{mod_importServer}}.
#'
#' @return Invisibly \code{NULL}; called for its side-effect of rendering.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(mod_qcUI("qc"))
#'   server <- function(input, output, session) {
#'     data <- shiny::reactive(NULL)
#'     mod_qcServer("qc", data)
#'   }
#'   shiny::shinyApp(ui, server)
#' }
mod_qcServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {

    output$intensity_dist <- shiny::renderPlot({
      shiny::req(data())
      gutenTAG::plotIntensityDistribution(data()$processed)
    })

    output$mean_var <- shiny::renderPlot({
      shiny::req(data())
      gutenTAG::plotMeanVariance(data()$processed)
    })

    output$gearys_c <- shiny::renderPlot({
      shiny::req(data())
      gutenTAG::plotGearysC(data()$processed)
    })

    output$snr <- shiny::renderPlot({
      shiny::req(data())
      gutenTAG::plotSNR(data()$processed)
    })

    output$qc_overview <- shiny::renderPlot({
      shiny::req(data())
      gutenTAG::plotQCOverview(data()$processed)
    })

    output$tic_spatial <- shiny::renderPlot({
      shiny::req(data())
      gutenTAG::plotTICSpatial(data()$processed)
    })
  })
}
