#' Data Import Module UI
#'
#' @description
#'   Sidebar UI for loading a MALDI-MSI \code{.imzML} file and a panel
#'   \code{.csv} file, then running the full gutenTAG processing pipeline.
#'
#' @param id Module namespace ID.
#'
#' @return A \code{\link[shiny]{tagList}} of UI elements.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(mod_importUI("import"))
#'   shiny::shinyApp(ui, function(input, output, session) {})
#' }
mod_importUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fileInput(ns("imzml"), "Load .imzML file", accept = ".imzML"),
    shiny::fileInput(ns("panel"), "Load panel .csv",  accept = ".csv"),
    shiny::actionButton(ns("run"), "Process", class = "btn-primary w-100"),
    shiny::uiOutput(ns("status"))
  )
}

#' Data Import Module Server
#'
#' @description
#'   Runs the gutenTAG processing pipeline (read, preProcess, peakDetection,
#'   generateMetapeaks, assignMetapeaks) when the user clicks Process and
#'   returns a reactive list of results.
#'
#' @param id Module namespace ID.
#'
#' @return A \code{\link[shiny]{reactive}} returning a named list with elements
#'   \code{processed} (output of \code{assignMetapeaks}), \code{metapeaks}
#'   (output of \code{generateMetapeaks}), and \code{panel} (the loaded panel
#'   data frame), or \code{NULL} before data are loaded.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(mod_importUI("import"))
#'   server <- function(input, output, session) {
#'     mod_importServer("import")
#'   }
#'   shiny::shinyApp(ui, server)
#' }
mod_importServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    results <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$run, {
      shiny::req(input$imzml, input$panel)

      output$status <- shiny::renderUI(
        shiny::tags$p("Processing \u2014 please wait...", class = "text-warning small mt-2")
      )

      tryCatch({
        panel <- gutenTAG::readPanel(input$panel$datapath)
        raw   <- Cardinal::readMSIData(input$imzml$datapath)
        pre   <- gutenTAG::preProcess(raw)
        peaks <- gutenTAG::peakDetection(pre)
        meta  <- gutenTAG::generateMetapeaks(peaks)
        proc  <- gutenTAG::assignMetapeaks(meta, pre, panel)

        results(list(processed = proc, metapeaks = meta, panel = panel))

        output$status <- shiny::renderUI(
          shiny::tags$p("Ready.", class = "text-success small mt-2")
        )
      }, error = function(e) {
        output$status <- shiny::renderUI(
          shiny::tags$p(
            paste("Error:", conditionMessage(e)),
            class = "text-danger small mt-2"
          )
        )
      })
    })

    shiny::reactive(results())
  })
}
