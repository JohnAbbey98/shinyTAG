#' Data Import Module UI
#'
#' @description
#'   Sidebar UI for loading a MALDI-MSI dataset and a panel \code{.csv} file,
#'   then running the full gutenTAG processing pipeline. Paths are entered as
#'   text so that Cardinal reads directly from disk — this avoids copying large
#'   files and ensures the \code{.ibd} companion file is found automatically.
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
    shiny::textInput(ns("imzml"), "Path to .imzML file",
                     placeholder = "/path/to/data.imzML"),
    shiny::textInput(ns("panel"), "Path to panel .csv",
                     placeholder = "/path/to/panel.csv"),
    shiny::actionButton(ns("run"), "Process", class = "btn-primary w-100"),
    shiny::uiOutput(ns("status"))
  )
}

#' Data Import Module Server
#'
#' @description
#'   Runs the gutenTAG processing pipeline (read, preProcess, peakDetection,
#'   generateMetapeaks, assignMetapeaks) when the user clicks Process and
#'   returns a reactive list of results. Files are read directly from the
#'   supplied paths so that Cardinal can locate the paired \code{.ibd} file.
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

      imzml_path <- normalizePath(input$imzml, mustWork = FALSE)
      panel_path <- normalizePath(input$panel, mustWork = FALSE)

      if (!file.exists(imzml_path)) {
        output$status <- shiny::renderUI(
          shiny::tags$p("imzML file not found.", class = "text-danger small mt-2")
        )
        return()
      }
      if (!file.exists(panel_path)) {
        output$status <- shiny::renderUI(
          shiny::tags$p("Panel CSV not found.", class = "text-danger small mt-2")
        )
        return()
      }

      output$status <- shiny::renderUI(
        shiny::tags$p("Processing \u2014 please wait...",
                      class = "text-warning small mt-2")
      )

      tryCatch({
        panel <- gutenTAG::readPanel(panel_path)
        raw   <- Cardinal::readMSIData(imzml_path)
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
