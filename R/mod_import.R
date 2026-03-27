#' Data Import Module UI
#'
#' @description
#'   Sidebar UI for loading a MALDI-MSI dataset and a panel \code{.csv} file,
#'   then running the full gutenTAG processing pipeline. Paths are entered as
#'   text so that Cardinal reads directly from disk. Accordion panels expose
#'   tunable parameters for each pipeline step.
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

    bslib::accordion(
      open = FALSE,

      bslib::accordion_panel(
        "Peak Detection",
        shiny::numericInput(ns("pd_snr"), "SNR", value = 3, min = 0, step = 0.5),
        shiny::numericInput(ns("pd_win"), "Window size", value = 50, min = 1, step = 1)
      ),

      bslib::accordion_panel(
        "Generate Metapeaks",
        shiny::numericInput(ns("gm_threshold"), "Threshold", value = 0.01,
                            min = 0, max = 1, step = 0.005),
        shiny::numericInput(ns("gm_smooth"), "Hist smooth factor",
                            value = 1, min = 0.1, step = 0.1),
        shiny::numericInput(ns("gm_sparsity"), "Sparsity", value = 3,
                            min = 0, step = 0.5),
        shiny::numericInput(ns("gm_fixed_limits"), "Fixed limits (empty = auto)",
                            value = NA, min = 0, step = 0.1)
      ),

      bslib::accordion_panel(
        "Assign Metapeaks",
        shiny::numericInput(ns("am_mz_threshold"), "m/z threshold", value = 1,
                            min = 0, step = 0.1)
      )
    ),

    shiny::actionButton(ns("run"), "Process", class = "btn-primary w-100 mt-2"),
    shiny::uiOutput(ns("status"))
  )
}

#' Data Import Module Server
#'
#' @description
#'   Runs the gutenTAG processing pipeline using the parameters set in the UI
#'   and returns a reactive list of results.
#'
#' @param id Module namespace ID.
#'
#' @return A \code{\link[shiny]{reactive}} returning a named list with elements
#'   \code{processed}, \code{metapeaks}, and \code{panel}, or \code{NULL}
#'   before data are loaded.
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

      # resolve fixed.limits: NA → NULL
      fixed_lim <- input$gm_fixed_limits
      if (is.na(fixed_lim)) fixed_lim <- NULL

      tryCatch({
        panel <- gutenTAG::readPanel(panel_path)
        raw   <- Cardinal::readMSIData(imzml_path)
        pre   <- gutenTAG::preProcess(raw)

        peaks <- gutenTAG::peakDetection(pre,
                   snr = input$pd_snr,
                   win = input$pd_win)

        meta  <- gutenTAG::generateMetapeaks(peaks,
                   threshold         = input$gm_threshold,
                   hist_smooth_factor = input$gm_smooth,
                   sparsity          = input$gm_sparsity,
                   fixed.limits      = fixed_lim)

        proc  <- gutenTAG::assignMetapeaks(meta, pre, panel,
                   mz_threshold = input$am_mz_threshold)

        proc  <- gutenTAG::computeGearysC(proc, update_correspondence = TRUE)
        proc  <- gutenTAG::computeSNR(proc, update_correspondence = TRUE)

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
