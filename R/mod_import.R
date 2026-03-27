#' Data Import Module UI
#'
#' @description
#'   Sidebar UI for loading a MALDI-MSI dataset and a panel \code{.csv} file,
#'   then running the full gutenTAG processing pipeline. The imzML upload
#'   accepts both the \code{.imzML} metadata file and its paired \code{.ibd}
#'   binary file simultaneously (select both in the file chooser).
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
    shiny::fileInput(
      ns("imzml"), "Load .imzML + .ibd files",
      accept   = c(".imzML", ".ibd"),
      multiple = TRUE
    ),
    shiny::fileInput(ns("panel"), "Load panel .csv", accept = ".csv"),
    shiny::actionButton(ns("run"), "Process", class = "btn-primary w-100"),
    shiny::uiOutput(ns("status"))
  )
}

#' Data Import Module Server
#'
#' @description
#'   Runs the gutenTAG processing pipeline (read, preProcess, peakDetection,
#'   generateMetapeaks, assignMetapeaks) when the user clicks Process and
#'   returns a reactive list of results. Both \code{.imzML} and \code{.ibd}
#'   files are staged in a shared temp directory before reading so that
#'   \code{Cardinal::readMSIData} can locate the binary data file.
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

        # imzML and ibd must share a directory with matching basenames.
        # Shiny scatters uploaded files into separate temp paths, so copy
        # both into one directory under their original names first.
        msi_dir <- tempfile("shinytag_msi")
        dir.create(msi_dir)
        for (i in seq_len(nrow(input$imzml))) {
          file.copy(input$imzml$datapath[i],
                    file.path(msi_dir, input$imzml$name[i]))
        }
        imzml_path <- file.path(
          msi_dir,
          input$imzml$name[grepl("\\.imzML$", input$imzml$name, ignore.case = TRUE)]
        )

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
