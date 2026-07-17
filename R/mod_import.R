#' Data Import Module UI
#'
#' @description
#'   Sidebar UI for loading MALDI-MSI data via two modes: (1) raw
#'   \code{.imzML} + panel \code{.csv} with full pipeline processing, or
#'   (2) a pre-processed \code{.Rdata} file that skips the pipeline entirely.
#'   Accordion panels expose tunable parameters for each pipeline step.
#'   A dataset summary is shown after successful loading.
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
    shiny::radioButtons(ns("app_mode"), NULL,
                        choices  = c("Processing" = "processing",
                                     "Preview"    = "preview"),
                        selected = "processing", inline = TRUE),
    shiny::tags$hr(style = "margin: 6px 0;"),
    shiny::radioButtons(ns("mode"), NULL,
                        choices  = c("Raw (.imzML)" = "raw",
                                     "Processed (.Rdata)" = "rdata"),
                        selected = "raw", inline = TRUE),

    # ── Raw mode inputs ───────────────────────────────────────────────────
    shiny::conditionalPanel(
      sprintf("input['%s'] === 'raw'", ns("mode")),
      shiny::textInput(ns("imzml"), "Path to .imzML file",
                       placeholder = "/path/to/data.imzML"),
      shiny::textInput(ns("panel"), "Path to panel .csv",
                       placeholder = "/path/to/panel.csv"),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Peak Detection",
          shiny::numericInput(ns("pd_snr"), "SNR", value = 3,
                              min = 0, step = 0.5),
          shiny::numericInput(ns("pd_win"), "Window size", value = 50,
                              min = 1, step = 1)
        ),
        bslib::accordion_panel(
          "Generate Metapeaks",
          shiny::numericInput(ns("gm_threshold"), "Threshold", value = 0.01,
                              min = 0, max = 1, step = 0.005),
          shiny::numericInput(ns("gm_smooth"), "Hist smooth factor",
                              value = 1, min = 0.1, step = 0.1),
          shiny::numericInput(ns("gm_sparsity"), "Sparsity", value = 3,
                              min = 0, step = 0.5),
          shiny::numericInput(ns("gm_fixed_limits"),
                              "Fixed limits (empty = auto)",
                              value = NA, min = 0, step = 0.1)
        ),
        bslib::accordion_panel(
          "Assign Metapeaks",
          shiny::numericInput(ns("am_mz_threshold"), "m/z threshold",
                              value = 1, min = 0, step = 0.1)
        )
      )
    ),

    # ── Rdata mode input ──────────────────────────────────────────────────
    shiny::conditionalPanel(
      sprintf("input['%s'] === 'rdata'", ns("mode")),
      shiny::textInput(ns("rdata"), "Path to .Rdata file",
                       placeholder = "/path/to/processed.Rdata"),
      shiny::checkboxInput(ns("run_geary"), "Compute Geary's C", value = FALSE),
      shiny::checkboxInput(ns("run_snr"), "Compute SNR", value = FALSE)
    ),

    shiny::actionButton(ns("run"), "Process", class = "btn-primary w-100 mt-2"),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("summary"))
  )
}

#' Data Import Module Server
#'
#' @description
#'   Loads data in raw or pre-processed mode, with a progress bar tracking
#'   each pipeline step. Returns a reactive list of results and renders a
#'   dataset summary after loading.
#'
#' @param id Module namespace ID.
#' @param preview_params A \code{\link[shiny]{reactive}} returning a named list
#'   of parameter values accepted from Preview Mode (optional). When it emits
#'   a non-\code{NULL} value, the corresponding Processing Mode inputs are
#'   updated and the app switches back to Processing Mode.
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
mod_importServer <- function(id, preview_params = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {

    results <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$run, {

      output$status <- shiny::renderUI(NULL)
      output$summary <- shiny::renderUI(NULL)

      if (input$mode == "rdata") {
        .load_rdata(input, output, results)
      } else {
        .load_raw(input, output, session, results)
      }
    })

    shiny::observeEvent(preview_params(), {
      p <- preview_params(); shiny::req(p)
      shiny::updateNumericInput(session, "pd_snr",          value = p$pd_snr)
      shiny::updateNumericInput(session, "pd_win",          value = p$pd_win)
      shiny::updateNumericInput(session, "gm_threshold",    value = p$gm_threshold)
      shiny::updateNumericInput(session, "gm_smooth",       value = p$gm_smooth)
      shiny::updateNumericInput(session, "gm_sparsity",     value = p$gm_sparsity)
      shiny::updateNumericInput(session, "gm_fixed_limits", value = p$gm_fixed_limits)
      shiny::updateNumericInput(session, "am_mz_threshold", value = p$am_mz_threshold)
      shiny::updateRadioButtons(session, "app_mode", selected = "processing")
    })

    shiny::reactive(results())
  })
}

# ── Internal: load pre-processed .Rdata ───────────────────────────────────────

.load_rdata <- function(input, output, results) {
  shiny::req(input$rdata)
  rdata_path <- normalizePath(input$rdata, mustWork = FALSE)

  if (!file.exists(rdata_path)) {
    output$status <- shiny::renderUI(
      shiny::tags$p("Rdata file not found.", class = "text-danger small mt-2")
    )
    return()
  }

  tryCatch({
    env <- new.env(parent = emptyenv())
    load(rdata_path, envir = env)
    obj <- ls(env)

    # expect a single list named 'results' with processed/metapeaks/panel
    res <- env[[obj[1]]]
    stopifnot(all(c("processed", "metapeaks", "panel") %in% names(res)))

    if (isTRUE(input$run_geary)) {
      res$processed <- gutenTAG::computeGearysC(res$processed,
                                                 update_correspondence = TRUE)
    }
    if (isTRUE(input$run_snr)) {
      res$processed <- gutenTAG::computeSNR(res$processed,
                                            update_correspondence = TRUE)
    }

    results(res)
    output$status <- shiny::renderUI(
      shiny::tags$p("Loaded.", class = "text-success small mt-2")
    )
    .render_summary(output, res)
  }, error = function(e) {
    output$status <- shiny::renderUI(
      shiny::tags$p(paste("Error:", conditionMessage(e)),
                    class = "text-danger small mt-2")
    )
  })
}

# ── Internal: run full pipeline on raw data ───────────────────────────────────

.load_raw <- function(input, output, session, results) {
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

  fixed_lim <- input$gm_fixed_limits
  if (is.na(fixed_lim)) fixed_lim <- NULL

  n_steps <- 7L

  tryCatch({
    shiny::withProgress(message = "Processing", value = 0, {

      shiny::incProgress(1 / n_steps, detail = "Reading panel")
      panel <- gutenTAG::readPanel(panel_path)

      shiny::incProgress(1 / n_steps, detail = "Reading imzML")
      raw <- Cardinal::readMSIData(imzml_path)

      shiny::incProgress(1 / n_steps, detail = "Pre-processing")
      pre <- gutenTAG::preProcess(raw)

      shiny::incProgress(1 / n_steps, detail = "Peak detection")
      peaks <- gutenTAG::peakDetection(pre,
                 snr = input$pd_snr, win = input$pd_win)

      shiny::incProgress(1 / n_steps, detail = "Generating metapeaks")
      meta <- gutenTAG::generateMetapeaks(peaks,
                threshold = input$gm_threshold,
                hist_smooth_factor = input$gm_smooth,
                sparsity = input$gm_sparsity,
                fixed.limits = fixed_lim)

      shiny::incProgress(1 / n_steps, detail = "Assigning metapeaks")
      proc <- gutenTAG::assignMetapeaks(meta, pre, panel,
                mz_threshold = input$am_mz_threshold)

      shiny::incProgress(1 / n_steps, detail = "Computing QC stats")
      proc <- gutenTAG::computeGearysC(proc, update_correspondence = TRUE)
      proc <- gutenTAG::computeSNR(proc, update_correspondence = TRUE)
    })

    res <- list(processed = proc, metapeaks = meta, panel = panel)
    results(res)

    output$status <- shiny::renderUI(
      shiny::tags$p("Ready.", class = "text-success small mt-2")
    )
    .render_summary(output, res)

  }, error = function(e) {
    output$status <- shiny::renderUI(
      shiny::tags$p(paste("Error:", conditionMessage(e)),
                    class = "text-danger small mt-2")
    )
  })
}

# ── Internal: dataset summary ─────────────────────────────────────────────────

.render_summary <- function(output, res) {
  n_pixels <- nrow(res$processed$IntensityDF)
  n_markers <- ncol(res$processed$IntensityDF)
  n_metapeaks <- ncol(res$processed$AllMetapeaks$AllMetapeaksIntensity)
  mz_range <- range(res$processed$CorrespondenceMatrix$mz_location,
                     na.rm = TRUE)

  output$summary <- shiny::renderUI(
    shiny::tags$div(
      class = "small mt-2",
      shiny::tags$strong("Dataset"),
      shiny::tags$ul(
        class = "mb-0 ps-3",
        shiny::tags$li(paste0(n_pixels, " pixels")),
        shiny::tags$li(paste0(n_markers, " markers")),
        shiny::tags$li(paste0(n_metapeaks, " total metapeaks")),
        shiny::tags$li(sprintf("m/z %.1f \u2013 %.1f", mz_range[1], mz_range[2]))
      )
    )
  )
}
