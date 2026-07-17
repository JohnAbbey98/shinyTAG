#' Preview Mode Module UI
#'
#' @description
#'   Interactive parameter exploration UI. Loads a downsampled subset of raw
#'   \code{.imzML} data and walks the user through the six gutenTAG pipeline
#'   steps (window, SNR, smoothing, sparsity, limits, association) one at a
#'   time, each with a reactive plot and parameter slider. An "Accept
#'   parameters" button propagates the chosen values to the Processing Mode
#'   inputs.
#'
#' @param id Module namespace ID.
#'
#' @return A \code{\link[shiny]{tagList}} of UI elements.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(mod_previewUI("preview"))
#'   shiny::shinyApp(ui, function(input, output, session) {})
#' }
mod_previewUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4, shiny::textInput(ns("imzml"), "Path to .imzML file",
                                        placeholder = "/path/to/data.imzML")),
      shiny::column(4, shiny::textInput(ns("panel"), "Path to panel .csv",
                                        placeholder = "/path/to/panel.csv")),
      shiny::column(2, shiny::sliderInput(ns("downsample"), "Downsample %",
                                          min = 1, max = 100, value = 10,
                                          step = 1)),
      shiny::column(2, shiny::actionButton(ns("load"), "Load",
                                           class = "btn-primary w-100",
                                           style = "margin-top: 32px;"))
    ),
    shiny::uiOutput(ns("status")),

    bslib::navset_card_tab(
      id = ns("step"),

      bslib::nav_panel(
        "1. Window",
        shiny::fluidRow(
          shiny::column(6, plotly::plotlyOutput(ns("spec1"), height = "300px")),
          shiny::column(6, plotly::plotlyOutput(ns("hist1"), height = "300px"))
        ),
        shiny::sliderInput(ns("win"), "Window size (noise estimation)",
                           min = 5, max = 200, value = 50, step = 1,
                           width = "100%")
      ),

      bslib::nav_panel(
        "2. SNR",
        shiny::fluidRow(
          shiny::column(6, plotly::plotlyOutput(ns("spec2"), height = "300px")),
          shiny::column(6, plotly::plotlyOutput(ns("hist2"), height = "300px"))
        ),
        shiny::sliderInput(ns("snr"), "Signal-to-noise ratio",
                           min = 0.5, max = 10, value = 3, step = 0.1,
                           width = "100%")
      ),

      bslib::nav_panel(
        "3. Smoothing",
        plotly::plotlyOutput(ns("hist3"), height = "350px"),
        shiny::sliderInput(ns("smooth"), "Histogram smoothing factor",
                           min = 0.1, max = 5, value = 1, step = 0.1,
                           width = "100%")
      ),

      bslib::nav_panel(
        "4. Sparsity",
        plotly::plotlyOutput(ns("hist4"), height = "350px"),
        shiny::fluidRow(
          shiny::column(8, shiny::sliderInput(ns("sparsity"), "Sparsity",
                                              min = 0.5, max = 10, value = 3,
                                              step = 0.1, width = "100%")),
          shiny::column(4, shiny::sliderInput(ns("threshold"),
                                              "Detection threshold",
                                              min = 0.001, max = 0.5,
                                              value = 0.01, step = 0.001,
                                              width = "100%"))
        )
      ),

      bslib::nav_panel(
        "5. Limits",
        shiny::fluidRow(
          shiny::column(7, plotly::plotlyOutput(ns("mp5"), height = "320px")),
          shiny::column(5, shiny::plotOutput(ns("img5"), height = "320px"))
        ),
        shiny::fluidRow(
          shiny::column(8, shiny::sliderInput(ns("limits"),
                                              "Fixed limits (m/z half-width)",
                                              min = 0.05, max = 2, value = 0.5,
                                              step = 0.05, width = "100%")),
          shiny::column(4, shiny::selectInput(ns("ch5"), "Preview channel",
                                              choices = NULL, width = "100%"))
        )
      ),

      bslib::nav_panel(
        "6. Association",
        shiny::fluidRow(
          shiny::column(7, plotly::plotlyOutput(ns("mp6"), height = "320px")),
          shiny::column(5, shiny::plotOutput(ns("img6"), height = "320px"))
        ),
        shiny::fluidRow(
          shiny::column(8, shiny::sliderInput(ns("assoc"),
                                              "Association m/z threshold",
                                              min = 0.05, max = 5, value = 1,
                                              step = 0.05, width = "100%")),
          shiny::column(4, shiny::selectInput(ns("ch6"), "Preview channel",
                                              choices = NULL, width = "100%"))
        )
      )
    ),

    shiny::actionButton(ns("accept"), "Accept parameters",
                        class = "btn-success w-100 mt-3")
  )
}

#' Preview Mode Module Server
#'
#' @description
#'   Reactive parameter exploration server. Loads a random pixel subset from
#'   a raw \code{.imzML} file on demand, pre-processes it, and drives six
#'   sequential reactive steps whose outputs feed forward. Emits accepted
#'   parameter values when the user clicks "Accept parameters".
#'
#' @param id Module namespace ID.
#'
#' @return A \code{\link[shiny]{reactive}} returning a named list of accepted
#'   pipeline parameters (\code{pd_win}, \code{pd_snr}, \code{gm_smooth},
#'   \code{gm_sparsity}, \code{gm_threshold}, \code{gm_fixed_limits},
#'   \code{am_mz_threshold}), emitted as an event when Accept is clicked.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ui <- shiny::fluidPage(mod_previewUI("preview"))
#'   server <- function(input, output, session) mod_previewServer("preview")
#'   shiny::shinyApp(ui, server)
#' }
mod_previewServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    preview_data <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$load, {
      output$status <- shiny::renderUI(NULL)
      .preview_load(input, output, preview_data)
    })

    # Reactive chain ────────────────────────────────────────────────────────
    win_r      <- shiny::debounce(shiny::reactive(input$win),      400)
    snr_r      <- shiny::debounce(shiny::reactive(input$snr),      400)
    smooth_r   <- shiny::debounce(shiny::reactive(input$smooth),   400)
    sparsity_r <- shiny::debounce(shiny::reactive(input$sparsity), 400)
    thresh_r   <- shiny::debounce(shiny::reactive(input$threshold),400)
    limits_r   <- shiny::debounce(shiny::reactive(input$limits),   400)
    assoc_r    <- shiny::debounce(shiny::reactive(input$assoc),    400)

    mean_spec <- shiny::reactive({
      pd <- preview_data(); shiny::req(pd)
      pd$mean_spec
    })

    noise_line <- shiny::reactive({
      ms <- mean_spec()
      w  <- win_r(); shiny::req(w)
      .rolling_mad(ms$mean, w)
    })

    peaks_r <- shiny::reactive({
      pd <- preview_data(); shiny::req(pd)
      w <- win_r(); s <- snr_r(); shiny::req(w, s)
      shiny::withProgress(message = "Peak detection", value = 0.5, {
        gutenTAG::peakDetection(pd$pre, snr = s, win = w)
      })
    })

    count_df_r <- shiny::reactive({
      gutenTAG::countPeaks(peaks_r())
    })

    smooth_df_r <- shiny::reactive({
      gutenTAG::smoothPeakCounts(count_df_r(), smooth_r())
    })

    detection_threshold_r <- shiny::reactive({
      pd <- preview_data(); shiny::req(pd)
      length(Cardinal::pixels(pd$pre)) * thresh_r()
    })

    seed_mz_r <- shiny::reactive({
      gutenTAG::generateSeedMz(smooth_df_r(),
                               detection_threshold = detection_threshold_r(),
                               sparsity = sparsity_r())
    })

    meta_r <- shiny::reactive({
      gutenTAG::estimateMetapeaks(
        count_df = count_df_r(),
        smooth_count_df = smooth_df_r(),
        seed_mz = seed_mz_r(),
        detection_threshold = detection_threshold_r(),
        fixed.limits = limits_r()
      )
    })

    assigned_r <- shiny::reactive({
      pd <- preview_data(); shiny::req(pd)
      shiny::withProgress(message = "Assigning metapeaks", value = 0.5, {
        gutenTAG::assignMetapeaks(meta_r(), pd$pre, pd$panel,
                                  mz_threshold = assoc_r())
      })
    })

    # Populate channel selectors for preview steps 5 & 6 ──────────────────
    shiny::observeEvent(meta_r(), {
      mp <- meta_r()$metapeaks
      choices <- paste0(round(mp$max, 3), " m/z")
      shiny::updateSelectInput(session, "ch5", choices = choices,
                               selected = choices[1])
    })
    shiny::observeEvent(assigned_r(), {
      proc <- assigned_r()
      targeted <- colnames(proc$IntensityDF)
      all_corr <- proc$AllMetapeaks$AllMetapeaksCorrespondence
      all_int  <- as.data.frame(proc$AllMetapeaks$AllMetapeaksIntensity)
      untargeted <- colnames(all_int)[is.na(all_corr$marker)]
      shiny::updateSelectInput(session, "ch6",
                               choices = list(Targeted = targeted,
                                              Untargeted = untargeted),
                               selected = targeted[1])
    })

    # Step 1 plots ─────────────────────────────────────────────────────────
    output$spec1 <- plotly::renderPlotly({
      ms <- mean_spec(); nl <- noise_line()
      .plot_spec_noise(ms, nl, snr_line = NULL, title = "Mean spectrum + MAD noise")
    })
    output$hist1 <- plotly::renderPlotly({
      .plot_counts_hist(count_df_r(), title = "Counts histogram")
    })

    # Step 2 plots ─────────────────────────────────────────────────────────
    output$spec2 <- plotly::renderPlotly({
      ms <- mean_spec(); nl <- noise_line(); s <- snr_r()
      .plot_spec_noise(ms, nl, snr_line = s,
                       title = sprintf("Mean spectrum + %sx noise", s))
    })
    output$hist2 <- plotly::renderPlotly({
      .plot_counts_hist(count_df_r(), title = "Counts histogram (SNR-filtered)")
    })

    # Step 3 plot ──────────────────────────────────────────────────────────
    output$hist3 <- plotly::renderPlotly({
      .plot_counts_smoothed(count_df_r(), smooth_df_r())
    })

    # Step 4 plot ──────────────────────────────────────────────────────────
    output$hist4 <- plotly::renderPlotly({
      .plot_counts_seeds(count_df_r(), smooth_df_r(), seed_mz_r(),
                        detection_threshold_r())
    })

    # Step 5 plot + image ──────────────────────────────────────────────────
    output$mp5 <- plotly::renderPlotly({
      .plot_metapeaks_preview(count_df_r(), meta_r(),
                              title = "Metapeaks with limits") %>%
        htmlwidgets::onRender(.mz_click_js(session$ns("click_mp5")))
    })

    shiny::observeEvent(input$click_mp5, {
      shiny::req(meta_r())
      mp <- meta_r()$metapeaks
      hit_idx <- .find_metapeak_hit(input$click_mp5$x, mp$limits)
      if (!length(hit_idx)) return()
      shiny::updateSelectInput(session, "ch5",
        selected = paste0(round(mp$max[hit_idx], 3), " m/z"))
    })

    output$img5 <- shiny::renderPlot({
      pr <- assigned_r(); shiny::req(pr, input$ch5)
      mp <- meta_r()$metapeaks
      idx <- which(paste0(round(mp$max, 3), " m/z") == input$ch5)
      if (!length(idx)) return(NULL)
      all_int <- as.data.frame(pr$AllMetapeaks$AllMetapeaksIntensity)
      if (idx > ncol(all_int)) return(NULL)
      gutenTAG::imageChannel(all_int, coords = pr$SpatialCoords, channel = idx)
    })

    # Step 6 plot + image ──────────────────────────────────────────────────
    output$mp6 <- plotly::renderPlotly({
      pr <- preview_data(); shiny::req(pr)
      .plot_metapeaks_assigned(count_df_r(), meta_r(), assigned_r(), pr$panel) %>%
        htmlwidgets::onRender(.mz_click_js(session$ns("click_mp6")))
    })

    shiny::observeEvent(input$click_mp6, {
      shiny::req(meta_r(), assigned_r())
      clicked_mz <- input$click_mp6$x
      mp <- meta_r()$metapeaks
      hit_idx <- .find_metapeak_hit(clicked_mz, mp$limits)
      if (!length(hit_idx)) return()
      hit_mz <- mp$max[hit_idx]
      pr <- assigned_r()
      all_corr <- pr$AllMetapeaks$AllMetapeaksCorrespondence
      match_row <- which(all_corr$mz_location == hit_mz)
      if (length(match_row) > 0 && !is.na(all_corr$marker[match_row[1]])) {
        shiny::updateSelectInput(session, "ch6",
                                 selected = all_corr$marker[match_row[1]])
      } else {
        shiny::updateSelectInput(session, "ch6",
          selected = paste0(round(hit_mz, 2), " m/z"))
      }
    })

    output$img6 <- shiny::renderPlot({
      pr <- assigned_r(); shiny::req(pr, input$ch6)
      targeted <- colnames(pr$IntensityDF)
      if (input$ch6 %in% targeted) {
        gutenTAG::imageChannel(pr, channel = input$ch6)
      } else {
        all_int <- as.data.frame(pr$AllMetapeaks$AllMetapeaksIntensity)
        gutenTAG::imageChannel(all_int, coords = pr$SpatialCoords,
                               channel = input$ch6)
      }
    })

    # Accept parameters ─────────────────────────────────────────────────────
    shiny::eventReactive(input$accept, {
      list(
        pd_win          = input$win,
        pd_snr          = input$snr,
        gm_smooth       = input$smooth,
        gm_sparsity    = input$sparsity,
        gm_threshold    = input$threshold,
        gm_fixed_limits = input$limits,
        am_mz_threshold = input$assoc
      )
    })
  })
}

# ── Internal: load + downsample ───────────────────────────────────────────────

.preview_load <- function(input, output, preview_data) {
  shiny::req(input$imzml, input$panel)

  imzml_path <- normalizePath(input$imzml, mustWork = FALSE)
  panel_path <- normalizePath(input$panel, mustWork = FALSE)

  if (!file.exists(imzml_path)) {
    output$status <- shiny::renderUI(
      shiny::tags$p("imzML file not found.", class = "text-danger small mt-2"))
    return()
  }
  if (!file.exists(panel_path)) {
    output$status <- shiny::renderUI(
      shiny::tags$p("Panel CSV not found.", class = "text-danger small mt-2"))
    return()
  }

  tryCatch({
    shiny::withProgress(message = "Loading preview data", value = 0, {
      shiny::incProgress(0.2, detail = "Reading panel")
      panel <- gutenTAG::readPanel(panel_path)

      shiny::incProgress(0.3, detail = "Reading imzML")
      raw <- Cardinal::readMSIData(imzml_path)

      n_pix <- length(Cardinal::pixels(raw))
      keep  <- max(1, round(n_pix * input$downsample / 100))
      idx   <- sort(sample.int(n_pix, keep))
      raw_sub <- raw[, idx]

      shiny::incProgress(0.3, detail = sprintf("Pre-processing %d pixels", keep))
      pre <- gutenTAG::preProcess(raw_sub)

      shiny::incProgress(0.2, detail = "Mean spectrum")
      ss <- Cardinal::summarizeFeatures(pre, stat = c(mean = "mean"))
      fd <- matter::as.data.frame(Cardinal::featureData(ss))
      mean_spec <- data.frame(mz = fd$mz, mean = fd$mean)
    })

    preview_data(list(pre = pre, panel = panel, mean_spec = mean_spec,
                      n_pixels = keep))
    output$status <- shiny::renderUI(
      shiny::tags$p(sprintf("Loaded %d pixels (%.0f%% of %d).",
                            keep, 100 * keep / n_pix, n_pix),
                    class = "text-success small mt-2")
    )
  }, error = function(e) {
    output$status <- shiny::renderUI(
      shiny::tags$p(paste("Error:", conditionMessage(e)),
                    class = "text-danger small mt-2"))
  })
}

# ── Internal: rolling MAD (noise estimator over mean spectrum) ────────────────

.rolling_mad <- function(y, window) {
  n <- length(y)
  half <- as.integer(window) %/% 2L
  out <- numeric(n)
  for (i in seq_len(n)) {
    lo <- max(1L, i - half)
    hi <- min(n,  i + half)
    out[i] <- stats::mad(y[lo:hi], na.rm = TRUE)
  }
  data.frame(mz = seq_len(n), noise = out)
}

# ── Internal: click helpers (mirrors mod_spectra.R JS handler) ───────────────

.mz_click_js <- function(input_id) {
  sprintf("
    function(el) {
      el.addEventListener('click', function(evt) {
        var layout = el._fullLayout;
        if (!layout || !layout.xaxis) return;
        var bb    = el.getBoundingClientRect();
        var xPx   = evt.clientX - bb.left - layout.margin.l;
        var yPx   = evt.clientY - bb.top  - layout.margin.t;
        if (xPx < 0 || xPx > layout.xaxis._length) return;
        if (yPx < 0 || yPx > layout.yaxis._length) return;
        var xData = layout.xaxis.p2d(xPx);
        Shiny.setInputValue('%s',
          {x: xData, nonce: Math.random()},
          {priority: 'event'});
      });
    }
  ", input_id)
}

.find_metapeak_hit <- function(clicked_mz, limits) {
  which(clicked_mz >= limits[, 1] & clicked_mz <= limits[, 2])[1]
}

# ── Internal: plot helpers (consistent palette with plotMetapeaks) ────────────

.plot_spec_noise <- function(mean_spec, noise_line, snr_line = NULL,
                             title = "Mean spectrum") {
  nl <- data.frame(mz = mean_spec$mz, noise = noise_line$noise)
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = mean_spec,
                       ggplot2::aes(x = .data[["mz"]], y = .data[["mean"]]),
                       color = "black", linewidth = 0.4) +
    ggplot2::geom_line(data = nl,
                       ggplot2::aes(x = .data[["mz"]], y = .data[["noise"]]),
                       color = "red", linewidth = 0.6) +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(title) +
    ggplot2::labs(x = "m/z", y = "Intensity")
  if (!is.null(snr_line)) {
    nl2 <- data.frame(mz = mean_spec$mz, thr = noise_line$noise * snr_line)
    p <- p + ggplot2::geom_line(data = nl2,
                                ggplot2::aes(x = .data[["mz"]],
                                             y = .data[["thr"]]),
                                color = "springgreen3", linewidth = 0.6)
  }
  plotly::ggplotly(p, dynamicTicks = TRUE)
}

.plot_counts_hist <- function(count_df, title = "Counts") {
  p <- ggplot2::ggplot(count_df,
                       ggplot2::aes(x = .data[["mz"]],
                                    y = .data[["count"]])) +
    ggplot2::geom_line(color = "black", linewidth = 0.4) +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(title) +
    ggplot2::labs(x = "m/z", y = "Peak count")
  plotly::ggplotly(p, dynamicTicks = TRUE)
}

.plot_counts_smoothed <- function(count_df, smooth_df) {
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = count_df,
                       ggplot2::aes(x = .data[["mz"]],
                                    y = .data[["count"]]),
                       color = "black", linewidth = 0.3, alpha = 0.7) +
    ggplot2::geom_line(data = smooth_df,
                       ggplot2::aes(x = .data[["mz"]],
                                    y = .data[["count"]]),
                       color = "sienna", linewidth = 0.8) +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle("Smoothed peak counts") +
    ggplot2::labs(x = "m/z", y = "Peak count")
  plotly::ggplotly(p, dynamicTicks = TRUE)
}

.plot_counts_seeds <- function(count_df, smooth_df, seed_mz,
                               detection_threshold = NULL) {
  seed_counts <- vapply(seed_mz, function(s) {
    smooth_df$count[which.min(abs(smooth_df$mz - s))]
  }, numeric(1))
  seed_df <- data.frame(mz = seed_mz, y = seed_counts)
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = count_df,
                       ggplot2::aes(x = .data[["mz"]],
                                    y = .data[["count"]]),
                       color = "black", linewidth = 0.3, alpha = 0.6) +
    ggplot2::geom_line(data = smooth_df,
                       ggplot2::aes(x = .data[["mz"]],
                                    y = .data[["count"]]),
                       color = "sienna", linewidth = 0.6) +
    ggplot2::geom_point(data = seed_df,
                        ggplot2::aes(x = .data[["mz"]], y = .data[["y"]]),
                        shape = 21, color = "turquoise4", fill = "turquoise",
                        alpha = 0.6, size = 4, stroke = 1) +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(sprintf("%d seed metapeaks", length(seed_mz))) +
    ggplot2::labs(x = "m/z", y = "Peak count")
  if (!is.null(detection_threshold)) {
    p <- p + ggplot2::geom_hline(yintercept = detection_threshold,
                                  color = "black", linewidth = 0.5,
                                  linetype = "dashed")
  }
  plotly::ggplotly(p, dynamicTicks = TRUE)
}

.plot_metapeaks_preview <- function(count_df, meta, title = "Metapeaks") {
  mp <- meta$metapeaks
  ymax <- max(count_df$count, na.rm = TRUE)
  mp_df <- data.frame(lower = mp$limits[, 1], upper = mp$limits[, 2],
                      max = mp$max)
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = mp_df,
                       ggplot2::aes(xmin = .data[["lower"]],
                                    xmax = .data[["upper"]],
                                    ymin = 0, ymax = ymax),
                       fill = "grey", alpha = 0.3) +
    ggplot2::geom_line(data = count_df,
                       ggplot2::aes(x = .data[["mz"]],
                                    y = .data[["count"]]),
                       color = "black", linewidth = 0.4) +
    ggplot2::geom_vline(data = mp_df,
                        ggplot2::aes(xintercept = .data[["max"]]),
                        color = "springgreen3", linewidth = 0.5,
                        linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(title) +
    ggplot2::labs(x = "m/z", y = "Peak count")
  plotly::ggplotly(p, dynamicTicks = TRUE)
}

.plot_metapeaks_assigned <- function(count_df, meta, assigned, panel) {
  mp <- meta$metapeaks
  ymax <- max(count_df$count, na.rm = TRUE)
  targeted_mz <- assigned$CorrespondenceMatrix$mz_location
  mp_df <- data.frame(lower = mp$limits[, 1], upper = mp$limits[, 2],
                      max = mp$max,
                      assigned = mp$max %in% targeted_mz)
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = mp_df,
                       ggplot2::aes(xmin = .data[["lower"]],
                                    xmax = .data[["upper"]],
                                    ymin = 0, ymax = ymax,
                                    fill = .data[["assigned"]]),
                       alpha = 0.3) +
    ggplot2::geom_line(data = count_df,
                       ggplot2::aes(x = .data[["mz"]],
                                    y = .data[["count"]]),
                       color = "black", linewidth = 0.4) +
    ggplot2::geom_vline(data = mp_df,
                        ggplot2::aes(xintercept = .data[["max"]]),
                        color = "springgreen3", linewidth = 0.5,
                        linetype = "dashed") +
    ggplot2::geom_vline(data = panel,
                        ggplot2::aes(xintercept = .data[["FeatureMass"]]),
                        color = "purple", linewidth = 0.7) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "red3", `FALSE` = "grey"),
                               name = "Assigned") +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle(sprintf("%d assigned / %d total",
                             sum(mp_df$assigned), nrow(mp_df))) +
    ggplot2::labs(x = "m/z", y = "Peak count")
  plotly::ggplotly(p, dynamicTicks = TRUE)
}
