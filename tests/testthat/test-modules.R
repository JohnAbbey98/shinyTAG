test_that("mod_importUI returns a tagList", {
  ui <- mod_importUI("test")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("mod_importUI contains both mode inputs", {
  ui <- mod_importUI("test")
  html <- as.character(ui)
  expect_true(grepl("raw", html))
  expect_true(grepl("rdata", html))
})

test_that("mod_spectraUI returns a shiny tag list", {
  ui <- mod_spectraUI("test")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("mod_imageUI returns a tagList", {
  ui <- mod_imageUI("test")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("mod_imageUI contains channel and palette selectors", {
  ui <- mod_imageUI("test")
  html <- as.character(ui)
  expect_true(grepl("channel", html))
  expect_true(grepl("palette", html))
})

test_that("mod_qcUI returns a tabset panel", {
  ui <- mod_qcUI("test")
  expect_s3_class(ui, "shiny.tag")
})

test_that("mod_qcUI has all six QC tabs", {
  ui <- mod_qcUI("test")
  html <- as.character(ui)
  expect_true(grepl("Intensity Distribution", html))
  expect_true(grepl("Mean-Variance", html))
  expect_true(grepl("Geary", html))
  expect_true(grepl("SNR", html))
  expect_true(grepl("QC Overview", html))
  expect_true(grepl("TIC Spatial", html))
})

test_that("runShinyTAG returns a shiny app object", {
  app <- runShinyTAG()
  expect_s3_class(app, "shiny.appobj")
})

test_that("mod_importServer returns NULL before processing", {
  shiny::testServer(mod_importServer, {
    expect_null(session$returned())
  })
})

test_that("mod_importServer rejects missing imzML path", {
  shiny::testServer(mod_importServer, {
    session$setInputs(mode = "raw", imzml = "/nonexistent.imzML",
                      panel = "/nonexistent.csv")
    session$setInputs(run = 1)
    expect_null(results())
  })
})

test_that("mod_importServer loads .Rdata file", {
  skip_if_not_installed("gutenTAG")
  rdata_path <- system.file("extdata/Example_processed.Rdata",
                            package = "gutenTAG")
  skip_if(rdata_path == "", message = "Example .Rdata not found")

  shiny::testServer(mod_importServer, {
    session$setInputs(mode = "rdata", rdata = rdata_path)
    session$setInputs(run = 1)
    res <- results()
    expect_type(res, "list")
    expect_true(all(c("processed", "metapeaks", "panel") %in% names(res)))
    expect_true(ncol(res$processed$IntensityDF) > 0)
  })
})

test_that("mod_spectraServer returns NULL before click", {
  shiny::testServer(mod_spectraServer, args = list(data = shiny::reactive(NULL)), {
    expect_null(session$returned())
  })
})

test_that("mod_imageServer accepts NULL data without error", {
  shiny::testServer(mod_imageServer,
    args = list(data = shiny::reactive(NULL),
                clicked_channel = shiny::reactive(NULL)), {
    # module loads without crashing when data is NULL
    expect_true(TRUE)
  })
})

test_that("mod_previewUI returns a tagList", {
  ui <- mod_previewUI("test")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("mod_previewUI contains all six pipeline steps", {
  ui <- mod_previewUI("test")
  html <- as.character(ui)
  expect_true(grepl("Window", html))
  expect_true(grepl("SNR", html))
  expect_true(grepl("Smoothing", html))
  expect_true(grepl("Sparsity", html))
  expect_true(grepl("Limits", html))
  expect_true(grepl("Association", html))
  expect_true(grepl("Accept parameters", html))
})

test_that("mod_importUI contains the app mode toggle", {
  ui <- mod_importUI("test")
  html <- as.character(ui)
  expect_true(grepl("processing", html))
  expect_true(grepl("preview", html))
})
