test_that("mod_importUI returns a tagList", {
  ui <- mod_importUI("test")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("mod_spectraUI returns a plotly output tag", {
  ui <- mod_spectraUI("test")
  expect_s3_class(ui, "shiny.tag")
})

test_that("mod_imageUI returns a tagList", {
  ui <- mod_imageUI("test")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("mod_qcUI returns a tabset panel", {
  ui <- mod_qcUI("test")
  expect_s3_class(ui, "shiny.tag")
})

test_that("runShinyTAG returns a shiny app object", {
  app <- runShinyTAG()
  expect_s3_class(app, "shiny.appobj")
})
