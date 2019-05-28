context("test-shiny-UI")

test_that("esquisserUI works", {
  ui <- esquisserUI(id = "id")
  expect_is(ui, "shiny.tag.list")
  expect_is(htmltools::findDependencies(ui), "list")
  names_deps <- unlist(lapply(htmltools::findDependencies(ui), `[[`, "name"))
  expect_true("shinyWidgets" %in% names_deps)
})

test_that("dataImportFileUI works", {
  importFile <- dataImportFileUI(id = "TEST")
  expect_is(importFile, "shiny.tag.list")
})

test_that("dataGlobalEnvUI works", {
  GlobalEnv <- dataGlobalEnvUI(id = "TEST")
  expect_is(GlobalEnv, "shiny.tag.list")
})

test_that("coerceUI works", {
  coerce <- coerceUI(id = "TEST")
  expect_is(coerce, "shiny.tag")
})

test_that("filterDF_UI works", {
  coerce <- filterDF_UI(id = "TEST")
  expect_is(coerce, "shiny.tag.list")
})
