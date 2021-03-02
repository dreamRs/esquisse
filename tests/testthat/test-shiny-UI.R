context("test-shiny-UI")

test_that("esquisserUI works", {
  expect_warning(ui <- esquisserUI(id = "id"))
  expect_is(ui, "shiny.tag.list")
})

test_that("dataImportFileUI works", {
  expect_warning(importFile <- dataImportFileUI(id = "TEST"))
  expect_is(importFile, "shiny.tag.list")
})

test_that("dataGlobalEnvUI works", {
  expect_warning(GlobalEnv <- dataGlobalEnvUI(id = "TEST"))
  expect_is(GlobalEnv, "shiny.tag.list")
})

test_that("coerceUI works", {
  expect_warning(coerce <- coerceUI(id = "TEST"))
  expect_is(coerce, "shiny.tag")
})

test_that("filterDF_UI works", {
  expect_warning(coerce <- filterDF_UI(id = "TEST"))
  expect_is(coerce, "shiny.tag.list")
})
