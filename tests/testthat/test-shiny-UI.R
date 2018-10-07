context("test-shiny-UI")

test_that("esquisserUI works", {
  ui <- esquisserUI(id = "id")
  expect_is(ui, "shiny.tag.list")
  expect_is(htmltools::findDependencies(ui), "list")
  names_deps <- unlist(lapply(htmltools::findDependencies(ui), `[[`, "name"))
  expect_true("shinyWidgets" %in% names_deps)
})

test_that("chooseData modal works", {
  data("mpg", package = "ggplot2")
  modal <- chooseDataModal(ns = shiny::NS("test"), defaultData = mpg)
  expect_is(modal, "shiny.tag")
})

test_that("coerceUI works", {
  coerce <- esquisse:::coerceUI(id = "TEST")
  expect_is(coerce, "shiny.tag")
})

