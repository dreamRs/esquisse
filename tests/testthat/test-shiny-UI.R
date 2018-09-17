context("test-shiny-UI")

test_that("esquisserUI works", {
  ui <- esquisse:::esquisserUI()
  expect_is(ui, "shiny.tag.list")
  expect_is(htmltools::findDependencies(ui), "list")
  names_deps <- unlist(lapply(htmltools::findDependencies(ui), `[[`, "name"))
  expect_true("shinyWidgets" %in% names_deps)
})
