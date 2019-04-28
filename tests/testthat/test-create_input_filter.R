context("test-create_input_filter")

test_that("create input filter discrete var", {
  data("mpg", package = "ggplot2")
  filter_discrete_ui <- esquisse:::create_input_filter(
    data = mpg, 
    var = "manufacturer",
    ns = shiny::NS("test")
  )
  expect_is(filter_discrete_ui, "shiny.tag")
  expect_true(grepl(pattern = "pontiac", filter_discrete_ui))
})

test_that("create input filter continuous var", {
  data("mpg", package = "ggplot2")
  filter_continuous_ui <- esquisse:::create_input_filter(
    data = mpg, 
    var = "cyl",
    ns = shiny::NS("test")
  )
  expect_is(filter_continuous_ui, "shiny.tag.list")
  expect_true(grepl(pattern = "slider", filter_continuous_ui))
})

test_that("create input filter date var", {
  data("economics", package = "ggplot2")
  filter_date_ui <- esquisse:::create_input_filter(
    data = economics, 
    var = "date",
    ns = shiny::NS("test")
  )
  expect_is(filter_date_ui, "shiny.tag.list")
  expect_true(grepl(pattern = "slider", filter_date_ui))
})

test_that("create input NA", {
  input_na <- esquisse:::naInput(var = "cyl", ns = shiny::NS("test"))
  expect_is(input_na, "shiny.tag")
  expect_true(grepl(pattern = "test-cyl_na_remove", input_na))
})


