context("test-create_filters")

test_that("create_filters works", {
  
  data("economics_long", package = "ggplot2")
  economics_long <- as.data.frame(economics_long)
  filters <- create_filters(economics_long, session = list(ns = identity))
  
  expect_length(filters, 3)
  expect_named(filters, c("ui", "filters_id", "filters_na_id"))
  
  expect_is(filters$ui, "shiny.tag.list")
  
  expect_equal(length(filters$ui[[1]]), length(filters$filters_id))
  expect_equal(length(filters$filters_id), length(filters$filters_na_id))
})


test_that("make_expr_filter works", {
  
  filter_inputs <- lapply(
    X = iris, 
    FUN = function(x) {
      sort(sample(unique(x), 2))
    }
  )
  filter_nas <- lapply(
    X = iris,
    FUN = function(x) {
      sample(c(TRUE, FALSE), 2)
    }
  )
  
  filters <- make_expr_filter(
    filters = filter_inputs, 
    filters_na = filter_nas,
    data = iris,
    data_name = "iris"
  )
  
  expect_length(filters, 2)
  expect_named(filters, c("expr_dplyr", "expr"))
  
  expect_is(filters$expr_dplyr, "call")
  expect_is(filters$expr, "call")
})

