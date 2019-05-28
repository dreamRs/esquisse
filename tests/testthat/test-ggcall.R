context("test-ggcall")

test_that("ggcall works", {
  
  expression <- ggcall(
    data = "mtcars",
    mapping = list(x = "mpg", y = "wt"),
    geom = "point"
  )
  gg <- eval(expression)
  
  expect_is(expression, "call")
  expect_is(gg, "ggplot")
})

test_that("ggcall args works", {
  
  expression <- ggcall(
    data = "mtcars", 
    mapping = list(x = "carat", fill = "cut"), 
    geom = "histogram",
    coord = "flip", 
    labs = list(title = "My title"),
    scales = "fill_brewer",
    scales_args = list(palette = "Blues"),
    theme = "minimal", 
    facet = c("clarity"),
    theme_args = list(legend.position = "bottom")
  )
  expression <- deparse(expression)
  
  expect_true(any(grepl("geom_histogram", expression)))
  expect_true(any(grepl("coord_flip", expression)))
  expect_true(any(grepl("scale_fill_brewer", expression)))
  expect_true(any(grepl("theme_minimal", expression)))
  expect_true(any(grepl("facet_wrap", expression)))
  expect_true(any(grepl("labs", expression)))
})

