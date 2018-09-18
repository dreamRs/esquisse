context("test-get_code_scale")

test_that("get_code_scale works", {
  
  pal_fill_discrete <- get_code_scale(fill = "color", filltype = "discrete", params = list(palette = "Dark2"))
  expect_is(pal_fill_discrete, "character")
  expect_true(grepl("scale_fill_brewer", pal_fill_discrete))
  
  pal_fill_continuous <- get_code_scale(fill = "color", filltype = "continuous", params = list(palette = "Blues"))
  expect_is(pal_fill_continuous, "character")
  expect_true(grepl("scale_fill_distiller", pal_fill_continuous))
  
  pal_fill_ggplot2 <- get_code_scale(fill = "color", filltype = "discrete", params = list(palette = "ggplot2"))
  expect_null(pal_fill_ggplot2)
  
  pal_color_discrete <- get_code_scale(color = "color", colortype = "discrete", params = list(palette = "Dark2"))
  expect_is(pal_color_discrete, "character")
  expect_true(grepl("scale_color_brewer", pal_color_discrete))
  
  pal_color_continuous <- get_code_scale(color = "color", colortype = "continuous", params = list(palette = "Blues"))
  expect_is(pal_color_continuous, "character")
  expect_true(grepl("scale_color_distiller", pal_color_continuous))
  
  pal_color_ggplot2 <- get_code_scale(color = "color", colortype = "discrete", params = list(palette = "ggplot2"))
  expect_null(pal_color_ggplot2)

})
