context("test-scales")


test_that("which_pal_scale works", {
  
  auto <- which_pal_scale(
    mapping = ggplot2::aes(fill = Sepal.Length), 
    palette = "ggplot2", 
    data = iris
  )
  
  expect_length(auto, 2)
  expect_named(auto, c("scales", "args"))
  
  args <- which_pal_scale(
    mapping = ggplot2::aes(color = variable), 
    palette = "Blues", 
    color_type = "discrete"
  )
  
  expect_length(args$args, 1)
  expect_identical(args$scales, names(args$args))
  
  two <- which_pal_scale(
    mapping = ggplot2::aes(color = var1, fill = var2), 
    palette = "Blues", 
    color_type = "discrete",
    fill_type = "continuous"
  )
  
  expect_length(two$scales, 2)
})
