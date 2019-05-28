context("test-mapping")


test_that("build_aes works", {
  
  mapping <- build_aes(iris, x = "Sepal.Width")
  expect_true(is.list(mapping))
  expect_length(mapping, 1)
  
  mapping <- build_aes(iris, x = "Sepal.Width",  y = "Sepal.Width")
  expect_true(is.list(mapping))
  expect_length(mapping, 2)
  
  
  mapping <- build_aes(iris, x = "Sepal.Width", geom = "boxplot")
  expect_true(is.list(mapping))
  expect_length(mapping, 2)
})
