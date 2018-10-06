context("test-get-data")

test_that("no data provided", {
  no_data <- get_data()
  expect_length(no_data, 2)
  expect_null(no_data$esquisse_data)
  expect_false(nzchar(no_data$esquisse_data_name))
})


test_that("data.frame name as string", {
  char_data <- get_data("iris")
  expect_length(char_data, 2)
  expect_identical(char_data$esquisse_data, iris)
  expect_identical(char_data$esquisse_data_name, "iris")
})


test_that("data.frame as argument", {
  arg_data <- get_data(iris)
  expect_length(arg_data, 2)
  expect_identical(arg_data$esquisse_data, iris)
  expect_identical(arg_data$esquisse_data_name, "iris")
})

test_that("custom data.frame as argument", {
  cust_data <- get_data(iris[iris$Species == "virginica", ])
  expect_length(cust_data, 2)
  expect_identical(cust_data$esquisse_data, iris[iris$Species == "virginica", ])
  expect_identical(cust_data$esquisse_data_name, "iris[iris$Species == \"virginica\", ]")
})


test_that("bad dataa", {
  expect_warning(get_data("truc"))
})
