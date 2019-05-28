context("test-geoms")

test_that("potential_geoms_ref works", {
  
  ref <- potential_geoms_ref()
  
  expect_is(ref, "data.frame")
})


test_that("match_geom_args works", {
  
  params <- list(
    bins = 30,
    scale = "width",
    adjust = 2,
    position = "stack",
    size = 1.6,
    fill = "#112246"
  )
  histo_params <- match_geom_args(geom = "histogram", args = params)
  expect_is(histo_params, "list")
  expect_length(histo_params, 2)
  histo_params <- match_geom_args(geom = "histogram", args = params, add_aes = FALSE)
  expect_length(histo_params, 1)
  histo_params <- match_geom_args(geom = "violin", args = params)
  expect_length(histo_params, 3)
})


test_that("potential_geoms works", {
  
  one_var_c <- potential_geoms(
    data = iris,
    mapping = aes(x = Sepal.Length)
  )
  expect_true(length(one_var_c) > 0)
  expect_true("histogram" %in% one_var_c)
  
  one_var_d <- potential_geoms(
    data = iris,
    mapping = aes(x = Species)
  )
  expect_true(length(one_var_d) > 0)
  expect_true("bar" %in% one_var_d)
  
  auto <- potential_geoms(
    data = iris,
    mapping = aes(x = Sepal.Length),
    auto = TRUE
  )
  expect_length(auto, 1)
  
  two_var_c <- potential_geoms(
    data = iris,
    mapping = aes(x = Sepal.Length, y = Sepal.Width)
  )
  expect_true(length(two_var_c) > 0)
  expect_true("point" %in% two_var_c)
})

