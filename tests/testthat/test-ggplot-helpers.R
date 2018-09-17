

# test-possible_geom ------------------------------------------------------

context("test-possible_geom")

test_that("possible_geom (one var continuous) works", {
  
  data("mpg", package = "ggplot2")
  geoms_continuous <- possible_geom(data = mpg, x = "cyl")
  
  expect_length(geoms_continuous, 4)
  expect_true("histogram" %in% geoms_continuous)
})



test_that("possible_geom (one var date) works", {
  
  data("economics", package = "ggplot2")
  geoms_date <- possible_geom(data = economics, x = "date")
  
  expect_length(geoms_date, 1)
  expect_true("histogram" %in% geoms_date)
})



test_that("possible_geom (one var discrete) works", {
  
  data("mpg", package = "ggplot2")
  geoms_discrete <- possible_geom(data = mpg, x = "manufacturer")
  
  expect_length(geoms_discrete, 1)
  expect_true("bar" %in% geoms_discrete)
})



test_that("possible_geom (two vars continuous) works", {
  
  data("mpg", package = "ggplot2")
  geoms_2_continuous <- possible_geom(data = mpg, x = "cyl", y = "displ")
  
  expect_length(geoms_2_continuous, 3)
  expect_true("point" %in% geoms_2_continuous)
})



test_that("possible_geom (two vars date+continuous) works", {
  
  data("economics", package = "ggplot2")
  geoms_date_continuous <- possible_geom(data = economics, x = "date", y = "pce")
  
  expect_length(geoms_date_continuous, 1)
  expect_true("line" %in% geoms_date_continuous)
})







# test-ggtry --------------------------------------------------------------

context("test-ggtry")

test_that("ggtry return ggplot", {
  
  data("mpg", package = "ggplot2")
  p <- ggtry(data = mpg, x = "manufacturer")
  p2 <- ggtry(data = mpg, x = "cty", y = "cyl")
  
  data("diamonds", package = "ggplot2")
  p3 <- ggtry(data = diamonds, x = "cut", fill = "color", params = list(position = "dodge"))

  expect_is(p, "ggplot")
  expect_is(p2, "ggplot")
  expect_is(p3, "ggplot")
})





# test-guess_aes ----------------------------------------------------------

context("test-guess_aes")

test_that("guess_aes works", {
  
  aes1 <- guess_aes(x = "carat", xtype = "continuous")
  aes2 <- guess_aes(x = "carat", y = "depth")
  aes3 <- guess_aes(x = "carat", y = "depth", color = "color")
  aesw <- guess_aes(x = "color", xtype = "categorical", y = "depth", ytype = "continuous", geom = "bar")
  
  expect_is(aes1, "list")
  expect_length(aes1, 1)
  expect_identical(names(aes1), c("x"))
  
  expect_is(aes2, "list")
  expect_length(aes2, 2)
  expect_identical(names(aes2), c("x", "y"))
  
  expect_is(aes3, "list")
  expect_length(aes3, 3)
  expect_identical(names(aes3), c("x", "y", "color"))
  
  expect_is(aesw, "list")
  expect_length(aesw, 2)
  expect_identical(names(aesw), c("x", "weight"))
})






# test-guess_geom ---------------------------------------------------------

context("test-guess_geom")

test_that("guess_geom guess geom", {
  
  geom_continuous <- guess_geom(xtype = "continuous")
  expect_identical(geom_continuous, "histogram")
  
  geom_continuous_force <- guess_geom(xtype = "continuous", type = "boxplot")
  expect_identical(geom_continuous_force, "boxplot")
  
  geom_continuous_force_wrong <- guess_geom(xtype = "continuous", type = "bar")
  expect_identical(geom_continuous_force_wrong, "blank")
  
  geom_2_continuous <- guess_geom(xtype = "continuous", ytype = "continuous")
  expect_identical(geom_2_continuous, "point")
})




# test-ggplot_geom_vars ---------------------------------------------------


context("test-ggplot_geom_vars")

test_that("ggplot_geom_vars return data.frame", {
  dat_geoms <- ggplot_geom_vars()
  
  expect_is(dat_geoms, "data.frame")
  expect_identical(ncol(dat_geoms), 4L)
})





