context("test-utils")

test_that("badgeType works", {

  badges <- badgeType(
    col_name = c("discrete", "time", "continuous", "id"),
    col_type = c("discrete", "time", "continuous", "id")
  )

  expect_length(badges, 4)
  expect_true(grepl(pattern = "label-discrete", x = badges[[1]]))
  expect_true(grepl(pattern = "label-datetime", x = badges[[2]]))
  expect_true(grepl(pattern = "label-continuous", x = badges[[3]]))
  expect_true(grepl(pattern = "label-default", x = badges[[4]]))
})




test_that("get_df works", {
  dat1 <- get_df("DONT_EXIST")
  expect_null(dat1)

  dat2 <- get_df("mpg")
  expect_is(dat2, "data.frame")

  my_data <- data.frame(x = letters, y = 1:26)
  e <- new.env()
  assign("my_data", my_data, envir = e)
  dat3 <- get_df("my_data", env = e)
  expect_is(dat3, "data.frame")
})


test_that("search_obj works", {

  e <- new.env()

  no_df <- search_obj(env = e)
  expect_null(no_df)

  data("economics", package = "ggplot2")
  assign("economics", economics, envir = e)
  one_df <- search_obj(env = e)
  expect_length(one_df, 1)

  assign("my_vec", 1:10, envir = e)
  still_one_df <- search_obj(env = e)
  expect_length(still_one_df, 1)

})



test_that("col_type return appropriate type", {

  discrete <- col_type(x = letters)
  expect_identical(discrete, "discrete")

  id <- col_type(x = as.character(1:100))
  expect_identical(id, "id")

  continuous <- col_type(x = rnorm(10))
  expect_identical(continuous, "continuous")
  
  date <- col_type(x = Sys.Date() + 1:10)
  expect_identical(date, "time")

})


test_that("col_type works with different object type", {
  
  DF <- col_type(x = iris)
  expect_length(DF, ncol(iris))
  
  DF1 <- col_type(x = iris[1, ])
  expect_length(DF1, ncol(iris))
  
  DF2 <- col_type(x = iris[, 3, drop = FALSE])
  expect_length(DF2, 1)
  
  LIST <- col_type(x = as.list(iris))
  expect_length(LIST, ncol(iris))
  
})


test_that("makeId & idToChar works", {
  
  expect_null(makeId(NULL))
  expect_null(makeId(character(0)))
  
  expect_length(makeId(names(iris)), length(names(iris)))
  
  expect_identical(idToChar(makeId(names(iris))), names(iris))
  
})


