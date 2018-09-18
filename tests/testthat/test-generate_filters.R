context("test-generate_filters")

test_that("filter discrete var", {
  data("mpg", package = "ggplot2")
  
  filter_discrete <- generate_filters(
    x = "manufacturer", 
    params = list(manufacturer = c("audi", "toyota")),
    params_na = list(manufacturer = FALSE),
    data = mpg
  )
  
  expect_length(filter_discrete, 2)
  expect_identical(names(filter_discrete), c("code", "ind"))
  expect_true(grepl(pattern = "%in%", filter_discrete$code))
  expect_true(grepl(pattern = "toyota", filter_discrete$code))
  expect_identical(sum(filter_discrete$ind), sum(mpg$manufacturer %in% c("audi", "toyota")))
})



test_that("filter continuous var", {
  data("mpg", package = "ggplot2")
  
  filter_continuous <- generate_filters(
    x = "displ", 
    params = list(displ = c(2.4, 4.6)),
    params_na = list(displ = FALSE),
    data = mpg
  )
  
  expect_length(filter_continuous, 2)
  expect_identical(names(filter_continuous), c("code", "ind"))
  expect_true(grepl(pattern = ">=", filter_continuous$code))
  expect_true(grepl(pattern = "displ", filter_continuous$code))
  expect_identical(sum(filter_continuous$ind), sum(mpg$displ >= 2.4 & mpg$displ <= 4.6))
})


test_that("filter two continuous vars", {
  data("mpg", package = "ggplot2")
  
  filter_2_continuous <- lapply(
    X = c("displ", "cty"),
    FUN =  generate_filters,
    params = list(displ = c(2.4, 4.6), cty = c(14, 19)),
    params_na = list(displ = FALSE, cty = FALSE),
    data = mpg
  )
  
  
  expect_length(filter_2_continuous, 2)
  expect_identical(names(filter_2_continuous[[1]]), c("code", "ind"))
  expect_identical(names(filter_2_continuous[[2]]), c("code", "ind"))
  expect_true(grepl(pattern = ">=", filter_2_continuous[[1]]$code))
  expect_true(grepl(pattern = "displ", filter_2_continuous[[1]]$code))
  expect_true(grepl(pattern = ">=", filter_2_continuous[[2]]$code))
  expect_true(grepl(pattern = "cty", filter_2_continuous[[2]]$code))
  expect_identical(
    sum(Reduce(`&`, lapply(filter_2_continuous, `[[`, "ind"))), 
    sum(mpg$displ >= 2.4 & mpg$displ <= 4.6 & mpg$cty >= 14 & mpg$cty <= 19)
  )
})




test_that("filter date var", {
  data("economics", package = "ggplot2")
  
  filter_date <- generate_filters(
    x = "date", 
    params = list(date = as.Date(c("1979-06-08", "2003-04-23"))),
    params_na = list(date = FALSE),
    data = economics
  )
  
  expect_length(filter_date, 2)
  expect_identical(names(filter_date), c("code", "ind"))
  expect_true(grepl(pattern = "date", filter_date$code))
  expect_true(grepl(pattern = "as\\.Date", filter_date$code))
  expect_identical(sum(filter_date$ind), 
                   sum(economics$date >= as.Date('1979-06-08') & economics$date <= as.Date('2003-04-23')))
})



test_that("filter POSIX var", {
  data("economics", package = "ggplot2")
  economics$date <- as.POSIXct(economics$date)
  
  filter_datetime <- generate_filters(
    x = "date", 
    params = list(date = as.POSIXct(c("1979-06-08", "2003-04-23"))),
    params_na = list(date = FALSE),
    data = economics
  )
  
  expect_length(filter_datetime, 2)
  expect_identical(names(filter_datetime), c("code", "ind"))
  expect_true(grepl(pattern = "date", filter_datetime$code))
  expect_true(grepl(pattern = "as\\.POSIXct", filter_datetime$code))
  expect_identical(sum(filter_datetime$ind),
                   sum(economics$date >= as.POSIXct('1979-06-08') & economics$date <= as.POSIXct('2003-04-23')))
})
