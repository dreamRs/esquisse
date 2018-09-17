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

