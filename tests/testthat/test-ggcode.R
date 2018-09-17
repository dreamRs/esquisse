context("test-ggcode")

test_that("ggcode works", {
  
  code_gg <- ggcode(
    data = "mtcars",
    aes = list(x = "hp", y = "wt"),
    geom = "point"
  )
  
  expect_is(code_gg, "character")
  expect_true(grepl(pattern = "ggplot", x = code_gg))
  expect_true(grepl(pattern = "aes", x = code_gg))
  expect_true(grepl(pattern = "x = hp", x = code_gg))
  expect_true(grepl(pattern = "point", x = code_gg))

})

