context("test-safe_ggplot")

test_that("safe_ggplot (no error) works", {

  session <- shiny::MockShinySession$new()

  p <- safe_ggplot(
    expr = rlang::expr(ggplot(iris) + geom_histogram(aes(Sepal.Length))),
    session = session
  )
  expect_is(p, "ggplot_built")
  expect_null(session$userData$esquisse_notifications)

})

test_that("safe_ggplot (error) works", {

  session <- shiny::MockShinySession$new()

  p <- safe_ggplot(
    expr = rlang::expr(ggplot(iris) + geom_histogram(aes(DONOTEXIST))),
    session = session
  )

  expect_is(p, "list")
  expect_false(is.null(session$userData$esquisse_notifications))

})

