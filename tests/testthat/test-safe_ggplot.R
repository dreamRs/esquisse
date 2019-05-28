context("test-safe_ggplot")

test_that("safe_ggplot (no error) works", {
  
  session <- as.environment(list(
    sendInputMessage = function(inputId, message) {
      session$lastInputMessage = list(id = inputId, message = message)
    },
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    },
    sendNotification = function(...) {
      session$lastNotification <- list(...)
    }
  ))
  
  p <- safe_ggplot(
    expr = rlang::expr(ggplot(iris) + geom_histogram(aes(Sepal.Length))), 
    session = session
  )
  expect_is(p, "ggplot_built")
  expect_null(session$lastNotification)

})

test_that("safe_ggplot (error) works", {
  
  session <- as.environment(list(
    sendInputMessage = function(inputId, message) {
      session$lastInputMessage = list(id = inputId, message = message)
    },
    sendCustomMessage = function(type, message) {
      session$lastCustomMessage <- list(type = type, message = message)
    },
    sendNotification = function(...) {
      session$lastNotification <- list(...)
    }
  ))
  
  
  p <- safe_ggplot(
    expr = rlang::expr(ggplot(iris) + geom_histogram(aes(DONOTEXIST))), 
    session = session
  )
  
  expect_is(p, "list")
  expect_false(is.null(session$lastNotification))
  
})

