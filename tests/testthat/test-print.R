# Unit tests
library(testthat)

test_that("pcomma handles single numeric values correctly", {
  expect_equal(pcomma(1234567), "1,234,567")
  expect_equal(pcomma(1234567.89, .prec = 2), "1,234,567.89")
})

test_that("pcomma handles numeric vectors correctly", {
  expect_equal(pcomma(1:1000), "1,000")
  expect_equal(pcomma(seq(1, 2000, by = 2)), "1,000")
})

test_that("pcomma handles data frames correctly", {
  df <- data.frame(a = 1:1000, b = 2:1001)
  expect_equal(pcomma(df), "1,000")
})

test_that("pcomma handles lists correctly", {
  lst <- list(a = 1, b = 2, c = 3)
  expect_equal(pcomma(lst), "3")
})

test_that("ppercent handles single numeric inputs correctly", {
  expect_equal(ppercent(50, 100), "50%")
  expect_equal(ppercent(33.333, 100, .prec = 2), "33.33%")
})

test_that("ppercent handles vector inputs correctly", {
  expect_equal(ppercent(1:75, 1:100), "75%")
})

test_that("ppercent handles data frame inputs correctly", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = 1:4)
  expect_equal(ppercent(df1, df2), "75%")
})

test_that("ppercent handles invalid inputs gracefully", {
  expect_warning(result <- ppercent(1, 0), "Invalid input or division by zero")
  expect_true(is.na(result))
})
