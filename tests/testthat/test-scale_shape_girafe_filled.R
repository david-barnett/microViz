test_that("scale values are as expected", {
  s <- scale_shape_girafe_filled()
  expect_equal(
    object = s$palette(n = 5),
    expected = c(
      "circle filled", "triangle filled", "square filled",
      "diamond filled", "triangle down filled"
    )
  )
  expect_error(
    object = s$palette(n = 6),
    regexp = "Insufficient values in manual scale. 6 needed but only 5"
  )
})

test_that("na values handled correctly", {
  s <- scale_shape_girafe_filled()
  expect_equal(s$na.value, "circle open")
  expect_true(s$na.translate)
})
