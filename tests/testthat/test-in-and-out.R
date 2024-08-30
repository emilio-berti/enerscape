test_that("in and out work", {
  library(terra)
  library(enerscape)
  m <- matrix(sample(1:1e5, 9), ncol = 3)
  in_slope <- slope(m, 5, m[2, 2], out = 0)
  out_slope <- slope(m, 5, m[2, 2], out = 1)
  expect_equal(in_slope, -out_slope)
})

test_that("in and out work", {
  library(terra)
  library(enerscape)
  m <- matrix(sample(1:1e3, 1e2), ncol = 1e1)
  r <- rast(m)
  en_in <- enerscape(r, 1, direction = "in")
  en_out <- enerscape(r, 1, direction = "out")
  vals <- values(c(en_in, en_out))
  expect_false(all(vals[, 1] == vals[, 2]))
})
