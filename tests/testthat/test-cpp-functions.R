test_that("Neighbours are correct", {
  set.seed(1234)
  m <- matrix(1:100, 10, 10)
  n <- neighbours(1, 1, 4, m)
  expect_identical(n, c(11, 2, 22, 13))
})

test_that("No neighbours for edges", {
  set.seed(1234)
  m <- matrix(1:100, 10, 10)
  n1 <- neighbours(0, 1, 4, m)
  n2 <- neighbours(1, ncol(m) - 1, 4, m)
  expect_length(n1, 0)
  expect_length(n2, 0)
})


test_that("Slope is correct", {
  set.seed(1234)
  m <- matrix(1:100, 10, 10)
  n <- neighbours(1, 1, 4, m)
  sl <- slope(n, m[2, 2], 1)
  s <- m[2, 2] - c(11, 2, 22, 13)
  s <- atan(s) * 180 / pi
  expect_equal(sl, s, tolerance = 1e-6)
})


test_that("Distance is correct", {
  set.seed(1234)
  m <- matrix(1:100, 10, 10)
  n <- neighbours(1, 1, 4, m)
  dist <- distances(n, m[2, 2], 1)
  s <- m[2, 2] - c(11, 2, 22, 13)
  s <- atan(s)
  d <- 1 / cos(s)
  expect_equal(dist, d, tolerance = 1e-6)
})

test_that("Energy works", {
  set.seed(1234)
  m <- matrix(1:100, 10, 10)
  n <- neighbours(1, 1, 4, m)
  sl <- slope(n, m[2, 2], 1)
  dist <- distances(n, 1, 10)
  mass <- 1000
  en <- energy(sl, dist, mass, 1, FALSE)
  ar <- 8.0 * mass ^ -0.34
  mec <- 50.0 * (1.0 + sin((2.0 * sl - 74.0) / 180.0 * pi)) * mass ^ -0.12
  work <- (ar + mec) * mass * dist;
  expect_equal(en, work, tolerance = 1e-6)
})
