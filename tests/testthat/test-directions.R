test_that("directions work", {
  m <- matrix(1:9, ncol = 3)
  expect_identical(
    neighbours(1, 1, 4, m, direction = 0),
    c(4, 2, 8, 6)
  )
  expect_identical(
    neighbours(1, 1, 4, m, direction = 1),
    2
  )
  expect_identical(
    neighbours(1, 1, 4, m, direction = 2),
    6
  )
  expect_identical(
    neighbours(1, 1, 4, m, direction = 3),
    8
  )
  expect_identical(
    neighbours(1, 1, 4, m, direction = 4),
    4
  )
})

test_that("directions error", {
  expect_error(enerscape(terra::rast(m), 10, neigh = 8, direction = "right"))
  expect_error(enerscape(terra::rast(m), 10, neigh = 4, direction = "asd"))
})
