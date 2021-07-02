context('Coordinates')

test_that('Polar coordinates (no shift)', {
  # Create some points in cartesian coordinates
  xs <- c(0, 0, 1, 2)
  ys <- c(0, 1, 1, 2)

  # The expected polar coordinates are
  rs_expected <-  c(0, 1,    sqrt(2), sqrt(8))
  ths_expected <- c(0, pi/2, pi/4,    pi/4)

  # Calculate using the function
  p <- get_polar_coordinates(xs, ys)

  # Check that the results match
  expect_equal(rs_expected, p$r)
  expect_equal(ths_expected, p$th)
})

test_that('Polar coordinates (with shift)', {
  # Create some points in cartesian coordinates
  xs <- c(0, 0, 1, 2) + 2
  ys <- c(0, 1, 1, 2) + 1

  # The expected polar coordinates are
  rs_expected <-  c(0, 1,    sqrt(2), sqrt(8))
  ths_expected <- c(0, pi/2, pi/4,    pi/4)

  # Calculate using the function
  p <- get_polar_coordinates(xs, ys, origin = c(2, 1))

  # Check that the results match
  expect_equal(rs_expected, p$r)
  expect_equal(ths_expected, p$th)
})
