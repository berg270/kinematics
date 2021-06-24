context('Kinematics')

## Auxiliary functions

#' Expect equal for a constant and a vector
#'
#' Passes if and only if all the elements in the vector equal the constant
#'
#' @param constant A constant
#' @param vector A vector
#' @param ... Optional parameters for expect_equal
#'
#' @return Nothing
#'
expect_equal_constvect <- function(constant, vector, ...) {
  expect_equal(rep(constant, length(vector)), vector, ...)
}

test_that('Speed',
          {
            # Generate movement with uniform speed (https://en.wikipedia.org/wiki/Acceleration#Uniform_acceleration)
            x_0 <- 0 # Initial...
            y_0 <- 1 # ... positions
            v_expected <- c(1.2, 0.8) # Initial (and constant) speed

            t <- seq(0, 1, by=0.05)
            x <- x_0 + v_expected[1]*t
            y <- y_0 + v_expected[2]*t

            # Estimate the speeds from the generated data
            vs_estimated <- speed(t, x, y)

            # Check that the estimates are correct
            expect_equal_constvect(v_expected[1], vs_estimated$vx)
            expect_equal_constvect(v_expected[2], vs_estimated$vy)
          }
)

test_that('Acceleration',
          {
            # Generate movement with uniform acceleration (https://en.wikipedia.org/wiki/Acceleration#Uniform_acceleration)
            x_0 <- 0 # Initial...
            y_0 <- 1 # ... positions
            v_0 <- c(1.2, 0.8) # Initial speed
            a_expected <- c(0.0, -9.8) # Initial (and constant) acceleration

            t <- seq(0, 1, by=0.05)
            x <- x_0 + v_0[1]*t + a_expected[1]*t^2/2
            y <- y_0 + v_0[2]*t + a_expected[2]*t^2/2

            # Estimate the speeds from the generated data
            vs_estimated <- speed(t, x, y)
            as_estimated <- accel(t, x, y)

            # Check that the estimates are correct
            expect_equal_constvect(a_expected[1], as_estimated$ax)
            expect_equal_constvect(a_expected[2], as_estimated$ay)

            # The speeds should not be constant
            expect_true(max(abs(vs_estimated$vx - v_0[1])) > 0.0)
            expect_true(max(abs(vs_estimated$vy - v_0[2])) > 0.0)
          }
)

test_that('Curvature (circle)',
          {
            # Generate circular movement of radius 2
            R_expected <- 2
            t <- seq(0, 2*pi, by=0.05)
            x <- R_expected*cos(t)
            y <- R_expected*sin(t)

            # Estimate the speeds from the generated data
            curvs_estimated <- curvature_radius(t, x, y)

            # Check that the estimates are correct
            tol <- 1e-3
            expect_equal_constvect(R_expected, curvs_estimated, tolerance = tol)
          }
)

test_that('Curvature (ellipse)',
          {
            # Generate an elliptic movement with parameters a and b
            a <- 2
            b <- 3

            t <- seq(0, 2*pi, by=0.05)
            x <- a*cos(t)
            y <- b*sin(t)

            # The local curvature radii can be calculated analytically
            R_expected <- (a^2*(sin(t))^2 + b^2*(cos(t))^2)^(3/2)/(a*b)

            # Estimate the speeds from the generated data
            curvs_estimated <- curvature_radius(t, x, y)

            # Check that the estimates are correct
            tol <- 1e-3
            expect_equal(R_expected, curvs_estimated, tolerance = tol)
          }
)
