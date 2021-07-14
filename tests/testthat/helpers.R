## ====== Auxiliary functions for testing ======

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

#' Expect NAs vector
#'
#' @param obj An input
#' @param ... Optional parameters for expect_true
#'
#' @return Nothing
#'
expect_nas <- function(obj, ...) {
  expect_true(all(is.na(obj)), ...)
}

#' 2D uniformly accelerated movement
#'
#' Generates a 2D accelerated movement time series from its analytic expression
#' \url{https://en.wikipedia.org/wiki/Acceleration#Uniform_acceleration}
#'
#' @param t The vector of times
#' @param r_0 Initial position
#' @param v_0 Initial speed
#' @param a_0 Initial acceleration
#'
#' @return A dataframe with the positions and times
#'
uniformly_accelerated_mov <- function(t, r_0, v_0, a_0 = c(0, 0)) {
  x <- r_0[1] + v_0[1] * t + a_0[1] / 2 * t^2
  y <- r_0[2] + v_0[2] * t + a_0[2] / 2 * t^2

  return(data.frame(t, x, y))
}
