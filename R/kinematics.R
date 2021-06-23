# Kinematics-related functions

#' Return speeds
#'
#' @param t The times vector
#' @param x The x positions
#' @param y The y positions
#'
#' @return The speeds
#' @export
#'
#' @seealso \code{\link{accel}}
#'
speed <- function(t, x, y) {
  # Turn data into function...
  x_fun <- splinefun(t, x)
  y_fun <- splinefun(t, y)

  # ... so numDeriv can be used to differentiate
  vx <- numDeriv::grad(x_fun, t)
  vy <- numDeriv::grad(y_fun, t)

  speeds <- data.frame(vx, vy)
}

#' Return accelerations
#'
#' @param t The times vector
#' @param x The x positions
#' @param y The y positions
#'
#' @return The accelerations
#' @export
#'
#' @seealso \code{\link{speed}}
#'
accel <- function(t, x, y) {
  # First get speeds...
  speeds <- speed(t, x, y)

  # ... and then differentiate again, as in previous function
  vx_fun <- splinefun(t, speeds$vx)
  vy_fun <- splinefun(t, speeds$vy)

  ax <- numDeriv::grad(vx_fun, t)
  ay <- numDeriv::grad(vy_fun, t)

  accels <- data.frame(ax, ay)
}

#' Return curvatures
#'
#' @param t The times vector
#' @param x The x positions
#' @param y The y positions
#'
#' @return The local curvature
#' @export
#'
#' @seealso \code{\link{speed}, \link{accel}, \link{curvature_radius}}
#'
curvature <- function(t, x, y) {
  # First get speeds and accelerations
  speeds <- speed(t, x, y)
  aspeed <- sqrt(speeds$vx^2 + speeds$vy^2)
  accels <- accel(t, x, y)

  # Calculate the cross product
  cross_prod <- speeds$vx*accels$ay - speeds$vy*accels$ax

  # Apply the definition of curvature
  curv <- abs(cross_prod)/abs(aspeed^3)
}

#' Return curvature radius
#'
#' @param t The times vector
#' @param x The x positions
#' @param y The y positions
#'
#' @return The local curvature radius
#' @export
#'
#' @seealso \code{\link{speed}, \link{accel}, \link{curvature}}
#'
curvature_radius <- function(t, x, y) {
  # The curvature radius is just the inverse of the local curvature
  curvatures <- curvature(t, x, y)
  curv_radius <- 1 / curvatures
}
