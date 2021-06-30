# Kinematics-related functions

#' Approximate derivative
#'
#' @param t Vector of times
#' @param x Vector of values
#'
#' @return A vector (of the same size of t) representing the numerical derivative
#' @seealso \code{\link{speed}, \link{accel}}
#'
approx_derivative <- function(t, x) {
  # Turn data into approximating function...
  x_fun <- stats::splinefun(t, x)

  # ... so numDeriv can be used to differentiate anywhere.
  dxdt <- numDeriv::grad(x_fun, t)

  # Why not use just a differences vector?
  # Because this way we ensure that both the input and output vectors have the
  # same size.

  return(dxdt)
}

#' Return speeds
#'
#' @param t The times vector
#' @param x The x positions
#' @param y The y positions
#'
#' @return The speeds
#' @export
#'
#' @seealso \code{\link{accel}, \link{approx_derivative}}
#'
speed <- function(t, x, y) {
  # Differentiate the positions...
  vx <- approx_derivative(t, x)
  vy <- approx_derivative(t, y)

  # ... and return result as data frame
  speeds <- data.frame(vx, vy)

  return(speeds)
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
#' @seealso \code{\link{speed}, \link{approx_derivative}}
#'
accel <- function(t, x, y) {
  # First get speeds...
  speeds <- speed(t, x, y)

  # ... and then differentiate again
  ax <- approx_derivative(t, speeds$vx)
  ay <- approx_derivative(t, speeds$vy)

  # Return result as data frame
  accels <- data.frame(ax, ay)

  return(accels)
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
  curv <- abs(cross_prod) / abs(aspeed^3)

  return(curv)
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

  return(curv_radius)
}

#' Return displacements
#'
#' @param x The x positions
#' @param y The y positions
#'
#' @return The displacements between a position and its previous
#' @export
displacement <- function(x, y) {
  # Extract the displacements
  disp_x <- c(NA, diff(x)) # The NA ensures that displacements have the ...
  disp_y <- c(NA, diff(y)) # ... same length as the data
  adisp <- sqrt(disp_x^2 + disp_y^2)

  # Append them to the final result
  disps <- data.frame(disp_x, disp_y, adisp)

  return(disps)
}
