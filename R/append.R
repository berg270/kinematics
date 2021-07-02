# Append functions
#
# These functions extend basic dynamical data frames (i.e.: data frames with a
# time, an x and a y column) with extra columns containing information such as
# speed, acceleration, etc.

#' Return a data frame with extra columns with dynamical information
#'
#' @param data_loc The clean data from a given location
#' @param append.displacement (Optional) Set it to FALSE to not calculate displacements. Useful if the data is going to be resampled
#'
#' @return A data frame including instantaneous dynamical variables, such as speed and acceleration
#' @export
#'
#' @seealso \code{\link{speed}, \link{accel}, \link{append_displacement}}
#'
append_dynamics <- function(data_loc, append.displacement = TRUE) {
  # Directional dynamical data
  speeds <- speed(data_loc$time, data_loc$x, data_loc$y)
  accels <- accel(data_loc$time, data_loc$x, data_loc$y)

  # Scalar dynamical data
  aspeed <- sqrt(speeds$vx^2 + speeds$vy^2)
  aaccel <- sqrt(accels$ax^2 + accels$ay^2)
  curv <- curvature(data_loc$time, data_loc$x, data_loc$y)
  curv_radius <- curvature_radius(data_loc$time, data_loc$x, data_loc$y)

  # Paste everything together
  data <- cbind(data_loc, speeds, aspeed, accels, aaccel, curv, curv_radius)

  # Add displacements if required
  if(append.displacement) {
    data <- append_displacement(data)
  }
  # Why would you NOT want this to happen? For instance, at subsampling. In such a
  # case it is smarter to set append.displacement to FALSE, and recalculate them
  # manually with append_displacement alone

  return(data)
}

#' Return a dataframe with information about the time-to-time displacements
#'
#' The displacement is a bit more complicated than other dynamical variables,
#' as it depends on the sampling frequency. If you are subsampling, always re-run
#' append_displacement after subsampling.
#'
#' @param data A dataframe with basic dynamics (typically the output of append_dynamics)
#'
#' @return A data frame including all the dynamical information, including displacements
#' @export
#'
#' @seealso \code{\link{append_dynamics}, \link{speed}}
#'
append_displacement <- function(data) {
  # Extract the displacements
  disps <- displacement(data$x, data$y)

  # Append them to the final result
  data <- cbind(data, disps)

  return(data)
}
