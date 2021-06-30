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

#' Returns a data frame with extra columns with polar coordinates
#'
#' @param data_loc The clean data from a given location
#'
#' @return A data frame including the polar coordinates
#' @export
#'
append_polar_coordinates <- function(data_loc) {

  # Load data on arenas
  # Load coordinates of gammarus or snail protocol
  arenas <- utils::read.csv(paste('data/arenas_', data_loc$test_species[1], '.csv', sep = ''))
  # Convert scale
  scale_value <- 0.46 # This value is extracted from the protocol
  arenas[2:5] <- apply(arenas[2:5], 2, function(x) x*scale_value)

  # Auxiliary function
  is_even <- function(x) return(x %% 2 == 0)
  # Combination of cosm and ind determines absolute position on board
  data_loc$ind_abd <- ifelse(is_even(data_loc$cosm_nr[[1]]), data_loc$ind+10, data_loc$ind)
  # Merge with other data
  data_loc <- merge(data_loc, arenas, by.x = 'ind_abd', by.y = 'id')

  # # Calculate min and max of x and y coordinates
  # max_x <- max(data_loc$x)
  # min_x <- min(data_loc$x)
  # max_y <- max(data_loc$y)
  # min_y <- min(data_loc$y)

  # Calculate center of the Petri dish
  #r_0_x <- mean(c(max_x, min_x))
  r_0_x <- data_loc$cx[1]
  #r_0_y <- mean(c(max_y, min_y))
  r_0_y <- data_loc$cy[1]

  # Correct the coordinates according to the center of the Petri dish
  x_r <- data_loc$x - r_0_x
  y_r <- data_loc$y - r_0_y

  # Calculate the polar coordinates
  r <- sqrt(x_r^2 + y_r^2)
  th <- atan2(y = y_r, x = x_r)

  # Paste everything together
  data <- cbind(data_loc, x_r, y_r, r, th)
}
