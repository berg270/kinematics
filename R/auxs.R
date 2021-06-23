# Auxiliary functions

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

  # Absolute dynamical data
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
  # Why would you NOT want this to happen? For instance, at resampling. In such a
  # case it is smarter to set append.displacement to FALSE, drop the displacements,
  # and recalculate them with append_displacement alone with the new times

  return(data)
}

#' Return a dataframe with information about the time-to-time displacements
#'
#' @param data A dataframe with basic dynamics (typically the output of append_dynamics)
#'
#' @return A data frame including al the dynamical information, including displacements
#' @export
#'
#' @seealso \code{\link{append_dynamics}, \link{speed}, \link{accel}}
#'
append_displacement <- function(data) {
  # The displacement is a bit more complicated than other dynamical variables, as it requires knowing the time differences
  # and thus is not rigorously an instantaneous measure.

  # The time differences are extracted here
  dts <- c(0, diff(data$time)) # The zero ensures dts and data have the same length

  # Extract the displacements
  disp_x <- dts * data$vx
  disp_y <- dts * data$vy
  adisp <- dts * data$aspeed

  # Append them to the final result
  data <- cbind(data, data.frame(disp_x, disp_y, adisp))
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
  arenas <- read.csv(paste('data/arenas_', data_loc$test_species[1], '.csv', sep = ''))
  # Convert scale
  scale_value <- 0.46 # This value is extracted from the protocol
  arenas[2:5] <- apply(arenas[2:5], 2, function(x) x*scale_value)
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

#' Remove unrealistic speeds
#'
#' @param input_data A kinematic data frame
#'
#' @return A kinematic data frame, with unrealistic speeds filtered out
#' @export
remove_unrealistic_speeds <- function(input_data){

  # Set threshold for unrealistic speed
  unrealistic_speed <- mean(input_data$aspeed)+2*sd(input_data$aspeed) #(90/0.035)/5
  # Remove rows that contain unrealistic speeds
  output_data <- filter(input_data, aspeed < unrealistic_speed)
  # Return output
  return(output_data)
}

#' Return true if an only if x is an even integer
#'
#' @param x An integer number
#'
#' @return A boolean indicating if the input is even
is_even <- function(x) return(x %% 2 == 0)
