# Auxiliary functions

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
