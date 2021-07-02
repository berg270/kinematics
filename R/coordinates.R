#' Get polar coordinates
#'
#' @param x Vector of x coordinates
#' @param y Vector if y coordinates
#' @param origin (Default = c(0, 0)) Position of the origin of coordinates
#'
#' @return
#' @export
#'
get_polar_coordinates <- function(x, y, origin = c(0, 0)) {
  # Use coordinates relative to the origin
  x_r <- x - origin[1]
  y_r <- y - origin[2]

  # Calculate the polar coordinates
  r <- sqrt(x_r^2 + y_r^2)
  th <- atan2(y = y_r, x = x_r)

  return(data.frame(r = r, th = th))
}
