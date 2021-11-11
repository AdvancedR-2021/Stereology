#' Load image
#'
#' @param path
#'
#' @return A stero object
#' @export
#'
#' @examples img <- load_img(DATA_TO_BE_ADDED)
#'
load_img <- function(path) {
  img <- imager::load.image(path)
  class(img) <- c("stero", class(img))
  return(img)
}




#' Plot stero image
#'
#' @param stero object
#'
#' @return A ggplot object visualizing the loaded picture
#' @export
#'
#' @examples plot(img)
plot.stero <- function(x){
  df <- as.data.frame(x,wide="c")
  df$rgb.val <- grDevices::rgb(df[c("c.1", "c.2", "c.3")])
  p <- ggplot2::ggplot(df,ggplot2::aes(x,y,fill=rgb.val))+
    ggplot2::geom_raster()+
    ggplot2::scale_fill_identity()+
    ggplot2::scale_y_reverse()+
    ggplot2::theme_void()
  return(p)
}



#' Adds grid to plot (deprecated)
#'
#' @param x
#' @param n
#'
#' @return
#' @export
#'
#' @examples
add_grid <- function(x, n=10){
  x +
    ggplot2::geom_hline(yintercept = seq(0, max(x$data$y), length.out=n), color = "red")+
    ggplot2::geom_vline(xintercept = seq(0, max(x$data$x), length.out=n), color = "red")
}

file <- system.file("extdata", "sponge3.jpg", package = "Stereology")


img_to_table <- function(img){
  df <- as.data.frame(img,wide="c")
  df$rgb.val <- grDevices::rgb(df[c("c.1", "c.2", "c.3")])
  return(df)
}



make_grid <- function(img_table, n=10){

  # Extract max values
  mx <- max(img_table$x)
  my <- max(img_table$y)

  # Find interval size of grid
  interval <- mx %/% n

  # If the interval is larger than y dimension - warn!
  if (interval >= my) warning("picture's y dimension is too small for this grid. Choose finer grid.")

  # Find random start point (randomly selected from uniform distribution in lower left cell)
  x_start <- round(runif(n=1, min = 0, max = interval))
  y_start <- round(runif(n=1, min = 0, max = interval))

  # Find number of grid lines per dimension
  y_rep <- 1 + (my) %/% interval
  x_rep <- (mx) %/% interval
  y_intercepts <- ((1:(y_rep))*interval) - y_start
  x_intercepts <- ((1:(x_rep))*interval) - x_start

  segments <- data.frame(
    x    = c(x_intercepts,          rep(min(df$x), y_rep)),
    xend = c(x_intercepts,          rep(max(df$x), y_rep)),
    y    = c(rep(min(df$y), x_rep), y_intercepts),
    yend = c(rep(max(df$y), x_rep), y_intercepts)
  )

  return(segments)
}


make_grid_plot <- function(img_table, grid) {
  ggplot2::ggplot(img_table,ggplot2::aes(x,y,fill=rgb.val))+
    ggplot2::geom_raster()+
    ggplot2::scale_fill_identity()+
    ggplot2::geom_segment(data=grid, ggplot2::aes(x=x, xend=xend, y=y, yend=yend, fill=NULL))+
    ggplot2::scale_y_reverse()+
    ggplot2::theme_void()
}


get_dimension <- function(df, grid){
  if (nrow(df)%%2) return(df)
  dimension <- c()
  df$corrected_x <- df$x
  df$corrected_y <- df$y
  df$pair_id <- (1+(1:nrow(df))) %/% 2

  for (i in unique(df$pair_id)){
    tmp <- df[df$pair_id == i,]
    x_diff <- tmp$x[1] - tmp$x[2]

    y_diff <- tmp$y[1] - tmp$y[2]
    direction <- ifelse(abs(x_diff) > abs(y_diff), "x", "y")

    x_intercepts <- grid$x[grid$x!=1]
    y_intercepts <- grid$y[grid$y!=1]


    if (direction=="x") {
      correct_y <- y_intercepts[which.min(abs(mean(tmp$y)-y_intercepts))]
      df[df$pair_id==i, "corrected_y"] <- rep(correct_y, 2)
    } else {
      correct_x <- x_intercepts[which.min(abs(mean(tmp$x)-x_intercepts))]
      df[df$pair_id==i, "corrected_x"] <- rep(correct_x, 2)
    }


    dimension <- c(dimension, rep(direction, 2))
  }
  df$dimension <- dimension


  # Corrections are saved:
  df$x <- df$corrected_x
  df$y <- df$corrected_y
  df <- df[,c("x", "y", "dimension")]
  return(df)
  }

# get_dimension(responses, grid)


# Debug
if (F) {
  img  <- load_img(file)
  df   <- img_to_table(img)
  grid <- make_grid(df)

  make_grid_plot(df, grid)
}






