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
  df <- dplyr::mutate(df, rgb.val=rgb(c.1,c.2,c.3))
  p <- ggplot2::ggplot(df,ggplot2::aes(x,y,fill=rgb.val))+
    ggplot2::geom_raster()+
    ggplot2::scale_fill_identity()+
    ggplot2::scale_y_reverse()+
    ggplot2::theme_void()
  return(p)
}



#' Adds grid to plot
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
    ggplot2::geom_hline(yintercept = seq(0, max(p$data$y), length.out=n), color = "red")+
    ggplot2::geom_vline(xintercept = seq(0, max(p$data$x), length.out=n), color = "red")
}

