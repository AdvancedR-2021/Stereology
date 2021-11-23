#' @title Theoretic estimations.
#' @description 2 functions: 'bw_mat()' and 'th_i_est()', offering exploration of
#'   theoretical estimates of an image. For more details use: \cr
#'   browseVignettes(package = "Stereology")
#' @example man/th_examples.R

#' @name Theoretic
NULL
#> NULL



#' @rdname Theoretic
#' @param image_path The path where the image is stored.
#' @param thr The threshold used to turn the image in black and white. The default value is 0.5.
#' @importFrom imager load.image grayscale
#' @importFrom magrittr %>%
#' @return The function 'bw_mat()' returns a list containing: \cr
#'   1) the matrix that represents the black and white version of the image that
#'   was used as input, and \cr
#'   2) the plot of the black and white version of the image.
#' @export


# Turn image to 0 (white)/ 1 (black) matrix

bw_mat <- function(image_path, thr = .5) {
  im <- imager::load.image(image_path)
  g_im <- imager::grayscale(im)
  t_gim <- g_im >thr
  mat <- t_gim %>%
    as.matrix()
  mat[mat==TRUE] = 1
  class(mat) <- c("bw_img", "matrix", "array")
  im_l <- list(img_mat = mat,
               bw_img = plot(t_gim))
  return(im_l)
}



#' @rdname Theoretic
#' @param mtr A matrix generated using the function 'bw_mat()'.
#' @param x Numeric variable. The index of the pixel-row to start the grid.
#' @param y Numeric variable. The index of the pixel-column to start the grid.
#' @param lx Numeric variable. The amount of horizontal indices to be used.
#' @param ly Numeric variable. The amount of vertical indices to be used.
#'
#' @return The function 'th_i_est()' returns a list containing: \cr
#'   1) point_mean: the mean of black pixels for the point grid method, \cr
#'   2) point_variance: the variance of black pixels for the point grid method, \cr
#'   3) line_mean: the mean of black pixels for the line grid method, and \cr
#'   4) line_variance: the variance of black pixels for the line grid method.
#'
#' @export

# List of theoretical estimates.

th_i_ests <- function (mtr, x, y, lx, ly) {
  if (!is(mtr, "bw_img") ) stop("The matrix needs be generated using the bw_img() function.")
  if (lx > nrow(mtr) || lx <= 0) stop("Incorrect amount of horizontal indices.")
  if (ly > ncol(mtr) || ly <= 0) stop("Incorrect amount of vertical indices.")
  # point estimator
  p_mat <- mtr[seq(x, nrow(mtr), floor(nrow(mtr)/lx)), seq(y, ncol(mtr), floor(ncol(mtr)/ly))]
  p_mean <- mean(p_mat)
  pme <- (p_mat - p_mean)^2
  p_var <- pme/((nrow(p_mat)*ncol(p_mat))-1)
  # line estimator
  lmh <- mtr[seq(x, nrow(mtr), floor(nrow(mtr)/lx)), ]
  lmv <- mtr[, seq(y, ncol(mtr), floor(ncol(mtr)/ly))]
  s_h <- rowSums(lmh)
  s_v <- colSums(lmv)
  l_mean <- (sum(s_h)+sum(s_v))/((nrow(lmh)*ncol(lmh))+(nrow(lmv)*ncol(lmv)))
  ss_h <- (lmh - l_mean)^2
  ss_v <- (lmv - l_mean)^2
  l_var <- sum(rowSums(ss_h)) + sum(colSums(ss_v))/((nrow(ss_h)*ncol(ss_h))+(nrow(ss_v)*ncol(ss_v))-1)
  # list of outputs
  r_list <- list(point_mean = p_mean,
                 point_variance = p_var,
                 line_mean = l_mean,
                 line_variance = l_var
  )
  return(r_list)

}

