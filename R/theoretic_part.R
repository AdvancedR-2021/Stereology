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
#' @import imager
#' @return The function 'bw_mat()' returns a list containing: \cr
#'   1) the matrix that represents the black and white version of the image that
#'   was used as input, and \cr
#'   2) the plot of the black and white version of the image.
#' @export
# Turn image to 0 (white)/ 1 (black) matrix

bw_mat <- function(image_path, thr = .5) {
  im <- load.image(image_path)
  g_im <- grayscale(im)
  t_gim <- g_im >thr
  mat <- t_gim %>%
    as.matrix()
  mat[mat==TRUE] = 1
  im_l <- list(img_mat = mat,
               bw_img = plot(t_gim))
  return((im_l))
}



#' @rdname Theoretic
#' @param mtr A matrix generated using the function 'bw_mat()'.
#' @param x Numeric variable. The index of the pixel-row to start subsetting the
#'    'mtr'.
#' @param y Numeric variable. The index of the pixel-column to start subsetting
#'   the 'mtr'.
#' @param dx Numeric variable. The length of pixels used to subset the 'mtr' on the x-axis.
#' @param dy Numeric variable. The length of pixels used to subset the 'mtr' on the y-axis.
#' @param trial_s Numeric variable. The amount of repetitions used for the
#'   simulation.
#' @return The function 'th_i_est()' returns a list containing: \cr
#'   1) submatrix: part of the original matrix \cr
#'   2) p: the probability (p) estimated using this sub-matrix, \cr
#'   3) mean_p_hat: the average of the p_hats generated through simulations, and \cr
#'   4) variance_p_hat: the variance of those p_hats.
#' @export
# List of MC estimates depending on part of the matrix.

th_i_ests <- function (mtr, x, y, dx, dy, trial_s) {
  #if (mtr ) stop("Y should be numeric.")
  pmat <- mtr[x:(x+dx), y:(y+dy)]
  est <- mean(pmat)
  sample_s <- (dx+1)*(dy+1)
  sims <- matrix(rbinom(trial_s*sample_s, size=1, prob = est),
                 nrow = trial_s,
                 ncol = sample_s)
  p_hat <- rowSums(sims)/sample_s
  avg_ph <- mean(p_hat)
  v_ph <- var(p_hat)
  r_list <- list(submatrix = pmat,
                 p = est,
                 mean_p_hat = avg_ph,
                 variance_p_hat = v_ph)
  return(invisible(r_list))

}
