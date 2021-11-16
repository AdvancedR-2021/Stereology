#' @title Theoretic estimations.
#' @description 2 functions ('mat_gen()' and 'th_i_est()') offering exploration
#'   of theoretical estimates of an image. For more details use:
#'   browseVignettes(package = "Stereology")
#' @param image_path The path where the image is stored.
#' @param mtr A matrix generated using the function 'mat.gen()'.
#' @param x Numeric variable. The index of the pixel-row to start subsetting the
#'    'mtr'.
#' @param y Numeric variable. The index of the pixel-column to start subsetting
#'   the 'mtr'.
#' @param d Numeric variable. the length of pixels used to subset the 'mtr'.
#' @param trial_s Numeric variable. The amount of repetitions used for the
#'   simulation.
#' @import imager
#' @example man/th_examples.R
#' @return The function 'mat_gen()' outputs the matrix that represents the image
#'   used as input.
#'   The function 'th_i_est()' a list containing:
#'   - part of the original matrix
#'   - the probability (p) estimated using this sub-matrix,
#'   - the average of the p_hats generated through simulations, and
#'   - the variance of those p_hats.

#' @name Theoretic
NULL
#> NULL



#' @rdname Theoretic
#' @export
# Turn image to 0 (white)/ 1 (black) matrix

mat_gen <- function(image_path ) {
  im <- load.image(image_path)
  g_im <- grayscale(im)
  t_gim <- g_im >.5
  mat <- t_gim %>%
    as.matrix()
  mat[mat==TRUE] = 1
  return(invisible(mat))
}



#' @rdname Theoretic
#' @export
# List of MC estimates depending on part of the matrix.

th_i_ests <- function (mtr, x, y, d, trial_s) {
  pmat <- mtr[x:(x+d), y:(y+d)]
  est <- mean(pmat)
  sample_s <- (d+1)^2
  sims <- matrix(rbinom(trial_s*sample_s, size=1, prob = est),
                 nrow = trial_s,
                 ncol = sample_s)
  p_hat <- rowSums(sims)/sample_s
  avg_ph <- mean(p_hat)
  v_ph <- var(p_hat)
  r_list <- list(matrix_part = pmat,
                 p = est,
                 mean_p_hat = avg_ph,
                 variance_p_hat = v_ph)
  return(r_list)

}
