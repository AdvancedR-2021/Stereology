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
  mat[mat == FALSE] <- 2
  mat[mat == 1] <- 0
  mat[mat == 2] <- 1
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
#'   1) point_mean: the mean of black pixels for the point grid method, and \cr
#'   2) line_mean: the mean of black pixels for the line grid method.
#'   3) point_var: the variance of black pixels for the point grid method.
#'   4) v_lines_var: the variance of black pixels using only the vertical lines of the grid.
#'   5) h_lines_var: the variance of black pixels using only the horizontal lines of the grid.
#'   6) sum_lines_var: the sum of the variance of of both horizontal and vertical lines of the grid. There is dependence between "vertical variance" and "horizontal variance", so this output is only useful for counterexamples.
#'
#' @export

# List of theoretical estimates.

th_i_ests <- function (mtr, x, y, lx, ly) {
  if (!is(mtr, "bw_img") ) stop("The matrix needs be generated using the bw_img() function.")
  if (lx > nrow(mtr) || lx <= 0) stop("Incorrect amount of horizontal indices.")
  if (ly > ncol(mtr) || ly <= 0) stop("Incorrect amount of vertical indices.")

  dx <- floor(nrow(mtr)/lx)
  dy <- floor(ncol(mtr)/ly)

  # point estimator
  p_mat <- mtr[seq(x, nrow(mtr), dx), seq(y, ncol(mtr), dy)]
  p_mean <- sum(rowSums(p_mat))/(nrow(p_mat)*ncol(p_mat))

  # line estimator
  lmh <- mtr[seq(x, nrow(mtr), dx), ]
  lmv <- mtr[, seq(y, ncol(mtr), dy)]
  s_h <- rowSums(lmh)
  s_v <- colSums(lmv)
  l_mean <- (sum(s_h)+sum(s_v))/((nrow(lmh)*ncol(lmh))+(nrow(lmv)*ncol(lmv)))


  #vectors
  s <- 1
  pnt_v <- vector("numeric", dx*dy)
  ln_v <- vector("numeric", dx*dy)
  ln_h <- vector("numeric", dx*dy)


  #loop
  for (i in x:(x+dx)) {
    for (j in y:(y+dy)) {
      #point
      p_m <- mtr[seq(i, nrow(mtr), dx), seq(j, ncol(mtr), dy)]
      pnt_v[s] <- sum(rowSums(p_m))/(nrow(p_m)*ncol(p_m))

      #line
      ## horizontal
      vr_lmh <- mtr[seq(i, nrow(mtr), dx), ]
      ln_h[s] <- sum(rowSums(vr_lmh))/(nrow(vr_lmh)*ncol(vr_lmh))
      ## vertical
      vr_lmv <- mtr[, seq(j, ncol(mtr), dy)]
      ln_v[s] <- sum(rowSums(vr_lmv))/(nrow(vr_lmv)*ncol(vr_lmv))

      #adjust index
      s <- s+1

    }
  }

  #variance
  pnt_var <- var(pnt_v)
  lv_var <- var(ln_v)
  lh_var <- var(ln_h)
  comb_var <- lv_var + lh_var

  # list of outputs
  r_list <- list(point_mean = p_mean,
                 line_mean = l_mean,
                 point_var = pnt_var,
                 v_lines_var = lv_var,
                 h_lines_var = lh_var,
                 sum_lines_var = comb_var)

  return(r_list)

}

