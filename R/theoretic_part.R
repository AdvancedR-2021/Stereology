#' @title Theoretic estimations.
#' @description A function that performs simple linear regression and returns the: intercept, slope, variance, correlation, degrees of freedom as well as a linear regression plot. For more details use: browseVignettes(package = "simplelinreg")
#' @param image_path the path where the image is stored.
#' @param mtr a matrix generated using the function 'mat.gen()'.
#' @param x the index of the pixel-row to subset the 'mtr'.
#' @param x the index of the pixel-column to subset the 'mtr'.
#' @param d the length used to subset the 'mtr'.
#' @param trial_s amount of repetitions for the simulation.
#' @import imager
#' @example man/examples.R
#' @export

# Turn image to 0 (white)/ 1 (black) matrix


mat.gen <- function(image_path ) {
  im <- load.image(image_path)
  g.im <- grayscale(im)
  t.gim <- g.im >.5
  mat <- t.gim %>% as.matrix()
  mat[mat==TRUE] = 1
  return(invisible(mat))
}

# List of mc estimates depending on part of the matrix.

stats <- function (mtr, x, y, d, trial_s) {
  pmat <- mtr[x:(x+d), y:(y+d)]
  est <- mean(pmat)
  sample_s <- (d+1)^2
  sims <- matrix(rbinom(trial_s*sample_s, size=1, prob = est),
                 nrow = trial_s,
                 ncol = sample_s)
  p.hat <- rowSums(sims)/sample_s
  avg_ph <- mean(p.hat)
  v.ph <- var(p.hat)
  r_list <- list(matrix_part = pmat,
                 p = est,
                 mean_p.hat = avg_ph,
                 variance_p.hat = v.ph)
  return(r_list)

}
