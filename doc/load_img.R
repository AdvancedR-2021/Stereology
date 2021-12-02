## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Stereology)

## -----------------------------------------------------------------------------
# Loading the image.

# To use a PNG/JPEG/BMP image stored on your computer try:
# path <- "~/images.png"
path <- system.file('extdata/sponge3.jpg',package='Stereology')

# Applying bw_mat().
a <- bw_mat(path)
str(a)

# Applying th_i_ests().
est_a <- th_i_ests(mtr = a$img_mat, x = 400, y = 100, lx= 5, ly = 10)

# Print all different outputs.
est_a


## -----------------------------------------------------------------------------
# Loading the image.
path2 <- system.file('extdata/smallsponge.jpg',package='Stereology')

# Applying bw_mat().
b <- bw_mat(path2, 0.75)
d <- bw_mat(path2, 0.65)
e <- bw_mat(path2, 0.55)

# Image dimensions
dim(b$img_mat)

# Applying th_i_ests().
est_b <- th_i_ests(mtr = b$img_mat, x = 10, y = 20, lx= 5, ly = 5)
est_d <- th_i_ests(mtr = d$img_mat, x = 10, y = 20, lx= 5, ly = 5)
est_e <- th_i_ests(mtr = e$img_mat, x = 10, y = 20, lx= 5, ly = 5)

# Print just mean_p_hat and variance_p_hat.

tab <- matrix(c(est_b$point_mean, est_b$line_mean,
                est_d$point_mean, est_d$line_mean,
                est_e$point_mean, est_e$line_mean), 
              ncol=2, byrow=TRUE)
colnames(tab) <- c('Point Mean', 'Line Mean')
rownames(tab) <- c('b','d','e')
tab <- as.table(tab)
tab

# The only pore in "e".
e$img_mat[160:175, 255:267]

