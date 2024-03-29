# Loading the image.

# To use a PNG/JPEG/BMP image stored on your computer try:
# path <- "~/images.png"
path <- system.file('extdata/sponge3.jpg',package='Stereology')

# Applying bw_mat().
a <- bw_mat(path)

# Applying th_i_ests()
est_a <- th_i_ests(mtr = a$img_mat, x = 700, y = 500, lx= 18, ly = 12)
est_a

# For more details use: browseVignettes(package = "Stereology").
