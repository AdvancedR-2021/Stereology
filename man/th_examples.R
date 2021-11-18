# Loading the image.

# To use a PNG/JPEG/BMP image stored on your computer try:
# path <- "~/images.png"
path <- system.file('extdata/sponge3.jpg',package='Stereology')

# Applying bw_mat().
a <- bw_mat(path, .5)

# Applying th_i_ests()
est_l <- th_i_ests(mtr = a$matrix, x = 400, y = 100, dx = 10, dy = 10,  trial_s = 1000)
est_l

# For more details use: browseVignettes(package = "Stereology").
