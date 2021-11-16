# Loading the image.

# To use a PNG/JPEG/BMP image stored on your computer try:
# path <- "~/images.png"
path <- system.file('extdata/sponge3.jpg',package='Stereology')

# Applying mat_gen().
mat <- mat_gen(path)

# Applying th_i_ests()
est_list <- th_i_ests(mtr = mat, x = 10, y = 10, d= 10, trial_s = 1000)
est_list

# For more details use: browseVignettes(package = "Stereology").
