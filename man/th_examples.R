# Loading the image.

# To use a PNG/JPEG/BMP image stored on your computer try:
# path <- "~/images.png"
path <- system.file('extdata/sponge3.jpg',package='Stereology')

# Applying mat.gen().
mat <- mat.gen(path)

# Applying thr.i.ests()
est.list <- th.i.ests(mtr = mat, x = 10, y = 10, d= 10, trial_s = 1000)
est.list

# For more details use: browseVignettes(package = "Stereology").
