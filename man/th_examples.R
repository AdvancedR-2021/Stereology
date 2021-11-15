# loading the image.

# to use a PNG/JPEG/BMP image stored on your computer try:
# path <- "~/images.png"
path <- system.file('extdata/sponge3.jpg',package='Sterology')

# applying mat.gen().
mat <- mat.gen(path)
mat

# applying thr.i.ests()
est.list <- thr.i.ests(m, 80, 145, 5, 1000)
est.list
