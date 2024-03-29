
load_img <- function(path) {
  img <- imager::load.image(path)
  class(img) <- c("stero", class(img))
  return(img)
}

img_to_table <- function(img, dimx = 300, dimy=300){
  img  <- imager::resize(img, dimx, dimy)
  df <- as.data.frame(img,wide="c")
  df$rgb.val <- grDevices::rgb(df[c("c.1", "c.2", "c.3")])
  return(df)
}

make_grid <- function(img_tabledata, n=10){
  # Extract max values
  mx <- max(img_tabledata$x)
  my <- max(img_tabledata$y)

  # Find interval size of grid
  interval <- mx %/% n

  # If the interval is larger than y dimension - warn!
  if (interval >= my) stop("picture's y dimension is too small for this grid. Choose finer grid.")

  # Find random start point (randomly selected from uniform distribution in lower left cell)
  x_start <- round(runif(n=1, min = 0, max = interval))
  y_start <- round(runif(n=1, min = 0, max = interval))

  # Find number of grid lines per dimension
  y_rep <- 1 + (my) %/% interval
  x_rep <- (mx) %/% interval
  y_intercepts <- ((1:(y_rep))*interval) - y_start
  x_intercepts <- ((1:(x_rep))*interval) - x_start

  segments <- data.frame(
    x    = c(x_intercepts,                     rep(min(img_tabledata$x), y_rep)),
    xend = c(x_intercepts,                     rep(max(img_tabledata$x), y_rep)),
    y    = c(rep(min(img_tabledata$y), x_rep), y_intercepts),
    yend = c(rep(max(img_tabledata$y), x_rep), y_intercepts)
  )

  return(segments)
}


make_grid_plot <- function(img_tabledata, grid) {
  ggplot2::ggplot(img_tabledata,ggplot2::aes(x,y,fill=rgb.val))+
    ggplot2::geom_raster()+
    ggplot2::scale_fill_identity()+
    ggplot2::geom_segment(data=grid, ggplot2::aes(x=x, xend=xend, y=y, yend=yend, fill=NULL))+
    ggplot2::scale_y_reverse()+
    ggplot2::theme_void()
}


get_dimension <- function(df, grid){
  # Just return df if pair not complete yet
  if (nrow(df)%%2) return(df)

  dimension <- c()
  df$corrected_x <- df$x
  df$corrected_y <- df$y
  df$pair_id <- (1+(1:nrow(df))) %/% 2

  for (i in unique(df$pair_id)){
    tmp <- df[df$pair_id == i,]
    x_diff <- tmp$x[1] - tmp$x[2]

    y_diff <- tmp$y[1] - tmp$y[2]
    direction <- ifelse(abs(x_diff) > abs(y_diff), "x", "y")

    x_intercepts <- grid$x[grid$x!=1]
    y_intercepts <- grid$y[grid$y!=1]


    if (direction=="x") {
      correct_y <- y_intercepts[which.min(abs(mean(tmp$y)-y_intercepts))]
      df[df$pair_id==i, "corrected_y"] <- rep(correct_y, 2)
    } else {
      correct_x <- x_intercepts[which.min(abs(mean(tmp$x)-x_intercepts))]
      df[df$pair_id==i, "corrected_x"] <- rep(correct_x, 2)
    }


    dimension <- c(dimension, rep(direction, 2))
  }
  df$dimension <- dimension


  # Corrections are saved:
  df$x <- df$corrected_x
  df$y <- df$corrected_y
  df <- df[,c("x", "y", "dimension", "pair_id")]
  return(df)
  }

find_dimensional_difference <- function(responses_one_dim) {
  if (responses_one_dim$dimension[1]=="x") {
    summ <-
      responses_one_dim %>%
      dplyr::group_by(y, pair_id) %>%
      dplyr::summarise(diff = max(x)-min(x)) %>%
      dplyr::ungroup()
    n <- dplyr::n_distinct(summ$y)
  } else {
    summ <-
      responses_one_dim %>%
      dplyr::group_by(x, pair_id) %>%
      dplyr::summarise(diff = max(y)-min(y)) %>%
      dplyr::ungroup()
    n <- dplyr::n_distinct(summ$x)
  }

  return(c("lines" = n, "difference"=sum(summ$diff)))
}


estimate_porosity <- function(responses, df){
  differences <- split(responses, responses$dimension)
  differences <- purrr::map(differences, find_dimensional_difference)
  differences <- dplyr::bind_rows(differences, .id = "dimension")

  if (! "x" %in% differences$dimension) {differences = dplyr::bind_rows(differences, list(dimension="x", lines = 0, difference = 0))}
  if (! "y" %in% differences$dimension) {differences = dplyr::bind_rows(differences, list(dimension="y", lines = 0, difference = 0))}

  differences$dimension_lengths <- c(max(df$x), max(df$y))

  estimate <- mean((differences$difference/differences$dimension_lengths)/differences$lines)
  return(round(estimate, 3))
}



se_prop <- function(proportion, pop_size){
  se <- sqrt(proportion*(1-proportion)/pop_size)
  return(round(se, 3))
}


# bootstrap_responses <- function(responses){
#
#   cohort <- unique(responses$pair_id)
#   resamp <- sample(cohort, size=length(cohort), replace = T)
#   pairs <- responses$pair_id
#   boot <- responses[,c("x", "y", "dimension")]
#
#
# }

if (F) {
  path = system.file("extdata", "sponge3.jpg", package = "Stereology")
  object <- load_img(path)
  df <- img_to_table(object)
  grid   <- make_grid(df)
  a = estimate_porosity(responses, df = object)
  se_prop(a, nrow(responses))

  plot_precision(responses, df)
}


