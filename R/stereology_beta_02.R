
write_data <- function(data, ...) {

  # Should be able to make filename specific saves
  write.csv(responses, file = "stereology_positions.csv") #paste0(gsub(".*", "", path), "_spateology_positions.csv"))
}

save_data <- function(data, grid = NULL) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- get_dimension(rbind(responses, data), grid)
  } else {
    responses <<- data
  }
}


load_data <- function() {
  if (exists("responses")) {
    responses
  }
}


load_metric <- function() {
  if (exists("result")) {
    return(result)
  }
  return(NULL)
}


get_metric <- function(object) {
  responses <- load_data()
  if (is.null(responses)) {return(NULL)}
  if(is.null(responses)|nrow(responses)<2){
    result <<- NULL
  }else{
    if (nrow(responses)%%2 == 0){
      p = estimate_porosity(responses, object)
      se = se_prop(p, nrow(responses))
      ifelse(is.na(p),
             result <<- NULL,
             result <<- list("se" = se, "p"=p)
             )
    } else{
      result <<- NULL
    }
  }
}


#' Porosity estimator
#'
#' The function line_estimator_app() takes several arguments: object (default = NULL) is an image data frame in long format with columns x, y, and hexacode for color; path is the path to an image, and can be used instead of the object; grid_number decides the vertical lines (horisontal lines are separated by the same distance as the x-axis lines and might differ from the grid_number); and finally seed makes the random initialisation of the gridlines a bit less random.
#' Rules of the line estimator:
#' - One must always assign two points to one air bubble.
#' - If one misses a point, then assign a new point on top (it is not possible to remove points).
#' - Always make sure lines are completed before using the estimates.
#' - Alternate between completing horisontal and vertical lines to reduce potential bias and obtain a better estimate.
#'
#' @param path
#'
#' @return Data table with locations of crossings
#' @export
#'
#' @examples
#' line_estimator_app()

line_estimator_app <- function(
  object = NULL,
  path = system.file("extdata", "sponge3.jpg", package = "Stereology"),
  grid_number = 10,
  seed = 1
){

  set.seed(seed)
  if (is.null(object)) {
    object <- load_img(path)
    object <- img_to_table(object)
    grid   <- make_grid(object, n = grid_number)
  } else {
    grid <- make_grid(object, n = grid_number)
  }
  p <- make_grid_plot(object, grid)


  shiny::shinyApp(

    ui <-
      shiny::fluidPage(

        shiny::plotOutput("plot", click = "click"),
        shiny::actionButton("porosity", "Get Porosity Estimate"),
        shiny::textOutput("metric"),
        shiny::actionButton("save", "Save Data Points")

        ),

    server <- function(input, output, session) {

      pointer_data <- shiny::reactive({
        data <- c("x"=round(input$click$x, 1), "y"=round(input$click$y, 1), "dimension"=NA, "pair_id" = NA)
        data
      })

      output$metric <- shiny::renderText({
        input$porosity
        r <- load_metric()
        if (is.null(r)) {"Complete at least one vertical line and one horisontal line, OR complete the current pair of points"} else {
          paste("Porosity:", r$p, "\n","  SE:",r$se)
        }
      })

      # This function should respond to clicks and then: load responses, calculate dimension, correct non-used dimension, and plot corrected value as point.
      plot_data <- shiny::reactive({
        input$click
        responses <- load_data()
        if (!is.null(responses)){
          return(ggplot2::geom_point(data=responses, ggplot2::aes(x=x, y=y, fill=NULL)))
        }
        NULL
      })

      output$plot <- shiny::renderPlot({
        p+plot_data()
      })

      # When the Submit button is clicked, save the form data
      shiny::observeEvent(input$click, {
        save_data(pointer_data(), grid)
      })

      shiny::observeEvent(input$save, {
        write_data(load_data())
      })

      shiny::observeEvent(input$porosity, {
        get_metric(object)
      })


      }
  )
}

#line_estimator_app(grid_number = 20)

