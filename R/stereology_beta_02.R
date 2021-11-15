

#' Write ster data
#'
#' @param data from spateology curation
#' @param ...
#'
#' @return a csv file saved in working folder
#' @export
#'
#' @examples None - to be used in run_spat function

write_data <- function(data, ...) {

  # Should be able to make filename specific saves
  write.csv(responses, file = "stereology_positions.csv") #paste0(gsub(".*", "", path), "_spateology_positions.csv"))
}





#' Update data frame
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
save_data <- function(data, grid = NULL) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- get_dimension(rbind(responses, data), grid)
  } else {
    responses <<- data
  }
}





#' Retrieves curated data
#'
#' @return a dataframe of x y positions
#' @export
#'
#' @examples
load_data <- function() {
  if (exists("responses")) {
    responses
  }
}


#' Porosity estimator
#'
#' @param path
#'
#' @return Data table with locations of crossings
#' @export
#'
#' @examples run_spat()
#'
ster <- function(
  object = NULL,
  path = system.file("extdata", "sponge3.jpg", package = "Stereology"),
  seed = 1
){

  set.seed(seed)
  if (is.null(object)) {
    object <- load_img(path)
    object <- img_to_table(object)
    grid   <- make_grid(object)
  } else {
    grid <- make_grid(object)
  }


  p <- make_grid_plot(object, grid)


  shiny::shinyApp(

    ui <-
      shiny::fluidPage(

        shiny::plotOutput("plot", click = "click"),
#
#         DT::dataTableOutput("responses", width = 300), shiny::tags$hr(),

        shiny::actionButton("save", "save")

        ),

    server <- function(input, output, session) {

      pointer_data <- shiny::reactive({
        data <- c("x"=round(input$click$x, 1), "y"=round(input$click$y, 1), "dimension"=NA, "pair_id" = NA)
        data
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

      # Show the previous responses
      # (update with current response when Submit is clicked)
      # output$responses <- DT::renderDataTable({
      #   input$click
      #   load_data()
      # })

      }
  )
}

#ster()
