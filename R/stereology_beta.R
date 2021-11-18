

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
  write.csv(responses, file = "spateology_positions.csv") #paste0(gsub(".*", "", path), "_spateology_positions.csv"))
}

#' Update data frame
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
save_data <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

save_plot <- function(p) {
  if (exists("plot")) {
    plot <<- plot + p
  } else {
    plot <<- p
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


#' Beta density estimator
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
  path = system.file("extdata", "sponge3.jpg", package = "Stereology")
){
  if (is.null(object)) {
    object <- load_img(path)
    p <- plot.stero(object)
    p <- add_grid(p)
  } else {
    p <- object
  }

  shiny::shinyApp(

    ui <- shiny::fluidPage(
      shiny::plotOutput("plot", click = "click"),
      DT::dataTableOutput("responses", width = 300), shiny::tags$hr(),
      shiny::actionButton("save", "save")),

    server <- function(input, output, session) {

      formData <- shiny::reactive({
        data <- c("x"=round(input$click$x, 1), "y"=round(input$click$y, 1)) #sapply(fields, function(x) input[[x]])
        data
        })

      output$plot <- shiny::renderPlot({
        p # The plot
      })

      # When the Submit button is clicked, save the form data
      shiny::observeEvent(input$click, {
        save_data(formData())
        })


      # NEED --> ADD points to plot such that one can see the progression.
      # shiny::observeEvent(input$click, {
      #   save_data(formData())
      # })

      shiny::observeEvent(input$save, {
        write_data(load_data())
      })

      # Show the previous responses
      # (update with current response when Submit is clicked)
      output$responses <- DT::renderDataTable({
        input$click
        load_data()
      })

      # shiny::observeEvent(
      #   input$click,
      #   {p+geom_point(responses, aes(x=x, y=y))})
      #save_plot(geom_point(data=responses, aes(x=x, y=y))))




      }
  )
}
#ster()
#responses

#runGitHub(repo = "JohanLassen/firenoodles")



