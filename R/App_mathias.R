#'Launch point estimator app
#'
#' @description Running this function launches an interactive app that allows
#'  you to upload an image, plot points on it, select relevant points and then
#'  estimate properties based on that. Once the app is open press the "Update"
#'  button to plot the image and points. If no image is provided, a default image will be
#'  plotted. Please be aware that it takes about 30 seconds for the image to
#'  load. Click on the points that appear in the regions, whose area/volume you
#'  want to estimate. In the default picturethat would be all the pores of the
#'  sponge. When you have pressed all relevant points click on "estimate" button
#'  to get the result. \cr
#'  Depends on the following packages: \cr
#'  shiny, shinythemes,plotly,ggplot2,dplyr,readr
#'
#'
#'
#' @import shiny
#' @import shinythemes
#' @importFrom plotly plotlyOutput add_image renderPlotly add_markers event_data
#' @import ggplot2
#' @importFrom imager load.image
#' @import dplyr
#'
#' @return Launches app
#'
#'
#' @export
#'
#'
#'
#'



point_estimator_app <- function(){

# Create app to estimate properties from P_p


ui <- shiny::fluidPage(
  shiny::titlePanel("Point estimator"),
  theme=shinythemes::shinytheme("darkly"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(

  # add fileinput
  shiny::fileInput("image","Opload image",accept=c(".jpg",".png")),
  #textInput("path","Insert path to image"),

  # Add slider to choose size of points
  shiny::sliderInput("size","Select size of points",min=1,max=10,value=5),

  # Add select input to choose color of points
  shiny::selectInput('color', 'Select color of points', choices = c("Blue", "Red","Black","White")),

  # Add transparency option
  #sliderInput("alpha","Select transparency",min=0,max=1,value=1),

  # Add slider inputs to select number of points
  shiny::sliderInput('ny_points', 'Select number of rows', min = 1, max = 20,value=7),
  shiny::sliderInput('nx_points', 'Select number of coloums', min = 1, max = 20,value=7),

  # Add actionbutton to update plot
  shiny::actionButton("run", "Update"),


  # Add outputs

  # Text box output
  shiny::verbatimTextOutput("click"),

  # Add text with selected points
  shiny::textOutput("npoints"),

  # Add actionbutton to calculate estimate
  shiny::actionButton("estimate", "Estimate"),

  # Add text with estimate
  shiny::textOutput("P_p"),

  # Reset count
  #shiny::actionButton("reset","Reset count")

  # Add actionbutton to remove currently selected point
  # actionButton("undo","Unselect point")

  ),


  shiny::mainPanel(shiny::tabsetPanel(

    shiny::tabPanel("Image",plotly::plotlyOutput("plot",width = "600px",height="600px")),

    shiny::tabPanel("Points",shiny::tableOutput("table"))

  )
  )


))

server <- function(input, output, session){
  plotly_img_grid <-function(){
  # loads image
  #img =EBImage::readImage("C:/Users/mathi/Desktop/Advanced R/Sterology/inst/extdata/sponge3.jpg")

  # Create grid of points
  # y_values <- seq(0.1*dim(img)[1],dim(img)[1]-0.1*dim(img)[1],length.out=input$nx_points)
  # x_values <- seq(0.1*dim(img)[2],dim(img)[2]-0.1*dim(img)[2],length.out=input$ny_points)
  # xy_grid <- tidyr::expand_grid(x=x_values,y=y_values)

  # plotly_img <- plot_ly(type="image", z=img*255)
  #
  #  plotly_img %>%
  #   add_markers(x = xy_grid$x,y = xy_grid$y,marker = list(size = 7, symbol = "dot"),inherit=FALSE,color=I(input$color),opacity=0.5)
}

  # Assign plot to output

  #Load image
  img <- shiny::eventReactive(input$run,{

    default <-system.file("extdata", "smallsponge.jpg", package = "Stereology")

    path_df <- input$image # dataframe with input. col "datapath" contains path to file


    import <- if(!is.data.frame(input$image)){imager::load.image(system.file("extdata", "smallsponge.jpg", package = "Stereology"))
    } else {imager::load.image(path_df$datapath)}

    as.raster(import)


  })

  # Create grid of points

  grid <- shiny::reactive({
    y_values <- seq(0.1*dim(img())[1],dim(img())[1]-0.1*dim(img())[1],length.out=input$nx_points)
    x_values <- seq(0.1*dim(img())[2],dim(img())[2]-0.1*dim(img())[2],length.out=input$ny_points)
    xy_grid <- tidyr::expand_grid(x=x_values,y=y_values)
    xy_grid

  })

  # Create plotly object of image

  plotly_img <- shiny::reactive({plotly::add_image(plot_ly(type="image"),z=img())})


  output$plot<-plotly::renderPlotly({

    # Create plotly object of image and add points to it
    shiny::isolate(plotly_img()) %>%
      plotly::add_markers(x = grid()$x,y = grid()$y,marker = list(size = input$size, symbol = "dot"),inherit=FALSE,color=I(input$color),showlegend=FALSE)


  })

  point <- shiny::reactive({plotly::event_data("plotly_click")
  })




  # Add click events
  output$click <- shiny::renderPrint({
    #d <- event_data("plotly_click")
    if (is.null(point()[])) "Selected points appear here" else point()[2:4]
  })


  df<-  matrix(0,nrow = 1,ncol = 3) ## matrix with all selected points. first col is point nr, second i x coordinate and thirs is y coord.

  # Create reactive function to save information from click events in matrix and return total number of points selected

table_points <- shiny::reactive({

  points_row <- matrix(point()[2:4],nrow = 1,ncol = 3)

  df <<-  rbind(df,points_row)

  n_points <- nrow(df)-1

  n_points

})

# Create output to show number of points selected

output$npoints <- shiny::renderText({


  if (is.null(point()[])) paste("0 points selected") else  paste("Number of points selected:",table_points())

  })


 # Calculate estimate from number of selected points and total points

estimate <- shiny::eventReactive(input$estimate,{

  x_points <- input$nx_points

  y_points <- input$ny_points

  n_points <- table_points()


  round(n_points/(x_points*y_points),4) ######## fraction of selected points of total points

})


output$P_p <- shiny::renderText({

  k1 <- estimate()

  paste("Point based estimate:",k1)
})

## Create table with all data about all selected points in seperate tab

output$table <- shiny::bindEvent(shiny::renderTable({

  table <- as.data.frame(df)
  colnames(table) <- c("Point_ID","x","y")
  table[-1,]


  }),

  point()
)





## Function to remove selected points

# remove_point <- eventReactive(input$undo,{
#
#   last_row <- nrow(df)
#
#   df <<- df[-last_row,]
#
#
# })



}


shiny::shinyApp(ui = ui, server = server)

}

## EXTRA FEATURES THAT CAN BE ADDED
# Create actionbutton to remove currently selected point
# reduce filesize
# make function to reset number of points and dataframe



#plotly_example("shiny","event_data")

