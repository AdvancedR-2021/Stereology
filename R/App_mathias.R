library(shiny);library(shinythemes);library(tidyverse)
library(plotly);library(EBImage);library(DT)


# Create app to estimate properties from P_p

ui <- fluidPage(
  titlePanel("Point estimator"),
  theme=shinythemes::shinytheme("darkly"),
  sidebarLayout(sidebarPanel(

  # add fileinput
  fileInput("image","Opload image",accept=c(".jpg",".png")),
  #textInput("path","Insert path to image"),

  # Add slider to choose size of points
  sliderInput("size","Select size of points",min=1,max=10,value=5),

  # Add select input to choose color of points
  selectInput('color', 'Select color of points', choices = c("Blue", "Red","Black","White")),

  # Add transparency option
  #sliderInput("alpha","Select transparency",min=0,max=1,value=1),

  # Add slider inputs to select number of points
  sliderInput('ny_points', 'Select number of rows', min = 1, max = 20,value=7),
  sliderInput('nx_points', 'Select number of coloums', min = 1, max = 20,value=7),

  # Add actionbutton to update plot
  actionButton("run", "Update"),


  # Add outputs

  # Text box output
  verbatimTextOutput("click"),

  # Add text with selected points
  textOutput("npoints"),

  # Add actionbutton to calculate estimate
  actionButton("estimate", "Estimate"),

  # Add text with estimate
  textOutput("P_p"),

  # Add actionbutton to remove currently selected point
  actionButton("undo","Unselect point")

  ),


  mainPanel(tabsetPanel(

  tabPanel("Image",plotlyOutput("plot",width = "600px",height="600px")),

  tabPanel("Points",tableOutput("table"))

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
  # xy_grid <- expand_grid(x=x_values,y=y_values)

  # plotly_img <- plot_ly(type="image", z=img*255)
  #
  #  plotly_img %>%
  #   add_markers(x = xy_grid$x,y = xy_grid$y,marker = list(size = 7, symbol = "dot"),inherit=FALSE,color=I(input$color),opacity=0.5)
}

  # Assign plot to output

  #Load image
  img <- reactive({

    default <-system.file("extdata", "sponge3.jpg", package = "Stereology")

    path_df <- input$image

    #ifelse(is.null(input$image), EBImage::readImage(system.file("extdata", "sponge3.jpg", package = "Stereology")),input$image)

    #EBImage::readImage("C:/Users/mathi/Desktop/Advanced R/Sterology/inst/extdata/sponge3.jpg")

    if(!is.data.frame(input$image)){EBImage::readImage(system.file("extdata", "sponge3.jpg", package = "Stereology"))
      } else {EBImage::readImage(path_df$datapath)}



  })

  # Create grid of points

  grid <- eventReactive(input$run,{
    y_values <- seq(0.1*dim(img())[1],dim(img())[1]-0.1*dim(img())[1],length.out=input$nx_points)
    x_values <- seq(0.1*dim(img())[2],dim(img())[2]-0.1*dim(img())[2],length.out=input$ny_points)
    xy_grid <- expand_grid(x=x_values,y=y_values)
    xy_grid

  })

  # Create plotly object of image

  plotly_img <- reactive({plot_ly(type="image",z=255*img())})


  output$plot<-plotly::renderPlotly({

    # Create plotly object of image and add points to it
     isolate(plotly_img()) %>%
      add_markers(x = grid()$x,y = grid()$y,marker = list(size = input$size, symbol = "dot"),inherit=FALSE,color=I(input$color),showlegend=FALSE)


  })

  point <- reactive({event_data("plotly_click")
  })




  # Add click events
  output$click <- renderPrint({
    #d <- event_data("plotly_click")
    if (is.null(point()[])) "Selected points appear here" else point()[2:4]
  })


  df<-  matrix(0,nrow = 1,ncol = 3) ## matrix with all selected points. first col is point nr, second i x coordinate and thirs is y coord.

  # Create reactive function to save information from click events in matrix and return total number of points selected

table_points <- reactive({

  points_row <- matrix(point()[2:4],nrow = 1,ncol = 3)

  df <<-  rbind(df,points_row)

  n_points <- nrow(df)-1

  n_points

})

# Create output to show number of points selected

output$npoints <- renderText({

  k <- table_points()
  paste("Number of points selected:",k)
  })


 # Calculate estimate from number of selected points and total points

estimate <- eventReactive(input$estimate,{

  x_points <- input$nx_points

  y_points <- input$ny_points

  n_points <- table_points()


  round(n_points/(x_points*y_points),4) ######## fraction of selected points of total points

})


output$P_p <- renderText({

  k1 <- estimate()

  paste("Point based estimate:",k1)
})

## Create table with all data about all selected points in seperate tab

output$table <- renderTable({

  table <- as.data.frame(df)
  colnames(table) <- c("Point_ID","x","y")
  table[-1,]


  })

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


shinyApp(ui = ui, server = server)



## EXTRA FEATURES THAT CAN BE ADDED
# Create actionbutton to remove currently selected point
# add shiny fileinput
# add full data frame showing selected points in seperate tab



#plotly_example("shiny","event_data")

