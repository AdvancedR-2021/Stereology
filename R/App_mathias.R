library(shiny);library(shinythemes);library(tidyverse)
library(plotly);library(EBImage);library(DT)


# Create app to estimate properties from P_p

ui <- fluidPage(
  titlePanel("Point estimator"),
  theme=shinythemes::shinytheme("darkly"),
  sidebarLayout(sidebarPanel(

  # Add slider to choose size of points
  sliderInput("size","Select size of points",min=1,max=10,value=5),

  # Add select input to choose color of points
  selectInput('color', 'Select color of points', choices = c("Blue", "Red","Black","White")),

  # Add transparency option
  sliderInput("alpha","Select transparency",min=0,max=1,value=1),

  # Add slider inputs to select number of points
  sliderInput('ny_points', 'Select number of rows', min = 1, max = 20,value=10),
  sliderInput('nx_points', 'Select number of coloums', min = 1, max = 20,value=10),

  # Add actionbutton to update plot
  actionButton("run", "Initiate plot"),

  # Add outputs

  # Text box output
  verbatimTextOutput("click"),

  # Add dataframe with selected points
  textOutput("npoints"),

  # Add actionbutton to calculate estimate
  actionButton("estimate", "Estimate"),

  # Add estimate
  textOutput("P_p")

  ),


  mainPanel(plotlyOutput("plot",width = "100%",height="100%"))


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

    EBImage::readImage("C:/Users/mathi/Desktop/Advanced R/Sterology/inst/extdata/sponge3.jpg")
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
      add_markers(x = grid()$x,y = grid()$y,marker = list(size = input$size, symbol = "dot"),inherit=FALSE,color=I(input$color),opacity=input$alpha)


  })

  point <- reactive({event_data("plotly_click")
  })




  # Add click events
  output$click <- renderPrint({
    #d <- event_data("plotly_click")
    if (is.null(point()[])) "Selected points appear here" else point()[2:4]
  })


  df<-  matrix(0,nrow = 1,ncol = 3) # initiate data frame. first col is point nr, second i x coordinate and thirs is y coord.

  # Create reactive function to save information from click events in data frame

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


 #P_p <- table_points()[1]/(x_points*y_points)

  n_points/(x_points*y_points) ######## Check format of this

})


output$P_p <- renderText({

  k1 <- estimate()

  paste("Point based estimate:",k1)
})





}


shinyApp(ui = ui, server = server)



## EXTRA FEATURES THAT CAN BE ADDED
# Create actionbutton to remove currently selected point
# Create eventreactive inputs
# use event_data(event="plotly_click")
# try plotly.restyle() or plotly.reac() to make updating plot faster
# use partial_bundle to reduce filesize
# add shiny fileinput
# add full data frame showing selected points in seperate tab



plotly_example("shiny","event_data")
