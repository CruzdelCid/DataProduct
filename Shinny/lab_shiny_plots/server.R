
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)



# Preparación variables globales
cars <- mtcars[c('wt', 'mpg')]
cars$nombre <- row.names(cars)
row.names(cars) <- NULL

green_points <- data.frame(cars[1,])
green_points <- green_points[!TRUE,]

gray_points <- data.frame(cars[1,])
gray_points <- gray_points[!TRUE,]




shinyServer(function(input, output) {
  
  # Puntos verdes
  puntos <- reactive({
    # Brush
    df <- brushedPoints(cars,input$mbrush,xvar='wt',yvar='mpg')
    
    if (nrow(df) == 0) {
      # Click 
      df <- nearPoints(cars,input$clk,xvar='wt',yvar='mpg')
    }
    
    
    # Eliminar
    eliminar <- nearPoints(cars,input$dclk,xvar='wt',yvar='mpg', threshold = 3)
    
    if(nrow(eliminar)!=0){
      green_points <<- green_points %>% filter(nombre != eliminar$nombre)
    }
    
    green_points <<- rbind(green_points, df)
    green_points <<-  unique(green_points)
    
    return(green_points)

  })
  
  
  
  
  # Sombra gris 
  puntos_gris <- reactive({
    gris <- nearPoints(cars,input$mhover,xvar='wt',yvar='mpg', threshold = 3)
    mh <- input$mhover
    
    if (!is.null(mh)) {
      gray_points <<- gray_points[!TRUE,]
      
    }
    
    if(nrow(gris)!=0){
      gray_points <<- rbind(gray_points, gris)
    }
    
    return(gray_points)
  })
  
  
  
  
  # Plot
  output$plot_click_options <- renderPlot({
    plot(cars$wt,cars$mpg, xlab = "wt", ylab="millas por galon")
    puntos()
    
    df <- puntos()
    df2 <- puntos_gris()
    
    
    points(df2$wt,df2$mpg, col='gray', pch=19)
    points(df$wt,df$mpg, col='green', pch=19)
  })
  
  
  
  # Tabla
  output$mtcars_tbl <- DT::renderDataTable({
    puntos()
    
  })
  
})