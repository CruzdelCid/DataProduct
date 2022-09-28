library(shiny)
library(ggplot2)
library(dplyr)
library(DT)


mtcarst<- mtcars
mtcarst$nombres <- row.names(mtcarst)
 
green_points <- data.frame(mtcarst[1,])
green_points <- green_points[!TRUE,]


add_green <- function(df) {
  # print(df)
  
  if(nrow(df)!=0){
    doble <- green_points %>% filter(nombres != df$nombres)

    if(nrow(doble) == 0) {
      green_points <<- rbind(green_points, df)
      
    } else {
      
    }
  }
}



shinyServer(function(input, output) {
  
  
  # Click
  output$click_data <- renderPrint({
    clk_msg <- NULL
    dclk_msg<- NULL
    mhover_msg <- NULL
    mbrush_msg <- NULL

    if(!is.null(input$clk$x) ){
      clk_msg<-
        paste0("click cordenada x= ", round(input$clk$x,2), 
               " click coordenada y= ", round(input$clk$y,2))
    }
    
    if(!is.null(input$dclk$x) ){
      dclk_msg<-paste0("doble click cordenada x= ", round(input$dclk$x,2), 
                       " doble click coordenada y= ", round(input$dclk$y,2))
    }
    if(!is.null(input$mhover$x) ){
      mhover_msg<-paste0("hover cordenada x= ", round(input$mhover$x,2), 
                         " hover coordenada y= ", round(input$mhover$y,2))
      
    }
    
    
    if(!is.null(input$mbrush$xmin)){
      brushx <- paste0(c('(',round(input$mbrush$xmin,2),',',round(input$mbrush$xmax,2),')'),collapse = '')
      brushy <- paste0(c('(',round(input$mbrush$ymin,2),',',round(input$mbrush$ymax,2),')'),collapse = '')
      mbrush_msg <- cat('\t rango en x: ', brushx,'\n','\t rango en y: ', brushy)
    }
    
    cat(clk_msg,dclk_msg,mhover_msg,mbrush_msg,sep = '\n')
    
  })
  
  
  # Tabla
  output$mtcars_tbl <- renderTable({
    puntos()[c('wt', 'mpg', 'nombres')]
    
  })
  
  
  
  #### puntos verdes 
  puntos <- reactive({

    verde <- nearPoints(mtcarst,input$clk,xvar='wt',yvar='mpg', threshold = 3)
    add_green(verde)
    
    verde <- brushedPoints(mtcarst,input$mbrush,xvar='wt',yvar='mpg')
    add_green(verde)
  
    eliminar <- nearPoints(mtcarst,input$dclk,xvar='wt',yvar='mpg', threshold = 3)
    
    if(nrow(eliminar)!=0){
      green_points <<- green_points %>% filter(nombres != eliminar$nombres)
    }
    
    print(green_points)
    return(green_points)
    
  }) 
  
  
  # punto gris
  
  
  
  
  #### Grafica 
  output$plot_click_options <- renderPlot({
    plot(mtcars$wt,mtcars$mpg, xlab = "wt", ylab="millas por galon")

    browser()
    df <- puntos()


    points(df$wt,df$mpg, col='green', pch=19)
  })
  
})

