library(shiny)
library(ggplot2)
library(dplyr)
library(DT)


mtcarst<- mtcars
mtcarst$nombres <- row.names(mtcarst)
print(mtcarst$nombres)

green_points <- data.frame(mtcarst[1,])
green_points <- green_points[!TRUE,]

gray_points <- data.frame(mtcarst[1,])
gray_points = gray_points[!TRUE,]




"df <- nearPoints(mtcars,input$clk,xvar='wt',yvar='mpg')
    
    if(nrow(df)!=0){

      df
      
    } else {
      # Si no existe nada el clk mostrara la seleccion 
      
      df <- brushedPoints(mtcars,input$mbrush,xvar='wt',yvar='mpg')
      
      if(nrow(df)!=0){
        df
        
      } else {
        
        NULL
        
      }
      
    } 
    
    
  
"




"
  puntos_gris <- reactive({
    gris <- nearPoints(mtcarst,input$mhover,xvar='wt',yvar='mpg', threshold = 3)
    mh <- input$mhover
    
    if (!is.null(mh)) {
      gray_points <<- gray_points[!TRUE,]
      
    }
    
    if(nrow(gris)!=0){
      gray_points <<- rbind(gray_points, gris)
    }
    
    return(gray_points)
  })
  
  "

#gray_points <- data.frame(mtcarst[1,])[c('wt', 'mpg', 'nombres')]
#gray_points <- gray_points[!TRUE,]

# points(df2$wt, df2$mpg, col='gray', pch=19)

# df2 <- puntos_gris()



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


