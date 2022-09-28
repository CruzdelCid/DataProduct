library(shiny)
library(DT)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("Laboratorio"),
  
  # Sidebar with a slider input for number of bins
  tabsetPanel(
    tabPanel("Plot Interactions",
             plotOutput("plot_click_options",
                        click = "clk",
                        dblclick = "dclk",
                        hover = 'mhover',
                        brush = 'mbrush' ),
             DT::dataTableOutput("mtcars_tbl")
    )
  )
))