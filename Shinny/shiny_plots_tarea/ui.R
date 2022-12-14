library(shiny)
library(DT)



shinyUI(fluidPage(
  
  # Application title
  titlePanel("Graficas y mas"),
  
  # Sidebar with a slider input for number of bins
  shiny::tabsetPanel(
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