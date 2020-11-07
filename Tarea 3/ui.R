
library(shiny)
library(DT)

shinyUI(fluidPage(

    # Application title
    titlePanel("Graficas en Shiny - Laboratorio 2"),
    
    tabsetPanel(
        tabPanel('Plot user Interactions', 
                 plotOutput('plot_click_options',
                            click = 'clk',
                            dblclick = 'dclk',
                            hover = 'mhover',
                            brush = 'mbrush'),
                 verbatimTextOutput('click_data'),
                 DT::dataTableOutput('tabla1'),
                 DT::dataTableOutput('tabla2')
                 )
    )
))
