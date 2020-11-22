
library(shiny)
library(DT)
library(lubridate)
library(ggplot2)
library(RColorBrewer)

shinyUI(fluidPage(
    
    titlePanel("Dashboard Academatica"),
    tabsetPanel(tabPanel('Tablas',
                         h3('Tabla de Datos 1'),
                         hr(),
                         fluidRow(column(width = 12, DT::dataTableOutput('tabla1'))),
                         
                         h3('Tabla de Datos 2'),
                         hr(),
                         fluidRow(column(width = 12, DT::dataTableOutput('tabla2'))),
                         
                         h3('Tabla de Datos 3'),
                         hr(),
                         fluidRow(column(width = 12, DT::dataTableOutput('tabla3')))
    ),
    tabPanel('Metadatos',
             h3('Metadatos del Video'),
             hr(),
             selectizeInput('Select_Input1',
                            'Escoge un ID de video:',
                            choices = data1$videoID,
                            selected = '',
                            multiple = FALSE),
             fluidRow(column(width = 12, tableOutput('tabla4')))
    ),
    tabPanel('Estadisticas',
             h3('Estadisticas de los Videos'),
             hr(),
             radioButtons('Radio_Button1',
                          'Escoge una estadistica:',
                          choices = c('Views', 'Likes', 'Dislikes', 'Favorites', 'Comments', 'General'),
                          selected = ' '),
             tabsetPanel(id = 'panel', tabPanel('Views',
                                                h3('Grafico Views'),
                                                hr(),
                                                plotOutput('grafica1')
             ),
             tabPanel('Likes',
                      h3('Grafico Likes'),
                      hr(),
                      plotOutput('grafica2')
             ),
             tabPanel('Dislikes',
                      h3('Grafico Dislikes'),
                      hr(),
                      plotOutput('grafica3')
             ),
             tabPanel('Favorites',
                      h3('Grafico Favorites'),
                      hr(),
                      plotOutput('grafica4')
             ),
             tabPanel('Comments',
                      h3('Grafico Comments'),
                      hr(),
                      plotOutput('grafica5')
             ),
             tabPanel('General',
                      h3('Grafico General de Estadisticas'),
                      hr(),
                      selectizeInput('Select_Input2',
                                     'Escoge un ID de video:',
                                     choices = data1$videoID,
                                     selected = ' ',
                                     multiple = FALSE),
                      plotOutput('grafica6'),
                      verbatimTextOutput('texto2')
             )
             )
    ),
    tabPanel('Fechas',
             h3('Informacion por fecha'),
             hr(),
             dateInput('Date_Input1',
                       'Selecciona una fecha:',
                       value = today(),
                       language = 'es',
                       weekstart = 1),
             fluidRow(column(width = 12, tableOutput('tabla5'))),
             verbatimTextOutput('texto1')
    )
    ),
    
)
)
