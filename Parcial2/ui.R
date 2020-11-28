library(shiny)

data1 <- read.csv('IHMStefanini_industrial_safety_and_health_database.csv')


shinyUI(fluidPage(
    
    titlePanel("Industrial Accident Causal Analysis"),
    tabsetPanel(
                tabPanel('Upload File', 
                         sidebarLayout(
                           sidebarPanel(
                             h2('Upload a file'),
                             fileInput('upload_file', 
                                       'Select a file', 
                                       buttonLabel = 'Load',
                                       accept = c('.csv', '.tsv'))
                           ),
                           mainPanel(
                             DT::dataTableOutput('contenido_archivo'))
                           )
                         ),
                tabPanel('General',
                         h3('General Dashboard'),
                         hr(),
                         fluidRow(column(width = 6, plotOutput('plot1')), column(width=6,plotOutput('plot2'))),
                         hr(),
                         fluidRow(column(width = 6, plotOutput('plot3')), column(width=6,plotOutput('plot4'))),
                         hr(),
                         fluidRow(column(width = 6, plotOutput('plot5')), column(width=6,plotOutput('plot6')))
                         ),
                tabPanel('Dinamic',
                         h3('Interactive Dashboard'),
                         sidebarLayout(sidebarPanel(
                             selectInput('Select_Input1',
                                         'Choose sex:',
                                         choices = c('Female', 'Male', 'Both'),
                                         selected = 'Female',
                                         multiple = FALSE
                                         ),
                             radioButtons('Radio_Button1',
                                          'Choose a country:',
                                          choices = c('Country_01', 'Country_02', 'Country_03', 'All countries'),
                                          selected = 'Country_01'
                                          ),
                             selectInput('Select_Input2',
                                         'Choose an industry sector:',
                                         choices = c(unique(data1$Industry.Sector), 'All sectors'),
                                         selected = 'Mining',
                                         multiple = FALSE
                                        ),
                             textInput('url', 'link', value = '')
                             ),
                  
                             
                             mainPanel(
                                 h2('Graphics'),
                                 fluidRow(column(width = 8, plotOutput('plot7'))),
                                 hr(),
                                 fluidRow(column(width = 8, plotOutput('plot8')), column(width = 4, tableOutput('tabla2'))),
                                 hr(),
                                 fluidRow(column(width = 8, plotOutput('plot9')), column(width = 4, tableOutput('tabla3'))),
                                 hr(),
                                 fluidRow(column(width = 12, plotOutput('plot10')))
                                 )
                                 
                             )
                         )
                )

    
))
