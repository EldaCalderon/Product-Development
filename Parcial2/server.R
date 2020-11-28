library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

#data1 <- read.csv('IHMStefanini_industrial_safety_and_health_database.csv')


shinyServer(function(input, output, session) {
    
    # Carga de Archivo
    archivo_carga <- reactive({
        if(is.null(input$upload_file)){
            return(NULL)
        }
        ext <- strsplit(input$upload_file$name, split = '[.]')[[1]][2]
        if(ext == 'csv'){
            file_data <- read.csv(input$upload_file$datapath)
            return(file_data)
        }
        if(ext == 'tsv'){
            file_data <- read.tsv(input$upload_file$datapath)
            return(file_data)
        }
        return(NULL)
    })
    
    # Tabla de datos
    output$contenido_archivo <- DT::renderDataTable({
        archivo_carga() %>% DT::datatable(filter = 'bottom')
    })
    
    
        
    # Parametros URL
    observe({
        query <- parseQueryString(session$clientData$url_search)
        Sex <- query[['Sex']]
        Country <- query[['Country']]
        Industry <- query[['Industry']]
        #browser()
        if(!is.null(Sex)){
            updateSelectInput(session, 'Select_Input1', selected = Sex)
        }
        if(!is.null(Country)){
            updateSelectInput(session, 'Radio_Button1', selected = Country)
        }
        if(!is.null(Industry)){
            updateSelectInput(session, 'Select_Input2', selected = Industry)
        }
        
    })
    
    
    
    
    observe({
        Select_Input1 <- input$Select_Input1
        Radio_Button1 <- input$Radio_Button1
        Select_Input2 <- input$Select_Input2
        link_url <- paste0('http://', session$clientData$url_hostname, ':', 
                           session$clientData$url_port, 
                           session$clientData$url_pathname,
                           '?Sex=', Select_Input1, '&',
                           'Country=', Radio_Button1, '&',
                           'Industry=', Select_Input2)
        updateTextInput(session, 'url', value = link_url)
    })
    
    
    observeEvent(input$upload_file,{
        data1 <- read.csv(input$upload_file$datapath)
        
    
    
        # Dataset
        #output$tabla1 <- DT::renderDataTable({
         #   data1 %>% 
          #      datatable()
        #})
    
        # Grafico 1-1
        output$plot1 <- renderPlot({
            cuenta_paises <- data1 %>%
                count('Country' = factor(Countries))
            
            grafica1 <- ggplot(cuenta_paises, aes(x = Country, y = n, fill = Country)) + 
                geom_bar(stat="identity", width=0.25) +
                geom_text(aes(label=n), vjust=1.6, color="white", size=4) +
                xlab("Country") + ylab("Amount") + 
                ggtitle("Accidents Amount x Country") +
                scale_fill_brewer(palette="Set1") + 
                theme(axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())
            
            grafica1
        })
        
        # Grafico 1-2
        output$plot2 <- renderPlot({
            cuenta_industria <- data1 %>%
                count('Industry' = factor(Industry.Sector))
            
            grafica2 <- ggplot(cuenta_industria, aes(x = '', y = n, fill = Industry)) + 
                geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) +
                geom_text(aes(y = c(390, 100, 20), label=n), vjust=1.6, color="white", size=4) +
                ggtitle("Accidents Amount x Industry Sector") + 
                scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
                theme_void()
            
            grafica2
        })
        
        # Grafico 2-1
        output$plot3 <- renderPlot({
            grafica3 <- ggplot(data1, aes(x = Accident.Level, fill = Potential.Accident.Level)) + 
                geom_bar(position = 'dodge') + 
                xlab("Level") + ylab("Amount") + 
                ggtitle("Accidents Amount x Level") +
                scale_fill_brewer(palette="Spectral")
            
            grafica3
                
        })
        
        # Grafico 2-2
        output$plot4 <- renderPlot({
            cuenta_sexo <- data1 %>%
                count('Genre' = factor(Genre))
            
            grafica4 <- ggplot(cuenta_sexo, aes(x = '', y = n, fill = Genre)) + 
                geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) +
                geom_text(aes(y = c(430,100), label=n), vjust=1.6, color="white", size=4) +
                ggtitle("Accidents Amount x Genre") + 
                theme_void()
            
            grafica4
        })
        
        # Grafico 3-1
        output$plot5 <- renderPlot({
            cuenta_et <- data1 %>%
                count('Employee_Tercero' = factor(Employee.ou.Terceiro))
            
            grafica5 <- ggplot(cuenta_et, aes(x = '', y = n, fill = Employee_Tercero)) + 
                geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) +
                geom_text(aes(y = c(350,150,30), label=n), vjust=1.6, color="black", size=4) +
                ggtitle("Accidents Amount x Worker Class") + 
                scale_fill_brewer(palette="Accent") +
                theme_void()
            
            grafica5
        })
        
        # Grafico 3-2
        output$plot6 <- renderPlot({
            cuenta_risco <- data1 %>%
                count('Risco' = factor(Risco.Critico))
            
            grafica6 <- ggplot(cuenta_risco, aes(x = Risco, y = n, fill = Risco)) + 
                geom_bar(stat="identity") +
                geom_text(aes(label=n), vjust=1, color="black", size=2) +
                xlab("Risco") + ylab("Amount") + 
                ggtitle("Accidents Amount x Risco") +
                theme(axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())+ 
                theme(legend.position = "bottom")
            
            grafica6
        })
        
        
        
        # TAB INTERACTIVO
        
        # Graficos Filtro Sexo
        lista_eventos <- reactive({list(input$Select_Input1, input$Radio_Button1, input$Select_Input2)})
    
        observeEvent(lista_eventos(), {
            selection1 <- input$Select_Input1
            selection2 <- input$Radio_Button1
            selection3 <- input$Select_Input2
            
            
            #filtroG <- c()    
            if (selection1 == 'Both' & selection2 != 'All countries' & selection3 != 'All sectors'){
                filtroG <- filter(data1, !is.null(Genre) & Countries == selection2 & Industry.Sector == selection3)
            } else if (selection1 != 'Both' & selection2 == 'All countries' & selection3 != 'All sectors'){
                filtroG <- filter(data1, Genre == selection1 & !is.null(Countries) & Industry.Sector == selection3)
            } else if (selection1 != 'Both' & selection2 != 'All countries' & selection3 == 'All sectors'){
                filtroG <- filter(data1, Genre == selection1 & Countries == selection2 & !is.null(Industry.Sector))
            } else if (selection1 == 'Both' & selection2 == 'All countries' & selection3 != 'All sectors'){
                filtroG <- filter(data1, !is.null(Genre) & !is.null(Countries) & Industry.Sector == selection3)
            } else if (selection1 == 'Both' & selection2 != 'All countries' & selection3 == 'All sectors'){
                filtroG <- filter(data1, !is.null(Genre) & Countries == selection2 & !is.null(Industry.Sector))
            } else if (selection1 != 'Both' & selection2 == 'All countries' & selection3 == 'All sectors'){
                filtroG <- filter(data1, Genre == selection1 & !is.null(Countries) & !is.null(Industry.Sector))
            } else if (selection1 == 'Both' & selection2 == 'All countries' & selection3 == 'All sectors'){
                filtroG <- filter(data1, !is.null(Genre) & !is.null(Countries) & !is.null(Industry.Sector))
            } else {
                filtroG <- filter(data1, Genre == selection1 & Countries == selection2 & Industry.Sector == selection3)
            }
            
            
                # Filtro 1-1
            cuenta_paises2 <- filtroG %>%
                group_by(Genre) %>%
                count('Country' = factor(Countries))
            #cuenta_paises2 <- filter(cuenta_paises2, Genre == selection1)
            
                # Filtro 1-2
            cuenta_industria2 <- filtroG %>%
                group_by(Genre) %>%
                count('Industry' = factor(Industry.Sector))
            #cuenta_industria2 <- filter(cuenta_industria2, Genre == selection1)
            
                # Filtro 3-1
            cuenta_et2 <- filtroG %>%
                group_by(Genre) %>%
                count('Employee_Tercero' = factor(Employee.ou.Terceiro))
            #cuenta_et2 <- filter(cuenta_et2, Genre == selection1)
            
                # Filtro 3-2
            cuenta_risco2 <- filtroG %>%
                group_by(Genre) %>%
                count('Risco' = factor(Risco.Critico))
            #cuenta_risco2 <- filter(cuenta_risco2, Genre == selection1)
            
            
            if (selection1 == 'Both' & selection2 == 'All countries' & selection3 == 'All sectors'){
                # Grafico 1-1
                output$plot7 <- renderPlot({
                    cuenta_paises <- data1 %>%
                        count('Country' = factor(Countries))
                    grafica1 <- ggplot(cuenta_paises, aes(x = Country, y = n, fill = Country)) + 
                        geom_bar(stat="identity", width=0.25) +
                        geom_text(aes(label=n), vjust=1.6, color="white", size=4) +
                        xlab("Country") + ylab("Amount") + 
                        ggtitle("Accidents Amount x Country") +
                        scale_fill_brewer(palette="Set1") + 
                        theme(axis.text.x=element_blank(),
                              axis.ticks.x=element_blank())
                    grafica1
                })
                
                # Grafico 1-2
                output$plot8 <- renderPlot({
                    cuenta_industria <- data1 %>%
                        count('Industry' = factor(Industry.Sector))
                    grafica2 <- ggplot(cuenta_industria, aes(x = '', y = n, fill = Industry)) + 
                        geom_bar(stat="identity", width=1, color="white") +
                        coord_polar("y", start=0) +
                        geom_text(aes(y = c(390, 100, 20), label=n), vjust=1.6, color="white", size=4) +
                        ggtitle("Accidents Amount x Industry Sector") + 
                        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
                        theme_void()
                    grafica2
                })
                
                output$tabla2 <- renderTable({
                    c()
                })
                
                # Grafico 3-1
                output$plot9 <- renderPlot({
                    cuenta_et <- data1 %>%
                        count('Employee_Tercero' = factor(Employee.ou.Terceiro))
                    grafica5 <- ggplot(cuenta_et, aes(x = '', y = n, fill = Employee_Tercero)) + 
                        geom_bar(stat="identity", width=1, color="white") +
                        coord_polar("y", start=0) +
                        geom_text(aes(y = c(350,150,30), label=n), vjust=1.6, color="black", size=4) +
                        ggtitle("Accidents Amount x Worker Class") + 
                        scale_fill_brewer(palette="Accent") +
                        theme_void()
                    grafica5
                })
                
                output$tabla3 <- renderTable({
                    c()
                })
                
                # Grafico 3-2
                output$plot10 <- renderPlot({
                    cuenta_risco <- data1 %>%
                        count('Risco' = factor(Risco.Critico))
                    grafica6 <- ggplot(cuenta_risco, aes(x = Risco, y = n, fill = Risco)) + 
                        geom_bar(stat="identity") +
                        geom_text(aes(label=n), vjust=1, color="black", size=2) +
                        xlab("Risco") + ylab("Amount") + 
                        ggtitle("Accidents Amount x Risco") +
                        theme(axis.text.x=element_blank(),
                              axis.ticks.x=element_blank())+ 
                        theme(legend.position = "bottom")
                    grafica6
                })
                
            } else {
                # Grafico 1-1
                output$plot7 <- renderPlot({
                    grafica7 <- ggplot(cuenta_paises2, aes(x = Country, y = n, fill = Country)) + 
                        geom_bar(stat="identity", width=0.25) +
                        geom_text(aes(label=n), vjust=1.6, color="white", size=4) +
                        xlab("Country") + ylab("Amount") + 
                        ggtitle("Accidents Amount x Country") +
                        scale_fill_brewer(palette="Set1") + 
                        theme(axis.text.x=element_blank(),
                              axis.ticks.x=element_blank())
                    grafica7
                })
                
                # Grafico 1-2
                output$plot8 <- renderPlot({
                    grafica8 <- ggplot(cuenta_industria2, aes(x = '', y = n, fill = Industry)) + 
                        geom_bar(stat="identity", width=1, color="white") +
                        coord_polar("y", start=0) +
                        #geom_text(aes(y = c(390, 100, 20), label=n), vjust=1.6, color="white", size=4) +
                        ggtitle("Accidents Amount x Industry Sector") + 
                        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
                        theme_void()
                    grafica8
                })
                
                output$tabla2 <- renderTable({
                    cuenta_industria2
                })
                
                # Grafico 3-1
                output$plot9 <- renderPlot({
                    grafica9 <- ggplot(cuenta_et2, aes(x = '', y = n, fill = Employee_Tercero)) + 
                        geom_bar(stat="identity", width=1, color="white") +
                        coord_polar("y", start=0) +
                        #geom_text(aes(y = c(350,150,30), label=n), vjust=1.6, color="black", size=4) +
                        ggtitle("Accidents Amount x Worker Class") + 
                        scale_fill_brewer(palette="Accent") +
                        theme_void()
                    grafica9
                })
                
                output$tabla3 <- renderTable({
                    cuenta_et2
                })
                
                # Grafico 3-2
                output$plot10 <- renderPlot({
                    grafica10 <- ggplot(cuenta_risco2, aes(x = Risco, y = n, fill = Risco)) + 
                        geom_bar(stat="identity") +
                        geom_text(aes(label=n), vjust=1, color="black", size=4) +
                        xlab("Risco") + ylab("Amount") + 
                        ggtitle("Accidents Amount x Risco") +
                        theme(axis.text.x=element_blank(),
                              axis.ticks.x=element_blank())+ 
                        theme(legend.position = "bottom")
                    grafica10
                })
                   
            }
                
            
        })
        
        
    })


})
