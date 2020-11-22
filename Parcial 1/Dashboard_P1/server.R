
library(shiny)
library(DT)
library(ggplot2)
library(RColorBrewer)
library(RMySQL)
library(DBI)
library(dplyr)
library(data.table)
library(mltools)
library(caret)
library(stringr)

# Conexion a MySQL
drv <- dbDriver("MySQL")
datos <- dbConnect(drv, user='root', password='root', host='88721f964988', dbname='parcial1')

# Creacion de Tablas
data1 <- dbGetQuery(datos, statement = ('select * from tabla1'))
data2 <- dbGetQuery(datos, statement = ('select * from tabla2'))
data3 <- dbGetQuery(datos, statement = ('select * from tabla3'))


shinyServer(function(input, output, session) {
    
    
    # TAB 1
    output$tabla1 <- DT::renderDataTable({
        data1 %>% 
            datatable()
    })
    
    output$tabla2 <- DT::renderDataTable({
        data2 %>% 
            datatable()
    })
    
    output$tabla3 <- DT::renderDataTable({
        data3 %>% 
            datatable()
    })
    
    
    # TAB 2
    
    output$tabla4 <- renderTable({
        if (!is.null(input$Select_Input1)){
            data3[data3$videoID == input$Select_Input1,]
        }
    })
    
    
    # TAB 3
    
    #Graficas
    output$grafica1 <- renderPlot({
        ggplot(data = data1, aes(x = videoID, y = viewCount)) +
            geom_point(color = 'green') + 
            xlab("ID") + ylab("Cantidad de Views") + 
            ggtitle("Cantidad de Vistas x Video")
    })
    
    
    output$grafica2 <- renderPlot({
        ggplot(data = data1, aes(x = videoID, y = likeCount)) +
            geom_point(color = 'blue') + 
            xlab("ID") + ylab("Cantidad de Likes") + 
            ggtitle("Cantidad de Me Gusta x Video")
    })
    
    
    output$grafica3 <- renderPlot({
        ggplot(data = data1, aes(x = videoID, y = dislikeCount)) +
            geom_point(color = 'red') + 
            xlab("ID") + ylab("Cantidad de Dislikes") + 
            ggtitle("Cantidad de No Me Gusta x Video")
    })
    
    
    output$grafica4 <- renderPlot({
        ggplot(data = data1, aes(x = videoID, y = favoriteCount)) +
            geom_point(color = 'orange') + 
            xlab("ID") + ylab("Cantidad de Favoritos") + 
            ggtitle("Cantidad de Marcado como Favorito x Video")
    })
    
    
    output$grafica5 <- renderPlot({
        ggplot(data = data1, aes(x = videoID, y = commentCount)) +
            geom_point(color = 'purple') + 
            xlab("ID") + ylab("Cantidad de Comentarios") + 
            ggtitle("Cantidad de Comentarios x Video")
    })
    
    output$texto2 <- renderPrint({
        print(paste0('Nombre del video: ', filter(data3, input$Select_Input2 == data3$videoID)[1,2]))
    })
    
    output$grafica6 <- renderPlot({
        barplot(height = matrix(filter(data1, data1$videoID == input$Select_Input2)[-1], nrow=1), 
                names = colnames(data1)[-1], main = "Estadistica General del Video",
                xlab = "Metrica",
                ylab = "Cantidad",
                col = brewer.pal(5, "Spectral"),
                width = 0.10)
    })
    
    
    
    
    # Paneles
    hideTab("panel", "Views")
    hideTab("panel", "Likes")
    hideTab("panel", "Dislikes")
    hideTab("panel", "Favorites")
    hideTab("panel", "Comments")
    hideTab("panel", "General")
    
    observeEvent(input$Radio_Button1, {
        if(input$Radio_Button1 == 'Views'){
            hideTab("panel", "Likes")
            hideTab("panel", "Dislikes")
            hideTab("panel", "Favorites")
            hideTab("panel", "Comments")
            showTab('panel', 'Views')
            hideTab("panel", "General")
        }   else if (input$Radio_Button1 == 'Likes'){
            hideTab("panel", "Views")
            hideTab("panel", "Dislikes")
            hideTab("panel", "Favorites")
            hideTab("panel", "Comments")
            showTab('panel', 'Likes')
            hideTab("panel", "General")
        }   else if (input$Radio_Button1 == 'Dislikes'){
            hideTab("panel", "Views")
            hideTab("panel", "Likes")
            hideTab("panel", "Favorites")
            hideTab("panel", "Comments")
            showTab('panel', 'Dislikes')
            hideTab("panel", "General")
        }   else if (input$Radio_Button1 == 'Favorites'){
            hideTab("panel", "Views")
            hideTab("panel", "Likes")
            hideTab("panel", "Dislikes")
            hideTab("panel", "Comments")
            showTab('panel', 'Favorites')
            hideTab("panel", "General")
        }   else if (input$Radio_Button1 == 'Comments'){
            hideTab("panel", "Views")
            hideTab("panel", "Likes")
            hideTab("panel", "Dislikes")
            hideTab("panel", "Favorites")
            showTab('panel', 'Comments')
            hideTab("panel", "General")
        }   else if (input$Radio_Button1 == 'General'){
            hideTab("panel", "Views")
            hideTab("panel", "Likes")
            hideTab("panel", "Dislikes")
            hideTab("panel", "Favorites")
            hideTab('panel', 'Comments')
            showTab("panel", "General")
        }
    })
    
    
    
    # TAB 4
    
    observeEvent(input$Date_Input1, {
        filtro <- filter(data2, toString(input$Date_Input1) == substr(data2$details, 1, 10))
        if(nrow(filtro != 0)){
            output$texto1 <- renderPrint({
                print(paste0('Hay ', nrow(filtro), ' videos publicados en la fecha seleccionada.'))
            })
            output$tabla5 <- renderTable({
                filtro2 <- filter(data3, filtro$contentVideoID == data3$videoID)
                tabla <- cbind(filtro, title = filtro2$title, link = filtro2$link, deparse.level = 1)
                tabla[, (4:7)]
            })
        } else {
            output$texto1 <- renderPrint({
                print('No hay videos publicados en esta fecha')
            })
            output$tabla5 <- renderTable({
                c()
            })
        }
    })
    
})
