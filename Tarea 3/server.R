
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

shinyServer(function(input, output) {
    
    output$plot_click_options <- renderPlot({
        ggplot(mtcars, mapping = aes(wt, mpg, color='red'))+
            geom_point(mtcars, mapping = aes(wt, mpg), color='red')+
            labs(x = 'peso del vehiculo', y = 'millas por galon')+
            theme_classic()+
            ylim(5, 40) 
    })
    
    
    output$click_data <- renderPrint({
        if(!is.null(input$clk$x)){
            clk_msg <- paste0('click coordenada x= ', round(input$clk$x, 2),
                              'click coordenada y= ', round(input$clk$y, 2))
            
            data1 = data.frame('wt1' = input$clk$x, 'mpg1' = input$clk$y)
            output$plot_click_options <- renderPlot({
                ggplot(mtcars, mapping = aes(wt, mpg))+
                    geom_point(mtcars, mapping = aes(wt, mpg), color='red')+
                    labs(x = 'peso del vehiculo', y = 'millas por galon')+
                    geom_point(data1, mapping = aes(wt1, mpg1), color='green')+
                    theme_classic()+
                    ylim(5, 40)
            })
            
            #print(clk_msg)
        }
      
        
        if(!is.null(input$dclk$x)){
            dclk_msg <- paste0('doble click coordenada x= ', round(input$dclk$x, 2),
                              'doble click coordenada y= ', round(input$dclk$y, 2))
            #print(dclk_msg)
            
            data1 = data.frame('wt1' = input$dclk$x, 'mpg1' = input$dclk$y)
            output$plot_click_options <- renderPlot({
                ggplot(mtcars, mapping = aes(wt, mpg))+
                    geom_point(mtcars, mapping = aes(wt, mpg), color='red')+
                    labs(x = 'peso del vehiculo', y = 'millas por galon')+
                    geom_point(data1, mapping = aes(wt1, mpg1), color='white')+
                    theme_classic()+
                    ylim(5, 40)
            })
        }
        
        if(!is.null(input$mhover$x)){
            mhover_msg <- paste0('hover click coordenada x= ', round(input$mhover$x, 2),
                               'hover click coordenada y= ', round(input$mhover$y, 2))
            #print(mhover_msg) #hover es donde se posiciona el mouse
            
            data1 = data.frame('wt1' = input$mhover$x, 'mpg1' = input$mhover$y)
            output$plot_click_options <- renderPlot({
                ggplot(mtcars, mapping = aes(wt, mpg))+
                    geom_point(mtcars, mapping = aes(wt, mpg), color='red')+
                    labs(x = 'peso del vehiculo', y = 'millas por galon')+
                    geom_point(data1, mapping = aes(wt1, mpg1), color='gray')+
                    theme_classic()+
                    ylim(5, 40)
            })
        }
        
        if(!is.null(input$mbrush$xmin)){
            brushx <- paste0('(', input$mbrush$xmin, ',', input$mbrush$xmax, ')')
            brushy <- paste0('(', input$mbrush$ymin, ',', input$mbrush$ymax, ')')
            mbrush <- cat('rango en x:', brushx, '\n',
                          'rango en y:', brushy, '\n') #cat es similar a paste pero tiene menos conversion
            cat(mbrush) #brush en seleccionar una region
            
            mtcars_df <- cbind(carname = row.names(mtcars), mtcars)
            df <- brushedPoints(mtcars_df, input$mbrush, xvar = 'wt', yvar = 'mpg')
            output$plot_click_options <- renderPlot({
                ggplot(mtcars, mapping = aes(wt, mpg))+
                    geom_point(mtcars, mapping = aes(wt, mpg), color='red')+
                    labs(x = 'peso del vehiculo', y = 'millas por galon')+
                    geom_point(df, mapping = aes(wt, mpg), color='blue')+
                    theme_classic()+
                    ylim(5, 40)
            })
        }
        
    })
    
    
    output$tabla1 <- DT::renderDataTable({
      df <- brushedPoints(mtcars, input$mbrush, xvar = 'wt', yvar = 'mpg')
      data1 = data.frame('wt1' = input$clk$x, 'mpg1' = input$clk$y)
      if(nrow(df) != 0){
        df %>% 
          datatable() 
      } else if (nrow(data1) != 0){
        data1 %>% 
          datatable() 
      } else {
        mtcars %>% 
          datatable() 
    }
      })
    
    #output$tabla2 <- DT::renderDataTable({
     # data1 = data.frame('wt1' = input$clk$x, 'mpg1' = input$clk$y)
      #if(nrow(data1) != 0){
       # data1 %>% 
        #  datatable() 
      #} else {
      #  NULL
      #}
    #})
    
})    
