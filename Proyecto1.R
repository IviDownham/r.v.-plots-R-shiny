library(shiny)
library(shinythemes)
library(actuar)
library(ggplot2)
library(qqplotr)
library(moments)

Dist = c('Beta', 'Chi-Cuadrada', 'Burr', 
         'Exponencial', 'Gamma', 'T',
         'Paralogistica', 'Normal', 'Logistica',
         'Pareto', 'F')

ui = fluidPage(
    navbarPage(
        theme = shinytheme("superhero"),
        "shinythemes",
        tabPanel("Simulación de V.A.'s",
                 sidebarPanel(
                     selectInput(
                         inputId = 'dist', 'Seleccione la distribución', choices = Dist,
                         selectize = FALSE
                     ),
                     sliderInput(inputId = "n", "Tamaño de muestra:", 1, 1000, 300),
                     sliderInput(inputId = "p1", "Parámetro 1:", 1, 100, 30),
                     sliderInput(inputId = "p2", "Parámetro 2:", 1, 100, 30),
                     uiOutput('ex1'),
                     uiOutput('ex2'),
                     uiOutput('ex3'),
                     uiOutput('ex4'),
                     uiOutput('ex5'),
                     uiOutput('ex6'),
                     uiOutput('ex7'),
                     uiOutput('ex8'),
                     uiOutput('ex9')
                ),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Histograma y nube de puntos",
                                  h4("Histograma de frecuencias"),
                                  plotOutput("hi"),
                                  h4("Nube de puntos"),
                                  plotOutput("nu")
                                  ),
                         tabPanel("Grafico de caja",
                                  h4("Grafico de caja"),
                                  plotOutput("ca")),
                         tabPanel("QQ y PP plots",
                                  h4("QQ-Plot"),
                                  plotOutput("qq"),
                                  h4("PP-Plot"),
                                  plotOutput("pp")),
                         tabPanel("Empírica vs Teórica",
                                  h4("F original vs F simulada"),
                                  plotOutput("FF"),
                                  h4("Histograma vs densidad original"),
                                  plotOutput("hif"))
                     )
                 )
        )
    )
)
server = function(input, output, session) {
    
###############################################################################
    
    x <- reactive({
            if(input$dist == 'Beta'){
                rbeta(input$n, input$p1, input$p2)
            }else if(input$dist == 'Chi-Cuadrada'){
                rchisq(input$n, input$p1)      # Un parametro
            }else if(input$dist == 'Burr'){
                rburr(input$n, input$p1, input$p2)
            }else if(input$dist == 'Exponencial'){
                rexp(input$n, input$p1)        # Un parametro
            }else if(input$dist == 'Gamma'){
                rgamma(input$n, input$p1, input$p2)
            }else if(input$dist == 'T'){
                rt(input$n, input$p1)          # Un parametro
            }else if(input$dist == 'Paralogistica'){
                rparalogis(input$n, input$p1)  # Un parametro
            }else if(input$dist == 'Normal'){
                rnorm(input$n, input$p1, input$p2)
            }else if(input$dist == 'Logistica'){
                rlogis(input$n, input$p1, input$p2)
            }else if(input$dist == 'Pareto'){
                rpareto(input$n, input$p1, input$p2)
            }else if(input$dist == 'F'){
                rf(input$n, input$p1, input$p2)
            }
        })
    
############################################################################### 
    
    y <- reactive({data.frame('a'= 1:length(x()),'x'= x())})
    
    output$hi<-renderPlot({
    ggplot(data = y(), aes(x = x)) + 
        geom_histogram(aes(y = ..density..), fill = '#2B3E50', bins = 30, alpha = 0.7) +
        labs(x = "Simulación de datos", y = "Frecuencia", 
             title = 'Histograma')
    })
    
    output$nu<-renderPlot({
    ggplot(data = y(), aes(x = x, y = a)) + 
            geom_point(alpha = 0.7, color = '#4E5D6C') +
        labs(x = "Simulación de datos", y = NULL, title = 'Plot')
    })
    
    output$ca<-renderPlot({
    ggplot(data = y(), aes(x = x)) + geom_boxplot(fill = '#2B3E50', alpha = 0.8) +
        labs(x = "Simulación de datos", y = NULL, title = 'Box-plot')
    })
    
    output$qq<-renderPlot({
    ggplot(data = y(), mapping = aes(sample = x)) +
        stat_qq_band(colour = '#4E5D6C', alpha = 0.3) +
        stat_qq_line(colour = '#2B3E50') +
        stat_qq_point(colour = '#2B3E50', alpha = 0.7) +
        labs(x = "Simulación de datos", y = NULL, title = 'QQ-plot')
    })
    
    output$pp<-renderPlot({
    ggplot(data = y(), mapping = aes(sample = x)) +
        stat_pp_band(colour = '#4E5D6C', alpha = 0.3) +
        stat_pp_line(colour = '#2B3E50') +
        stat_pp_point(colour = '#2B3E50', alpha = 0.3) +
        labs(x = "Simulación de datos", y = NULL, title = 'PP-plot')
    })
    
    # Requieren especificar la distribucion
    
    
    
    output$hif<-renderPlot({
        
###############################################################################
        
    if(input$dist == 'Beta'){
        ggplot(data = y(), aes(x=x)) + 
            geom_density(colour = '#4E5D6C', fill = "#4E5D6C", alpha = 0.5) +
            stat_function(fun = dbeta, args = list(input$p1, input$p2), color = '#2B3E50') +
            labs(x = "Simulación de datos", y = "Frecuencia", title = 'Densidad ')
    }else if(input$dist == 'Chi-Cuadrada'){
        ggplot(data = y(), aes(x = x)) + 
            geom_density(colour = '#4E5D6C', fill = "#4E5D6C", alpha = 0.5) +
            stat_function(fun = dchisq, args = list(input$p1), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = "Frecuencia", title = 'Densidad')
    }else if(input$dist == 'Burr'){
        ggplot(data = y(), aes(x = x)) + 
            geom_density(colour = '#4E5D6C', fill = "#4E5D6C", alpha = 0.5) +
            stat_function(fun = dburr, args = list(input$p1, input$p2), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = "Frecuencia", title = 'Densidad')
    }else if(input$dist == 'Exponencial'){
        ggplot(data = y(), aes(x = x)) + 
            geom_density(colour = '#4E5D6C', fill = "#4E5D6C", alpha = 0.5) +
            stat_function(fun = dexp, args = list(input$p1), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = "Frecuencia", title = 'Densidad')
    }else if(input$dist == 'Gamma'){
        ggplot(data = y(), aes(x = x)) + 
            geom_density(colour = '#4E5D6C', fill = "#4E5D6C", alpha = 0.5) +
            stat_function(fun = dgamma, args = list(input$p1, input$p2), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = "Frecuencia", title = 'Densidad')
    }else if(input$dist == 'T'){
        ggplot(data = y(), aes(x = x)) + 
            geom_density(colour = '#4E5D6C', fill = "#4E5D6C", alpha = 0.5) +
            stat_function(fun = dt, args = list(input$p1), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = "Frecuencia", title = 'Densidad')
    }else if(input$dist == 'Paralogistica'){
        ggplot(data = y(), aes(x = x)) + 
            geom_density(colour = '#4E5D6C', fill = "#4E5D6C", alpha = 0.5) +
            stat_function(fun = dparalogis, args = list(input$p1), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = "Frecuencia", title = 'Densidad')
    }else if(input$dist == 'Normal'){
        ggplot(data = y(), aes(x = x)) + 
            geom_density(colour = '#4E5D6C', fill = "#4E5D6C", alpha = 0.5) +
            stat_function(fun = dnorm, args = list(input$p1, input$p2), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = "Frecuencia", title = 'Densidad')
    }else if(input$dist == 'Logistica'){
        ggplot(data = y(), aes(x = x)) + 
            geom_density(colour = '#4E5D6C', fill = "#4E5D6C", alpha = 0.5) +
            stat_function(fun = dlogis, args = list(input$p1, input$p2), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = "Frecuencia", title = 'Densidad')
    }else if(input$dist == 'Pareto'){
        ggplot(data = y(), aes(x = x)) + 
            geom_density(colour = '#4E5D6C', fill = "#4E5D6C", alpha = 0.5) +
            stat_function(fun = dpareto, args = list(input$p1, input$p2), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = "Frecuencia", title = 'Densidad')
    }else if(input$dist == 'F'){
        ggplot(data = y(), aes(x = x)) + 
            geom_density(colour = '#4E5D6C', fill = "#4E5D6C", alpha = 0.5) +
            stat_function(fun = df, args = list(input$p1, input$p2), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = "Frecuencia", title = 'Densidad')
    }
    
###############################################################################
    
    })    
        
    output$FF<-renderPlot({
    if(input$dist == 'Beta'){
        ggplot(data = y(), aes(x = x)) + stat_ecdf(geom = "step", colour = '#2B3E50') +
            stat_function(fun = pbeta, args = list(input$p1, input$p2), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = NULL, title = 'Distribución')
    }else if(input$dist == 'Chi-Cuadrada'){
        ggplot(data = y(), aes(x = x)) + stat_ecdf(geom = "step", colour = '#2B3E50') +
            stat_function(fun = pchisq, args = list(input$p1), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = NULL, title = 'Distribución')
    }else if(input$dist == 'Burr'){
        ggplot(data = y(), aes(x = x)) + stat_ecdf(geom = "step", colour = '#2B3E50') +
            stat_function(fun = pburr, args = list(input$p1, input$p2), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = NULL, title = 'Distribución')
    }else if(input$dist == 'Exponencial'){
        ggplot(data = y(), aes(x = x)) + stat_ecdf(geom = "step", colour = '#2B3E50') +
            stat_function(fun = pexp, args = list(input$p1), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = NULL, title = 'Distribución')
    }else if(input$dist == 'Gamma'){
        ggplot(data = y(), aes(x = x)) + stat_ecdf(geom = "step", colour = '#2B3E50') +
            stat_function(fun = pgamma, args = list(input$p1, input$p2), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = NULL, title = 'Distribución')
    }else if(input$dist == 'T'){
        ggplot(data = y(), aes(x = x)) + stat_ecdf(geom = "step", colour = '#2B3E50') +
            stat_function(fun = pt, args = list(input$p1), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = NULL, title = 'Distribución')
    }else if(input$dist == 'Paralogistica'){
        ggplot(data = y(), aes(x = x)) + stat_ecdf(geom = "step", colour = '#2B3E50') +
            stat_function(fun = pparalogis, args = list(input$p1), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = NULL, title = 'Distribución')
    }else if(input$dist == 'Normal'){
        ggplot(data = y(), aes(x = x)) + stat_ecdf(geom = "step", colour = '#2B3E50') +
            stat_function(fun = pnorm, args = list(input$p1, input$p2), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = NULL, title = 'Distribución')
    }else if(input$dist == 'Logistica'){
        ggplot(data = y(), aes(x = x)) + stat_ecdf(geom = "step", colour = '#2B3E50') +
            stat_function(fun = plogis, args = list(input$p1, input$p2), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = NULL, title = 'Distribución')
    }else if(input$dist == 'Pareto'){
        ggplot(data = y(), aes(x = x)) + stat_ecdf(geom = "step", colour = '#2B3E50') +
            stat_function(fun = ppareto, args = list(input$p1, input$p2), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = NULL, title = 'Distribución')
    }else if(input$dist == 'F'){
        ggplot(data = y(), aes(x = x)) + stat_ecdf(geom = "step", colour = '#2B3E50') +
            stat_function(fun = pf, args = list(input$p1, input$p2), color = '#4E5D6C') +
            labs(x = "Simulación de datos", y = NULL, title = 'Distribución')
    }

###############################################################################
        
    })
    
    output$ex1 <- renderUI({
        xbar <- round(mean(x()), 4)
        withMathJax(sprintf("La media muestral es: $$\\bar{X} = %.03f$$", xbar))
    })
    
    output$ex2 <- renderUI({
        SD <- round(sd(x()), 4)
        withMathJax(sprintf("La desviación estandar muestral es: $$\\sigma = %.03f$$", SD))
    })
    
    output$ex3 <- renderUI({
        Q0 <- round(quantile(x(), 0), 4)
        withMathJax(sprintf("Los cuartiles son los siguientes: $$Q_{0} = %.03f$$", Q0))
    })
    
    output$ex4 <- renderUI({
        Q1 <- round(quantile(x(), 0.25), 4)
        withMathJax(sprintf("$$Q_{0.25} = %.03f$$", Q1))
    })
    
    output$ex5 <- renderUI({
        Q2 <- round(quantile(x(), 0.5), 4)
        withMathJax(sprintf("$$Q_{0.5} = %.03f$$", Q2))
    })
    
    output$ex6 <- renderUI({
        Q3 <- round(quantile(x(), 0.75), 4)
        withMathJax(sprintf("$$Q_{0.75} = %.03f$$", Q3))
    })
    
    output$ex7 <- renderUI({
        Q4 <- round(quantile(x(), 1), 4)
        withMathJax(sprintf("$$Q_{1} = %.03f$$", Q4))
    })
    
    output$ex8 <- renderUI({
        SK <- round(skewness(x()), 4)
        withMathJax(sprintf("La asimetría es: $$\\gamma_1 = %.03f$$", SK))
    })
    
    output$ex9 <- renderUI({
        KU <- round(kurtosis(x()), 4)
        withMathJax(sprintf("La curtosis es: $$\\beta_2 = %.03f$$", KU))
    })
    
}

shinyApp(ui=ui,server=server)
