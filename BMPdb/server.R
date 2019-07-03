#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggpubr)
shinyServer(function(input, output, session) {
    
    updateSelectizeInput(session, 'x',
                         choices = c("All", unique(Combined$Analysis_Category)), selected = "All", 
                         server = TRUE
    )
    updateSelectizeInput(session, 'y',
                         choices = c("All", unique(Combined$Group)), selected = "All", 
                         server = TRUE
    )
    updateSelectizeInput(session, 'color',
                         choices = c("All", unique(Combined$`WQX Parameter`)), selected = "All", 
                         server = TRUE)
    updateSelectizeInput(session, 'dev',
                         choices = c("All", unique(Combined$STATE)), selected = "All",
                         server = TRUE)

    output$mytable <- renderPlot({
        data <- WQ
       data2 <- subset(data, `Use in BMP Category Analysis` == "Yes" | `Use in BMP Category Analysis` == "yes")
        data <- subset(data2, `Monitoring Station Type` == "Inflow" | `Monitoring Station Type` == "Outflow")
        
        if (input$y != "All") {
            data <- data[data$Group %in% input$y,]
        }
        if (input$x != "All") {
            data <- data[data$Analysis_Category %in% input$x,]
        }
        if (input$color != "All") {
            data <- data[data$`WQX Parameter` %in% input$color,]
        }
        if (input$dev != "All") {
            data <- data[data$STATE %in% input$dev,]
        }
        data
        boxplot(`WQ Analysis Value` ~ `Monitoring Station Type`, data= data)
    })
    output$mywilcox <- renderPrint({
        datasub <- Combined
        
        if (input$y != "All") {
            datasub <- datasub[datasub$Group %in% input$y,]
        }
        if (input$x != "All") {
            datasub <- datasub[datasub$Analysis_Category %in% input$x,]
        }
        if (input$color != "All") {
            datasub <- datasub[datasub$`WQX Parameter` %in% input$color,]
        }
        if (input$dev != "All") {
            datasub <- datasub[datasub$STATE %in% input$dev,]
        }
        datasub
        t <- wilcox.test(datasub$`WQ Analysis Value.x`, datasub$`WQ Analysis Value.y`)
        print(t)
    })

})



t <- wilcox.test(Combined$`WQ Analysis Value.x`, Combined$`WQ Analysis Value.y`)
t
