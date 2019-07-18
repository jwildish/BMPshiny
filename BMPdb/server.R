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
library(dplyr)
library(tidyverse)
library(scales)
shinyServer(function(input, output, session) {

    updateCheckboxGroupInput(session, 'Type',
                         choices = unique(Combined$Analysis_Category), selected = "Detention Basin"
    )
  updateSelectizeInput(session, 'Category',
                         choices = c("All", unique(Combined$Group)), selected = "Metals", 
                         server = TRUE)
  observeEvent(input$Category,{
      updateSelectizeInput(session, 'Metric',
                         choices = c("All", unique(Combined$`WQX Parameter`[Combined$Group == input$Category])), selected = "Lead")})
    updateSelectizeInput(session, 'State',
                         choices = c("All", unique(Combined$COUNTRY)), selected = "All",
                         server = TRUE)
   


    output$mytable <- renderPlot({
        data <- WQ
       data2 <- subset(data, `Use in BMP Category Analysis` == "Yes" | `Use in BMP Category Analysis` == "yes")
        data <- subset(data2, `Monitoring Station Type` == "Inflow" | `Monitoring Station Type` == "Outflow")
        
        if (input$Type != "All") {
            data <- data[data$Analysis_Category %in% input$Type,]
        }
        if (input$Metric != "All") {
            data <- data[data$`WQX Parameter` %in% input$Metric,]
        }
        if (input$State != "All") {
            data <- data[data$COUNTRY %in% input$State,]
        }
        data
        wq2 <- data
        
        wq2 <- rename(wq2, "value" = "WQ Analysis Value")
        wq2 <- rename(wq2, "station" = "Monitoring Station Type")
        
        wq2$station <- as.factor(wq2$station)
        
        wq2 <- subset(wq2, station == "Inflow" | station == "Outflow")
        test <- range(wq2$value)
        mytable<-ggplot(wq2, aes(x=Analysis_Category, y=value, fill = station)) + geom_boxplot(outlier.colour="red", outlier.shape=NA,
                                                                                      outlier.size=.01) + theme_minimal() + scale_y_log10()
        ggplot(wq2, aes(x=Analysis_Category, y=value, fill = station)) + geom_boxplot(outlier.colour="red", outlier.shape=NA,
                                                                 outlier.size=.01) + theme_minimal() + scale_y_log10()
        
    })
    output$mywilcox <- renderPrint({
        datasub1 <- Combined

        if (input$Type != "All") {
            datasub1 <- datasub1[datasub1$Analysis_Category %in% input$Type,]
        }
        if (input$Metric != "All") {
            datasub1 <- datasub1[datasub1$`WQX Parameter` %in% input$Metric,]
        }
        if (input$State != "All") {
            datasub1 <- datasub1[datasub1$COUNTRY %in% input$State,]
        }
        datasub1
        
        t <- wilcox.test(datasub1$`WQ Analysis Value.x`, datasub1$`WQ Analysis Value.y`)
        print(t)
        
        t$p.value
    })
    
    
    output$significant <- renderText({ 
      datasub1 <- Combined
      
      if (input$Type != "All") {
        datasub1 <- datasub1[datasub1$Analysis_Category %in% input$Type,]
      }
      if (input$Metric != "All") {
        datasub1 <- datasub1[datasub1$`WQX Parameter` %in% input$Metric,]
      }
      if (input$State != "All") {
        datasub1 <- datasub1[datasub1$COUNTRY %in% input$State,]
      }
      datasub1
      wq2
      t <- wilcox.test(datasub1$`WQ Analysis Value.x`, datasub1$`WQ Analysis Value.y`)
      print(t)
      t$p.value
      if(t$p.value < .05){
        return(paste("<span style=\"color:green\">Water Quality Change is Statistically Significant (P<.05)</span>"))
        
      }else{
        return(paste("<span style=\"color:red\">Water Quality Change is NOT Statistically Significant (P<.05)</span>"))
      }
    })
    
    output$downloadPlot <- downloadHandler(
      filename = function() { paste(input$dataset, '.png', sep='') },
      content = function(file) {
        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, plot = mytable, device = device)
      }
    )
    
    output$table <- renderDT({ 
      datasub1 <- Combined
      
      if (input$Type != "All") {
        datasub1 <- datasub1[datasub1$Analysis_Category %in% input$Type,]
      }
      if (input$Metric != "All") {
        datasub1 <- datasub1[datasub1$`WQX Parameter` %in% input$Metric,]
      }
      if (input$State != "All") {
        datasub1 <- datasub1[datasub1$COUNTRY %in% input$State,]
      }
      datasub2 <- datasub1 %>% dplyr::group_by(Analysis_Category, Group, `WQX Parameter`) %>% 
       dplyr::summarise(`Median Percent Reduction` = median(PercentReduction, na.rm = TRUE),
                 `Median Percent Reduction (25th Quantile)` = quantile(PercentReduction, probs = c(.25)),
                 `Median Percent Reduction (75th Quantile)` = quantile(PercentReduction, probs = c(.75)),
                 `Sample Size` = n())
      
     datasub2$`Median Percent Reduction` <- scales::percent(datasub2$`Median Percent Reduction`)
      datasub2$`Median Percent Reduction (25th Quantile)` <- scales::percent(datasub2$`Median Percent Reduction (25th Quantile)`)
    datasub2$`Median Percent Reduction (75th Quantile)` <- scales::percent(datasub2$`Median Percent Reduction (75th Quantile)`)
      datasub2$Group <- NULL
      datasub2$`Range (25th - 75th Percentile)` <- paste(datasub2$`Median Percent Reduction (25th Quantile)`, "to", datasub2$`Median Percent Reduction (75th Quantile)`)
      datasub2$`Median Percent Reduction (25th Quantile)` <- NULL
      datasub2$`Median Percent Reduction (75th Quantile)` <- NULL
      DT::datatable(datasub2)
    })

})


