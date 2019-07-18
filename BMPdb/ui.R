#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
library(DT)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    headerPanel(title = "Water Quality performance of GI"),
    sidebarPanel(checkboxGroupInput('Type', label = 'GI Type',
                                    choices = unique(Combined$Analysis_Category)),
                 selectizeInput('Category', label = 'Water Quality Category',
                                choices = unique(Combined$Group)),
                     selectizeInput("Metric", label = "WQ Metric", 
                                    choices = unique(Combined$`WQX Parameter`)),
                     selectizeInput("State", label = "GI COUNTRY", 
                                    choices = unique(Combined$COUNTRY))
                     
    ),
    mainPanel(tabsetPanel(
        tabPanel("Plot1",htmlOutput("samplesize"), htmlOutput("significant"), 
                 callModule(downloadablePlot,
                            "mytable", 
                            logger = ss_userAction.Log,
                            filenameroot = "mydownload1",
                            aspectratio = 1.33,
                            downloadfxns = list(png = myplotfxn, tsv = mydatafxn),
                            visibleplot = myplotfxn),
                 plotOutput("mytable"), 
                 downloadButton('downloadPlot'), 
                 div(DT::dataTableOutput("table"), style = "font-size: 75%; width: 75%"))

    ))
))


