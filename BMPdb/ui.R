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
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    headerPanel(title = "Water Quality performance of GI"),
    sidebarPanel(selectizeInput('x', label = 'GI Type',
                                    choices = unique(Combined$Analysis_Category)),
                     selectizeInput('y', label = 'WQ Category', 
                                    choices = unique(Combined$Group)),
                     selectizeInput("color", label = "WQ Metric", 
                                    choices = unique(Combined$`WQX Parameter`)),
                     selectizeInput("dev", label = "GI State", 
                                    choices = unique(Combined$STATE))
                     
    ),
    mainPanel(box(plotOutput("mytable"), width = 12),
              box(textOutput("mywilcox"), width =12)
    )
))

