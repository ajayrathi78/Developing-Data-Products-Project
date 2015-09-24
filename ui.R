# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

library(rCharts)

shinyUI(
  navbarPage("Trades Explorer",
             tabPanel("Trade_App",
                      sidebarPanel(
                        sliderInput("range", 
                                    "Range:", 
                                    min = 2001, 
                                    max = 2015, 
                                    value = c(2003, 2011),
                                    format="####"),
                        uiOutput("securitiesControls"),
                        actionButton(inputId = "clear_all", label = "Clear selection", icon = icon("check-square")),
                        actionButton(inputId = "select_all", label = "Select all", icon = icon("check-square-o"))
                      ),
                      
                      mainPanel(
                        tabsetPanel(
                          
                         tabPanel(p(icon("map-marker"), "Introduction"),
                          
                                   column(7,
                                          "This project is part of Developing data products course from Coursera. 
                                          Data used here is the summarized data for various securites. This is dummy data created solely for the purpose of this project. 
                                          Data set contains Year, Security, Number of trades, Profitable trades in year, Loss trades in a year, Total profit from profitable trades
                                          and Total loss from Loss making trades. 
                                          In this simple application there is a slider for showing the charts and also filters for each of the securities. 
                                          There are three graphs which are shown in this project
                                          1) No. of trades by Year
                                          2) Profit and Loss trades by year 
                                          3) Total Profit and Loss by Year"
                                   )
                                   
                          ),
                          
                         tabPanel(p(icon("line-chart"), "Trades By year"),
                                   h4('Number of Trades by year', align = "center"),
                                   showOutput("TradesByYear", "nvd3"),
                                   h4('Profit & Loss Trades by year', align = "center"),
                                   showOutput("PTRADELTRADE", "nvd3"),
                                   h4('Profit and Loss by year', align = "center"),
                                   showOutput("PROFITLOSS", "nvd3")
                          )
                        
                        )
                      )
                      
             )
             
  )
)