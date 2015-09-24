library(shiny)
library(ggplot2)
library(rCharts)
library(ggvis)
library(data.table)
library(reshape2)
library(dplyr)
library(markdown)
library(mapproj)
library(maps)
source("trades.R", local = TRUE)

states_map <- map_data("state")
dt <- fread('Data/stocktrades.csv') %>% mutate(SECURITY = tolower(SECURITY))
securities <- sort(unique(dt$SECURITY))


# Shiny server 
shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  values$securities <- securities
  
  output$securitiesControls <- renderUI({
    checkboxGroupInput('securities', 'Event types', securities, selected=values$securities)
  })
  
  observe({
    if(input$clear_all == 0) return()
    values$securities <- c()
  })
  
  observe({
    if(input$select_all == 0) return()
    values$securities <- securities
  })
  
  dt.agg <- reactive({
    aggregate_by_state(dt, input$range[1], input$range[2], input$securities)
  })
  
  dt.agg.year <- reactive({
    aggregate_by_year(dt, input$range[1], input$range[2], input$securities)
  })
  
  dataTable <- reactive({
    prepare_downolads(dt.agg())
  })
  
  output$populationImpactByState <- renderPlot({
    print(plot_impact_by_state (
      dt = compute_affected(dt.agg(), input$populationCategory),
      states_map = states_map, 
      year_min = input$range[1],
      year_max = input$range[2],
      title = "Population impact %d - %d (number of affected)",
      fill = "Affected"
    ))
  })
  
  output$economicImpactByState <- renderPlot({
    print(plot_impact_by_state(
      dt = compute_damages(dt.agg(), input$economicCategory),
      states_map = states_map, 
      year_min = input$range[1],
      year_max = input$range[2],
      title = "Economic impact %d - %d (Million USD)",
      fill = "Damages"
    ))
  })
  
  output$TradesByYear <- renderChart({
    plot_trades_by_year(dt.agg.year())
  })
  
  output$PTRADELTRADE <- renderChart({
    plot_PLTRADE_by_year(
      dt = dt.agg.year() %>% select(Year, Ltrade, Ptrade),
      dom = "PTRADELTRADE",
      yAxisLabel = "Trades",
      desc = TRUE
    )
  })
  
  output$PROFITLOSS <- renderChart({
    plot_PLTRADE_by_year(
      dt = dt.agg.year() %>% select(Year, Loss, Profit),
      dom = "PROFITLOSS",
      yAxisLabel = "Total Value (Thousand INR)"
    )
  })
  

})
