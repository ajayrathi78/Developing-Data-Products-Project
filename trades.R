#' @param dt data.table
#' @param year_min integer
#' @param year_max integer
#' @param securities character vector
#' @return data.table
#'
aggregate_by_state <- function(dt, year_min, year_max, securities) {
  replace_na <- function(x) ifelse(is.na(x), 0, x)
  round_2 <- function(x) round(x, 2)
  
  states <- data.table(STATE=sort(unique(dt$STATE)))
  
  aggregated <- dt %>% filter(YEAR >= year_min, YEAR <= year_max, SECURITY %in% securities) %>%
    group_by(STATE) %>%
    summarise_each(funs(sum), TRADE:LOSS)

  left_join(states,  aggregated, by = "STATE") %>%
    mutate_each(funs(replace_na), PTRADE:LOSS) %>%
    mutate_each(funs(round_2), PROFIT, LOSS)    
}

#' @param dt data.table
#' @param year_min integer
#' @param year_max integer
#' @param securities character vector
#' @return data.table
#'
aggregate_by_year <- function(dt, year_min, year_max, securities) {
  round_2 <- function(x) round(x, 2)
  
  dt %>% filter(YEAR >= year_min, YEAR <= year_max, SECURITY %in% securities) %>%
    group_by(YEAR) %>% summarise_each(funs(sum), TRADE:LOSS) %>%
    mutate_each(funs(round_2), PROFIT, LOSS) %>%
    rename(
      Year = YEAR, Trade = TRADE,
      Ptrade = PTRADE, Ltrade = LTRADE,
      Profit = PROFIT, Loss = LOSS
    )
}

#' @param dt data.table
#' @param category character
#' @return data.table
#'
compute_affected <- function(dt, category) {
  dt %>% mutate(Affected = {
    if(category == 'both') {
      LTRADE + PTRADE
    } else if(category == 'Ptrade') {
      PTRADE
    } else {
      LTRADE
    }
  })
}

#' @param dt data.table
#' @param category character
#' @return data.table
#'
compute_damages <- function(dt, category) {
  dt %>% mutate(Damages = {
    if(category == 'both') {
      PROFIT + LOSS
    } else if(category == 'Loss') {
      LOSS
    } else {
      PROFIT
    }
  })
}

#' @param dt data.table
#' @param states_map data.frame returned from map_data("state")
#' @param year_min integer
#' @param year_max integer
#' @param fill character name of the variable
#' @param title character
#' @param low character hex
#' @param high character hex
#' @return ggplot
#' 
plot_impact_by_state <- function (dt, states_map, year_min, year_max, fill, title, low = "#fff5eb", high = "#d94801") {
  title <- sprintf(title, year_min, year_max)
  p <- ggplot(dt, aes(map_id = STATE))
  p <- p + geom_map(aes_string(fill = fill), map = states_map, colour='black')
  p <- p + expand_limits(x = states_map$long, y = states_map$lat)
  p <- p + coord_map() + theme_bw()
  p <- p + labs(x = "Long", y = "Lat", title = title)
  p + scale_fill_gradient(low = low, high = high)
}

#' @param dt data.table
#' @param dom
#' @param yAxisLabel
#' @param desc
#' @return plot
#' 
plot_PLTRADE_by_year <- function(dt, dom, yAxisLabel, desc = FALSE) {
  PLPLOT <- nPlot(
    value ~ Year, group = "variable",
    data = melt(dt, id="Year") %>% arrange(Year, if (desc) { desc(variable) } else { variable }),
    type = "stackedAreaChart", dom = dom, width = 650
  )
  PLPLOT$chart(margin = list(left = 100))
  PLPLOT$yAxis(axisLabel = yAxisLabel, width = 80)
  PLPLOT$xAxis(axisLabel = "Year", width = 70)
  
  PLPLOT
}

#' @param dt data.table
#' @param dom
#' @param yAxisLabel
#' @return plot

plot_trades_by_year <- function(dt, dom = "TradesByYear", yAxisLabel = "Trade") {
  TradesByYear <- nPlot(
    Trade ~ Year,
    data = dt,
    type = "lineChart", dom = dom, width = 650
  )
  
  TradesByYear$chart(margin = list(left = 100))
  TradesByYear$yAxis( axisLabel = yAxisLabel, width = 80)
  TradesByYear$xAxis( axisLabel = "Year", width = 70)
  TradesByYear
}

