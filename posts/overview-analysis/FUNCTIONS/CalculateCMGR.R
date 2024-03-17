


CalculateCMGR <- function(data, n, valueCol, ...) {

  valueCol <- enquo(valueCol)
  arrangeCols <- enquos(...)

  df <- data %>%
    arrange(!!!arrangeCols) %>%
    mutate(CMGR = ((!!valueCol / lag(!!valueCol, n)) ^ (1 / n)) - 1)

  return(df)
}
