
CalculateCMGRByGroup <- function(data, n, mainCol, valueCol, ...) {
  mainCol <- enquo(mainCol)
  valueCol <- enquo(valueCol)
  arrangeCols <- enquos(...)

  df <- data %>%
    arrange(!!!arrangeCols) %>%
    group_by(!!mainCol) %>%
    mutate(CMGR = ((!!valueCol / lag(!!valueCol, n)) ^ (1 / n)) - 1) %>%
    ungroup()

  return(df)
}
