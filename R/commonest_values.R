commonest_values <- function(v, n = 5) {
  table(v, useNA = 'ifany') %>%
    sort(decreasing = TRUE) %>%
    head(n = n) %>%
    names()
}



second_highest_val = function(x) {
  max(x[-which.max(x)])
}
