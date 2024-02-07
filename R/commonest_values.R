#' @title commonest_values
#'
#' @param v the vector to assess
#' @param n the number of common values are returned
#'
#' @return the commonest n values in v, sorted
#' @export
#'
commonest_values <- function(v, n = 5) {
  table(v, useNA = 'ifany') %>%
    sort(decreasing = TRUE) %>%
    head(n = n) %>%
    names()
}



second_highest_val = function(x) {
  max(x[-which.max(x)])
}
