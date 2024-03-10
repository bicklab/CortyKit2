#' @title commonest values
#'
#' @param v the vector
#' @param n how many values to return
#'
#' @return the n most common values in v
#' @export
commonest_values <- function(v, n = 5) {
  table(v, useNA = 'ifany') %>%
    sort(decreasing = TRUE) %>%
    utils::head(n = n) %>%
    names()
}



#' @title second highest value of vector
#'
#' @param x the vector
#'
#' @return the second highest value in x
#' @export
second_highest_val = function(x) {
  max(x[-which.max(x)])
}
