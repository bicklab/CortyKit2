#' @title tt: table of table
#'
#' @param x the vector
#'
#' @return hard to explain, but it's table(table(x))
#' @export
tt = function(x) {
	table(table(x))
}

#' @title lu: length of unique
#'
#' @param x the vector
#'
#' @return the number of unique values in the vector
#' @export
lu = function(x) {
	length(unique(x))
}

# g = pillar::glimpse

#' @title nin: not in
#'
#' @param a one vector
#' @param b another vector
#'
#' @return a binary vector of whether each entry in A is not in B (the converse of %in%)
#' @export
'%nin%' = function(a, b) {
  (!(a %in% b))
}
