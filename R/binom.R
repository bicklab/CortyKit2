#' @title vectorized binomial p value
#'
#' @param a number of successes
#' @param b number of failures (defaults to n - a)
#'
#' @return p value of binomial test
#' @export
binom_05_p_val = function(a, b) {

	n = a + b
	center <- floor(n / 2)
	lower_tail <- pbinom(a,     n, 0.5, lower.tail = TRUE)
	upper_tail <- pbinom(a - 1, n, 0.5, lower.tail = FALSE)
	adj = ifelse(a == b, dbinom(a, n, 0.5), 0)
	return(2 * pmin(lower_tail, upper_tail) - adj)
}
