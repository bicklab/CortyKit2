#' @title vectorized binomial p value
#'
#' @param a number of successes
#' @param b number of failures (defaults to n - a)
#' @param n number of trials (defaults to a + b)
#' @param null_prob prob of success under the null hypothesis, default 0.5
#'
#' @return p value of binomial test
#' @export
binom_p_val = function(a, b = n - a, n = a + b, null_prob = 0.5) {

  # pbinom function does not seem to be vectorized over lower.tail arg
	need_lower_tail = (a/n < null_prob)
	lower_tail_prob = pbinom(q = a, size = n, prob = null_prob, lower.tail = TRUE)
  upper_tail_prob = pbinom(q = a, size = n, prob = null_prob, lower.tail = FALSE)
  return(ifelse(need_lower_tail, lower_tail_prob, upper_tail_prob))
}


slow_binom_p_val = function(a, b = n - a, n = a + b) {
  stats::binom.test(x = a, n = a + b)$p.value
}
