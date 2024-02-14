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
  2 * pbinom(q = pmin.int(a, b), size = n, prob = null_prob) - (a == b)*dbinom(x = a, size = n, prob = null_prob)
}


slow_binom_p_val = function(a, b = n - a, n = a + b) {
  binom.test(x = a, n = a + b)$p.value
}
