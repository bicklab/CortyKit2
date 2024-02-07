binom_p_val = function(a, b = n - a, n = a + b) {
  2 * pbinom(q = pmin.int(a, b), size = n, prob = 0.5) - (a == b)*dbinom(x = a, size = n, prob = 0.5)
}


slow_binom_p_val = function(a, b = n - a, n = a + b) {
  binom.test(x = a, n = a + b)$p.value
}
