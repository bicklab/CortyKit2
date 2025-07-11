test_that(
  'vectorized binom returns same results as old/slow version',
  {
    num = 1e2
    a = sample(30:50, size = num, replace = TRUE)
    b = sample(30:50, size = num, replace = TRUE)
    slow_binom_p_val = function(a, b) {
    	stats::binom.test(x = a, n = a + b)$p.value
    }
    # print(tibble::tibble(a, b))
    expect_equal(
      object = binom_05_p_val(a = a, b = b),
      expected = purrr::map2_dbl(.x = a, .y = b, .f = slow_binom_p_val),
      tolerance = 1e-5
    )
  }
)
