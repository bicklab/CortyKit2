test_that(
  'vectorized binom returns same results as old/slow version',
  {
    num = 1e3
    a = sample(30:50, size = num, replace = TRUE)
    b = sample(30:50, size = num, replace = TRUE)
    # print(tibble(a, b))
    expect_equal(
      object = binom_p_val(a = a, b = b),
      expected = purrr::map2_dbl(.x = a, .y = b, .f = slow_binom_p_val),
      tolerance = 1e-5
    )
  }
)
