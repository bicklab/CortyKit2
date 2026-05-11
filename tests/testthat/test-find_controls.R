test_that(
	"match_cases_to_controls() succeeds when it should",
	{
		n = 100
		data = tibble::tibble(
			person_id = 1:n,
			age = rep(c(40, 50, 60, 70, 80), n/5),
			sex = rep(c('F', 'M'), n/2)
		)
		result = match_cases_to_controls(case_ids = 1:10, match_variables = c('age', 'sex'), data = data)
		expect_no_error(match_cases_to_controls(case_ids = 1:10, match_variables = c('age', 'sex'), data = data))
		expect_s3_class(result, 'tbl_df')
		expect_named(result, c('case_id', 'control_id'))
		expect_equal(nrow(result), 50)
		# each control should appear at most once
		expect_equal(length(unique(result$control_id)), nrow(result))
		# every case_id in result should be one of the requested case_ids
		expect_true(all(result$case_id %in% 1:10))
	}
)

test_that(
	"match_cases_to_controls() fails when it should",
	{
		n = 100
		data = tibble::tibble(
			person_id = 1:n,
			age = rep(c(40, 50, 60, 70, 80), n/5),
			sex = rep(c('F', 'M'), n/2)
		)
		expect_error(match_cases_to_controls(case_ids = 1:20, match_variables = c('age', 'sex'), data = data))
		expect_error(match_cases_to_controls(case_ids = c('apple', 'banana'), match_variables = c('age', 'sex'), data = data))
		expect_error(match_cases_to_controls(case_ids = 1:10, match_variables = c('age', 'height'), data = data))
	}
)
