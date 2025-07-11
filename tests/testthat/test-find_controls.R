test_that(
	"get_control_ids() succeeds when it should",
	{
		n = 100
		data = tibble::tibble(
			person_id = 1:n,
			age = rep(c(40, 50, 60, 70, 80), n/5),
			sex = rep(c('F', 'M'), n/2)
		)
		control_ids = get_control_ids(case_ids = 1:10, match_variables = c('age', 'sex'), data = data)
		expect_no_error(get_control_ids(case_ids = 1:10, match_variables = c('age', 'sex'), data = data))
		expect_type(control_ids, 'integer')
		expect_equal(length(control_ids), 50)
	}
)

test_that(
	"get_control_ids() fails when it should",
	{
		n = 100
		data = tibble::tibble(
			person_id = 1:n,
			age = rep(c(40, 50, 60, 70, 80), n/5),
			sex = rep(c('F', 'M'), n/2)
		)
		expect_error(get_control_ids(case_ids = 1:20, match_variables = c('age', 'sex'), data = data))
		expect_error(get_control_ids(case_ids = c('apple', 'banana'), match_variables = c('age', 'sex'), data = data))
		expect_error(get_control_ids(case_ids = 1:10, match_variables = c('age', 'height'), data = data))
	}
)

