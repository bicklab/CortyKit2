binary_phewas = function(covars, phecodes, phecode_info, min_num_codes = 4) {

	obs_phecodes = unique(phecodes$phecodeX)
	if (any(obs_phecodes %nin% phecode_info$phecode)) {
		stop('There is at least one phecode that appears in [phecodes] but not in [phecode_info]')
	}
	phecode_info = filter(phecode_info, phecode %in% unique(phecodes$phecodeX))

	results = list()
	for (this_phecode_idx in 1:nrow(phecode_info)) {

		this_phecode = phecode_info$phecode[this_phecode_idx]
		this_sex = phecode_info$sex[this_phecode_idx]

		if (this_sex == 'Male')
			this_covars = filter(covars, sex_male == TRUE)

		if (this_sex == 'Female')
			this_covars = filter(covars, sex_female == TRUE)

		if (this_sex == 'Both')
			this_covars = covars


		phecodes %>%
			filter(phecodeX == this_phecode, n >= min_num_codes) |>
			pull(person_id) ->
			pids_w_phecode

		covars %>%
			mutate(has_phecode = person_id %in% pids_w_phecode) ->
			to_glm

		glm(formula = has_phecode ~ ., data = to_glm) |>
			broom::tidy() |>
			mutate(phecode = this_phecode) ->
			results[[this_phecode]]
	}
	return(bind_rows(results))
}
