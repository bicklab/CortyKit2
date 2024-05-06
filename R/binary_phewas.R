binary_phewas = function(covars, phecodes, phecode_info,
												 min_num_codes = 4,
												 min_num_cases = 5,
												 num_cores = 18) {

	# cannot function if there are phecodes observed that lack info
	obs_phecodes = unique(phecodes$phecodeX)
	if (any(obs_phecodes %nin% phecode_info$phecode))
		stop('There is at least one phecode that appears in [phecodes] but not in [phecode_info]')

	# only need info on observed phecodes
	phecode_info = filter(phecode_info, phecode %in% unique(phecodes$phecodeX))

	# only use phecode counts that are in great enough quantity
	# to be believed
	phecodes = filter(phecodes, phecode_count >= min_num_codes)

	phecode_info |>
		group_by(category) |>
		group_split() |>
		future_map(binary_phewas_)


		results = list()
	for (phecode_idx in 1:nrow(phecode_info)) {

		this_phecode = phecode_info$phecode[phecode_idx]
		this_sex = phecode_info$sex[phecode_idx]

		if (this_sex == 'Male')
			this_covars = filter(covars, sex_male == TRUE)

		if (this_sex == 'Female')
			this_covars = filter(covars, sex_female == TRUE)

		if (this_sex == 'Both')
			this_covars = covars

		phecodes %>%
			filter(phecodeX == this_phecode) |>
			pull(person_id) ->
			pids_w_phecode

		if (length(pids_w_phecode) <= min_num_cases)
			next

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
