# binary_phewas = function(covars, phecodes, phecode_info, min_num_codes = 4) {
#
# 	# ensure we have INFO on all the phecodes that have been observed
# 	obs_phecodes = unique(phecodes$phecode)
# 	if (any(obs_phecodes %nin% phecode_info$phecode)) {
# 		stop('There is at least one phecode that appears in [phecodes] but not in [phecode_info]')
# 	}
# 	# and remove the INFO pertaining to the unobserved ones
# 	phecode_info = filter(phecode_info, phecode %in% unique(phecodes$phecode))
#
# 	# loop over phecodes listed in INFO
# 	results = list()
# 	for (phecode_idx in 1:nrow(phecode_info)) {
#
# 		this_phecode = phecode_info$phecode[phecode_idx]
# 		this_sex = phecode_info$sex[phecode_idx]
#
# 		if (this_sex == 'Male')
# 			this_covars = filter(covars, sex_male == TRUE)
#
# 		if (this_sex == 'Female')
# 			this_covars = filter(covars, sex_female == TRUE)
#
# 		if (this_sex == 'Both')
# 			this_covars = covars
#
# 		phecodes |>
# 			filter(phecodeX == this_phecode, phecode_count >= min_num_codes) |>
# 			pull(person_id) ->
# 			pids_w_phecode
#
# 		covars %>%
# 			mutate(has_phecode = person_id %in% pids_w_phecode) ->
# 			to_glm
#
# 		glm(formula = has_phecode ~ ., data = select(to_glm, -person_id)) |>
# 			broom::tidy() |>
# 			mutate(phecode = this_phecode) ->
# 			results[[this_phecode]]
# 	}
# 	return(bind_rows(results))
# }
