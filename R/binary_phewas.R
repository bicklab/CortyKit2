#' @title phewas
#'
#' @param covars the covariates (predictors) of the logistic glm
#' @param phecodes_counts a table of how many times each phecode was observed for each person
#' @param phecode_info information abou the phecodes
#' @param min_num_codes minimum number of codes a person must have to count as having the phecode
#' @param min_num_cases minimum number of cases of a phecode that must be observed to consider it worth testing
#'
#' @return a table of results
#' @export
binary_phewas = function(covars,
												 phecode_counts,
												 phecode_info,
												 min_num_codes = 4,
												 min_num_cases = 5,
												 num_cores = 1) {

	# only use phecode counts that are in great enough quantity
	# to be believed
	phecode_counts = filter(phecode_counts, phecode_count >= min_num_codes)

	# cannot function if there are phecodes observed that lack info
	if (any(unique(phecode_counts$phecode) %nin% phecode_info$phecode))
		stop('There is at least one phecode that appears in [phecode_counts] but not in [phecode_info]')

	# only need info on observed phecodes
	phecode_info = filter(phecode_info, phecode %in% unique(phecode_counts$phecode))

	# set up multisession evaluation, then split the work across workers
	future::plan(future::multisession)

	phecode_info |>
		mutate(phecode_prefix = str_sub(phecode, 1, 2)) |>
		split(~ phecode_prefix) ->
		phecode_info_split

	phecode_counts |>
		mutate(phecode_prefix = str_sub(phecode, 1, 2)) |>
		split(~ phecode_prefix) ->
		phecode_counts_split

	if (num_cores == 1) {

		results = list()
		for (n in names(phecode_info_split)) {

			message('starting on ', n)
			binary_phewas_one_chunk(
				phecode_info_chunk = phecode_info_split[[n]],
				phecodes_chunk = phecode_counts_split[[n]],
				covars = covars,
				min_num_cases = min_num_cases
			) ->
				results[[n]]
		}
		return(bind_rows(results))

		# return(
		# 	purrr::list_rbind(
		# 		purrr::map2(
		# 			.x = phecode_info_split,
		# 			.y = phecode_counts_split,
		# 			.f = binary_phewas_one_chunk,
		# 			covars = covars,
		# 			min_num_cases = min_num_cases,
		# 			.progress = TRUE
		# 		)
		# 	)
		# )
	}

	return(
		furrr::future_map2_dfr(
			.x = phecode_info_split,
			.y = phecode_counts_split,
			.f = binary_phewas_one_chunk,
			covars = covars,
			min_num_cases = min_num_cases,
			.progress = TRUE
		)
	)

}

binary_phewas_one_chunk = function(phecode_info_chunk, phecodes_chunk, covars, min_num_cases) {

	for (phecode_idx in 1:nrow(phecode_info_chunk)) {

		this_phecode = phecode_info_chunk$phecode[phecode_idx]
		this_sex = phecode_info_chunk$sex[phecode_idx]
		message('starting on ', phecode_idx, ' of ', nrow(phecode_info_chunk), '...')
		message(this_phecode, ' applies to ', this_sex)

		if (this_sex == 'Male')
			this_covars = filter(covars, sex_male == TRUE)

		if (this_sex == 'Female')
			this_covars = filter(covars, sex_female == TRUE)

		if (this_sex == 'Both')
			this_covars = covars

		# need to pull this into the map somehow!
		phecodes_chunk %>%
			filter(phecode == this_phecode) |>
			pull(person_id) ->
			pids_w_phecode

		message('found ', length(pids_w_phecode), ' people with phecode')

		if (length(pids_w_phecode) <= min_num_cases) {
			message('too few people with this phecode')
			next
		}

		safe_glm = safely(function(x) stats::lm(formula = has_phecode ~ has_any_dada2,
																						# family = 'binomial',
																						data = x))
		message('made safe_glm')

		covars %>%
			dplyr::mutate(has_phecode = person_id %in% pids_w_phecode) ->
			to_glm
		message('made to_glm')

		# print(to_glm)
		# cat(head(to_glm))
		glimpse(to_glm)
		# print(glimpse(to_glm))

		safe_glm(to_glm) |>
			pluck('result') |>
			broom::tidy() |>
			dplyr::mutate(phecode = this_phecode) ->
			result

		results[[this_phecode]] = result
	}
	return(bind_rows(results))

}
