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
												 use_all_cores = TRUE) {

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

	if (use_all_cores == FALSE) {

		outer_results = list()
		for (n in names(phecode_info_split)) {

			message('starting on ', n)
			binary_phewas_one_chunk(
				phecode_info_chunk = phecode_info_split[[n]],
				phecodes_chunk = phecode_counts_split[[n]],
				covars = covars,
				min_num_cases = min_num_cases
			) ->
				outer_results[[n]]
		}
		return(bind_rows(outer_results))

		# message('Running PheWAS on 1 core.')
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

	message('Running PheWAS on ', future::availableCores() , ' cores.')
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

	inner_results = list()
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

		message('covars has dim: ', dim(this_covars))

		# need to pull this into the map somehow!
		phecodes_chunk %>%
			filter(phecode == this_phecode) |>
			pull(person_id) |>
			unique() ->
			pids_w_phecode

		print(str(pids_w_phecode))

		# message('found ', length(pids_w_phecode), ' people with phecode')

		if (length(pids_w_phecode) <= min_num_cases) {
			next
		}

		safe_glm = safely(function(x) stats::glm(formula = has_phecode ~ . ,
																						 family = 'binomial',
																						 data = x))

		covars %>%
			dplyr::mutate(has_phecode = person_id %in% pids_w_phecode) |>
			select(-person_id)  ->
			to_glm

		message('to_glm has dim: ', dim(to_glm))

		safe_glm(to_glm) |>
			pluck('result') |>
			broom::tidy() |>
			dplyr::mutate(phecode = this_phecode) ->
			inner_results[[this_phecode]]
	}
	return(bind_rows(inner_results))

}
