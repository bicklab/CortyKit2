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
												 min_num_cases = 5) {

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

	return(
		furrr::future_map2_dfr(
			.x = phecode_info_split,
			.y = phecode_counts_split,
			.f = binary_phewas_one_chunk,
			covars = covars,
			.progress = TRUE
		)
	)
}

binary_phewas_one_chunk = function(phecode_info_chunk, phecodes_chunk, covars) {

	for (phecode_idx in 1:nrow(phecode_info_chunk)) {

		this_phecode = phecode_info_chunk$phecode[phecode_idx]
		this_sex = phecode_info_chunk$sex[phecode_idx]

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

		if (length(pids_w_phecode) <= min_num_cases)
			next

		covars %>%
			mutate(has_phecode = person_id %in% pids_w_phecode) |>
			glm(formula = has_phecode ~ .) |>
			broom::tidy() |>
			mutate(phecode = this_phecode) ->
			results[[this_phecode]]
	}
	return(bind_rows(results))

}
