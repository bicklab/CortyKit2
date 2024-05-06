#' @title phewas
#'
#' @param covars the covariates (predictors) of the logistic glm
#' @param phecodes table of phecodes
#' @param phecode_info information abou the phecodes
#' @param min_num_codes minimum number of codes a person must have to count as having the phecode
#' @param min_num_cases minimum number of cases of a phecode that must be observed to consider it worth testing
#'
#' @return a table of results
#' @export
binary_phewas = function(covars,
												 phecodes,
												 phecode_info,
												 min_num_codes = 4,
												 min_num_cases = 5) {

	# cannot function if there are phecodes observed that lack info
	obs_phecodes = unique(phecodes$phecodeX)
	if (any(obs_phecodes %nin% phecode_info$phecode))
		stop('There is at least one phecode that appears in [phecodes] but not in [phecode_info]')

	# only need info on observed phecodes
	phecode_info = filter(phecode_info, phecode %in% unique(phecodes$phecodeX))

	# only use phecode counts that are in great enough quantity
	# to be believed
	phecodes = filter(phecodes, phecode_count >= min_num_codes)

	# set up multisession evaluation, then split the work across workers
	future::plan(strategy = multisession)

	phecode_info |>
		dplyr::group_by(category) |>
		dplyr::group_split() |>
		furrr::future_map_dfr(binary_phewas_one_chunk) ->
		result

		return(result)
}

binary_phewas_one_chunk = function(phecode_info_chunk) {

	for (phecode_idx in 1:nrow(phecode_info_chunk)) {

		this_phecode = phecode_info_chunk$phecode[phecode_idx]
		this_sex = phecode_info_chunk$sex[phecode_idx]

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
