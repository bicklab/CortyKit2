#' Calculate required controls for each matching group
get_control_demand = function(data, case_ids, match_vars, ccr) {
	data |>
		dplyr::filter(.data$person_id %in% case_ids) |>
		dplyr::group_by(dplyr::across(dplyr::all_of(match_vars))) |>
		dplyr::summarise(
			num_controls_needed = ccr * dplyr::n(),
			case_pids = list(.data$person_id),
			.groups = "drop"
		)
}

#' Calculate available controls for each matching group
get_control_supply = function(data, case_ids, match_vars) {
	data |>
		dplyr::filter(.data$person_id %nin% case_ids) |>
		dplyr::group_by(dplyr::across(dplyr::all_of(match_vars))) |>
		dplyr::summarise(
			num_controls_avail = dplyr::n(),
			avail_control_pids = list(.data$person_id),
			.groups = "drop"
		)
}

sample_controls = function(demand_df, supply_df, seed) {
	set.seed(seed)
	match_vars = intersect(names(demand_df), names(supply_df))

	joined = dplyr::left_join(
		demand_df,
		supply_df,
		by = dplyr::join_by(!!!rlang::syms(match_vars))
	)

	sample_or_fail = function(num_controls_needed, avail_control_pids, ...) {
		row = list(...)
		group = row[match_vars]
		fail_key = paste(group, collapse = ", ")

		if (length(avail_control_pids) < num_controls_needed) {
			stop(glue::glue(
				"Sampling failed for group [{fail_key}]: need {num_controls_needed}, but only {length(avail_control_pids)} available."
			))
		}

		sample(avail_control_pids, size = num_controls_needed, replace = FALSE)
	}

	purrr::pmap(
		.l = joined,
		.f = sample_or_fail
	) |>
		unlist()
}




#' @title Get control_id's from a dataset that match the case_id's on match_variables.
#'
#' @param case_ids the IDs of the cases
#' @param match_variables the variables to match on
#' @param data The data about cases and potential controls.
#'		It must contain match_variables as columns and have a column called
#'		'person_id' that contains all values of case_ids
#' @param control_case_ratio how many controls do we want per case?
#' @param sampling_seed seed used for random sampling of controls from data
#'		(default is ZIP code of VUMC)
#'
#' @export
get_control_ids = function(case_ids,
													 match_variables,
													 data,
													 sampling_seed = 37232,
													 control_case_ratio = 5) {

	if (!all(case_ids %in% data$person_id)) {
		stop("Some case_id's aren't in data.")
	}

	if (!all(match_variables %in% names(data))) {
		stop("Some match_variable's aren't in data.")
	}

	demand_df = get_control_demand(data, case_ids, match_variables, control_case_ratio)
	supply_df = get_control_supply(data, case_ids, match_variables)

	return(sample_controls(demand_df, supply_df, sampling_seed))
}
