#' @importFrom rlang .data
match_on = function(case_ids, data, match_vars, ccr) {

	data |>
		dplyr::filter(.data$person_id %in% case_ids) |>
		dplyr::group_by(!!!rlang::syms(match_vars)) |>
		dplyr::summarise(
			num_controls_needed = ccr*dplyr::n(),
			case_pids = list(.data$person_id),
			.groups = 'drop'
		) ->
		control_demand_df

	data |>
		dplyr::filter(.data$person_id %nin% case_ids) |>
		dplyr::group_by(!!!rlang::syms(match_vars)) |>
		dplyr::summarise(
			num_controls_avail = dplyr::n(),
			avail_control_pids = list(.data$person_id),
			.groups = 'drop'
		) ->
		control_supply_df

	set.seed(1)

	control_demand_df |>
		dplyr::left_join(control_supply_df, by = dplyr::join_by(!!!rlang::syms(match_vars))) |>
		# can use this to figure out if matching will succeed
		# mutate(deficit = num_controls_needed - num_controls_avail) |> filter(deficit > 0) |> arrange(deficit)
		dplyr::mutate(control_pids = purrr::map2_chr(
			.x = .data$avail_control_pids,
			.y = .data$num_controls_needed,
			.f = sample,
			replace = FALSE)) |>
		dplyr::pull(.data$control_pids) |> unlist()  |>
		return()
}


#' get control ids
#'
#' @param case_ids the case ids
#' @param match_variables vector of variables to match on in order from most important to least
#' @param data data used for matching, with id column labeled 'person_id'
#' @param exclude_ids ids that user does not want to consider as possible controls
#' @param control_case_ratio number of controls per case, default to 10
#' @param verbose whether to output what ends up getting matched on
#'
#' @return a vector of control ids
#' @export
get_control_ids = function(case_ids,
													 match_variables,
													 data,
													 exclude_ids = NULL,
													 control_case_ratio = 5,
													 seed = 1,
													 verbose = FALSE) {

	# check inputs
	{
		if (!all(case_ids %in% data$person_id)) {
			stop("Some case_id's aren't in data.")
		}
		if (!is.null(exclude_ids)) {
			if (!all(exclude_ids %in% data$person_id)) {
				stop("Some exclude_id's aren't in data.")
			}
			if (!purrr::is_empty(intersect(case_ids, exclude_ids))) {
				stop("There is overlap between case_id's and exclude_id's.")
			}
			covariates = dplyr::filter(covariates, .data$person_id %nin% exclude_ids)
		}
		if (!all(match_variables %in% names(data))) {
			stop("Some match_variable's aren't in data.")
		}
	}

	# establish what controls are needed
	data |>
		dplyr::filter(.data$person_id %in% case_ids) |>
		dplyr::group_by(!!!rlang::syms(match_variables)) |>
		dplyr::summarise(
			num_controls_needed = control_case_ratio*dplyr::n(),
			case_pids = list(.data$person_id),
			.groups = 'drop'
		) ->
		control_demand_df

	# establish what controls are available
	data |>
		dplyr::filter(.data$person_id %nin% case_ids) |>
		dplyr::group_by(!!!rlang::syms(match_variables)) |>
		dplyr::summarise(
			num_controls_avail = dplyr::n(),
			avail_control_pids = list(.data$person_id),
			.groups = 'drop'
		) ->
		control_supply_df

	set.seed(seed)

	control_demand_df |>
		dplyr::left_join(control_supply_df,
										 by = dplyr::join_by(!!!rlang::syms(match_variables)))  |>
		# can use this to figure out if matching will succeed
		# mutate(deficit = num_controls_needed - num_controls_avail) |> filter(deficit > 0) |> arrange(deficit)
		dplyr::mutate(
			complete_match = .data$num_controls_needed <= .data$num_controls_avail,
			control_pids = purrr::map2_chr(
				.x = .data$avail_control_pids,
				.y = min(.data$num_controls_needed, .data$num_controls_avail),
				.f = sample,
				replace = FALSE)) ->
		result

	return(result)
}
