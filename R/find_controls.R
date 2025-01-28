#' Calculate required controls for each matching group
get_control_demand = function(data, case_ids, match_vars, ccr) {
	data %>%
		dplyr::filter(.data$person_id %in% case_ids) %>%
		dplyr::group_by(!!!rlang::syms(match_vars)) %>%
		dplyr::summarise(
			num_controls_needed = ccr*dplyr::n(),
			case_pids = list(.data$person_id),
			.groups = 'drop'
		)
}

#' Calculate available controls for each matching group
get_control_supply = function(data, case_ids, match_vars) {
	data %>%
		dplyr::filter(.data$person_id %nin% case_ids) %>%
		dplyr::group_by(!!!rlang::syms(match_vars)) %>%
		dplyr::summarise(
			num_controls_avail = dplyr::n(),
			avail_control_pids = list(.data$person_id),
			.groups = 'drop'
		)
}

#' Check if matching is feasible and return details of any problems
check_matching_feasibility = function(demand_df, supply_df, match_vars) {
	matching_check = demand_df %>%
		dplyr::left_join(supply_df, by = dplyr::join_by(!!!rlang::syms(match_vars))) %>%
		dplyr::mutate(
			deficit = num_controls_needed - num_controls_avail,
			matching_group = paste(!!!rlang::syms(match_vars), sep = ", ")
		)

	problem_groups = matching_check %>%
		dplyr::filter(deficit > 0) %>%
		dplyr::arrange(desc(deficit))

	if(nrow(problem_groups) > 0) {
		list(
			feasible = FALSE,
			problem_groups = problem_groups
		)
	} else {
		list(
			feasible = TRUE,
			matching_data = matching_check
		)
	}
}

#' Sample controls for each matching group
sample_controls = function(matching_data) {
	set.seed(1)
	matching_data %>%
		dplyr::mutate(
			sampled_controls = purrr::map2(
				.x = avail_control_pids,
				.y = num_controls_needed,
				.f = function(x, y) sample(x, size = y, replace = FALSE)
			)
		) %>%
		dplyr::pull(sampled_controls) %>%
		unlist()
}

#' Main function to get control IDs
#' @export
get_control_ids = function(case_ids,
													 match_variables,
													 data,
													 exclude_ids = NULL,
													 control_case_ratio = 5) {
	# Input validation
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
		data = dplyr::filter(data, .data$person_id %nin% exclude_ids)
	}

	if (!all(match_variables %in% names(data))) {
		stop("Some match_variable's aren't in data.")
	}

	# Get demand and supply
	demand_df = get_control_demand(data, case_ids, match_variables, control_case_ratio)
	supply_df = get_control_supply(data, case_ids, match_variables)

	# Check feasibility
	feasibility = check_matching_feasibility(demand_df, supply_df, match_variables)

	if(!feasibility$feasible) {
		error_msg = sprintf(
			"Insufficient controls for %d matching groups:\n%s",
			nrow(feasibility$problem_groups),
			paste(sprintf(
				"  Group '%s': needs %d controls, only %d available",
				feasibility$problem_groups$matching_group,
				feasibility$problem_groups$num_controls_needed,
				feasibility$problem_groups$num_controls_avail
			), collapse = "\n")
		)
		stop(error_msg)
	}

	# Sample controls
	sample_controls(feasibility$matching_data)
}
