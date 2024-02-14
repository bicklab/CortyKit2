match_on = function(case_ids, covariates, ccr, ...) {

  covariates %>%
    dplyr::filter(person_id %in% case_ids) %>%
		dplyr::group_by(...) %>%
		dplyr::summarise(
      num_cases = dplyr::n(),
      num_controls_needed = ccr*num_cases,
      case_pids = list(person_id),
      .groups = 'drop'
    ) ->
    control_demand_df

  covariates %>%
  	dplyr::filter(person_id %nin% case_ids) %>%
  	dplyr::group_by(...) %>%
  	dplyr::summarise(
      num_controls_avail = dplyr::n(),
      avail_control_pids = list(person_id),
      .groups = 'drop'
    ) ->
    control_supply_df

  set.seed(1)

  control_demand_df %>%
  	dplyr::left_join(control_supply_df, by = dplyr::join_by(...)) %>%
    # can use this to figure out if matching will succeed
    # mutate(deficit = num_controls_needed - num_controls_avail) %>% filter(deficit > 0) %>% arrange(deficit)
  	mutate(control_pids = purrr::map2_chr(.x = avail_control_pids,
  																				.y = num_controls_needed,
  																				.f = sample, replace = FALSE)) %>%
    pull(control_pids) %>% unlist() ->
    control_pids

  return(control_pids)
}


#' get control ids
#'
#' @param case_ids the case ids
#' @param covariates covariates, some of which will be matched on
#' @param exclude_ids ids that user does not want to consider as possible controls
#' @param control_case_ratio number of controls per case, default to 10
#' @param verbose whether to output what ends up getting matched on
#'
#' @return a vector of control ids
#' @export
get_control_ids = function(case_ids, covariates, exclude_ids = NULL, control_case_ratio = 10, verbose = FALSE) {

  if (!is.null(exclude_ids)) {
    if (!purrr::is_empty(base::intersect(case_pids, exclude_ids))) {
      stop('there is overlap between cases and excludes')
    }
    covariates = dplyr::filter(covariates, person_id %nin% exclude_ids)
  }

  mo = purrr::possibly(
    .f = function(...) {
      match_on(case_pids, covariates, ccr = control_case_ratio, ...)
    },
    otherwise = 'nomatch'
  )

  # try very stringent matching
  m = mo(age_at_biosample, dragen_sex_ploidy, ancestry_pred)
  if (!identical(m, 'nomatch')) {
    if (verbose) { cat('matched on age, sex, and ancestry\n') }
    return(m)
  }

  # try sequentially less-stringent matching until one works
  m = mo(decade_at_biosample, dragen_sex_ploidy, ancestry_pred)
  if (!identical(m, 'nomatch')) {
    if (verbose) { cat('matched on decade, sex, and ancestry\n') }
    return(m)
  }

  m = mo(age_at_biosample, dragen_sex_ploidy)
  if (!identical(m, 'nomatch')) {
    if (verbose) { cat('matched on age and sex (no ancestry)\n') }
    return(m)
  }

  m = mo(decade_at_biosample, dragen_sex_ploidy)
  if (!identical(m, 'nomatch')) {
    if (verbose) { cat('matched on decade and sex (no ancestry)\n') }
    return(m)
  }

  return('nomatch')
}
