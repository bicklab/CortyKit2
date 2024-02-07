match_on = function(case_pids, demog_plus, ccr, ...) {

  demog_plus %>%
    filter(person_id %in% case_pids) %>%
    group_by(...) %>%
    summarise(
      num_cases = n(),
      num_controls_needed = ccr*num_cases,
      case_pids = list(person_id),
      .groups = 'drop'
    ) ->
    control_demand_df

  demog_plus %>%
    filter(person_id %nin% case_pids) %>%
    group_by(...) %>%
    summarise(
      num_controls_avail = n(),
      avail_control_pids = list(person_id),
      .groups = 'drop'
    ) ->
    control_supply_df

  set.seed(1)

  control_demand_df %>%
    left_join(control_supply_df, by = join_by(...)) %>%
    # can use this to figure out if matching will succeed
    # mutate(deficit = num_controls_needed - num_controls_avail) %>% filter(deficit > 0) %>% arrange(deficit)
    mutate(control_pids = map2(.x = avail_control_pids, .y = num_controls_needed, .f = sample, replace = FALSE)) %>%
    pull(control_pids) %>% unlist() ->
    control_pids

  return(control_pids)
}


get_control_ids = function(case_ids, demog_plus, exclude_pids = NULL, control_case_ratio = 10, verbose = FALSE) {

  if (!is.null(exclude_pids)) {
    if (!purrr::is_empty(intersect(case_pids, exclude_pids))) {
      stop('there is overlap between cases and excludes')
    }
    demog_plus = filter(demog_plus, person_id %nin% exclude_pids)
  }

  mo = possibly(
    .f = function(...) {
      match_on(case_pids, demog_plus, ccr = control_case_ratio, ...)
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
