# # old dx_code_scan
# turns out this is much faster than the fisher_phewas function
phewas_case_control = function(case_pids,
															 control_pids,
															 phecode_counts,
															 shrinkage_pseudocounts = 1) {

  phecode_counts %>%
		dplyr::filter(person_id %in% c(case_pids, control_pids)) %>%
		dplyr::mutate(case_or_control = ifelse(person_id %in% case_pids, 'case', 'control')) %>%
		dplyr::select(person_id, case_or_control, phecode, phecode_count) %>%
		dplyr::group_by(phecode) %>%
		dplyr::summarise(
      case_w = sum(phecode_count >= 2 & case_or_control == 'case'),
      case_wo = length(case_pids) - sum(phecode_count >= 1 & case_or_control == 'case'),
      control_w = sum(phecode_count >= 2 & case_or_control == 'control'),
      control_wo = length(control_pids) - sum(phecode_count >= 1 & case_or_control == 'control')
    ) ->
    dx_code_scan

  ms = list()
  ors = list()
  for (i in 1:nrow(dx_code_scan)) {
    ms[[i]] = base::matrix(
      c(dx_code_scan$control_wo[i],
        dx_code_scan$control_w[i],
        dx_code_scan$case_wo[i],
        dx_code_scan$case_w[i]),
      nrow = 2, ncol = 2) + shrinkage_pseudocounts
    ors[[i]] = stats::fisher.test(ms[[i]])
  }

  dx_code_scan$or = unlist(purrr::map_dbl(ors, 'estimate'))
  dx_code_scan$or_low = unlist(purrr::map_dbl(ors, function(x) { x[['conf.int']][1] } ))
  dx_code_scan$or_hi = unlist(purrr::map_dbl(ors, function(x) { x[['conf.int']][2] } ))
  dx_code_scan$p_val = unlist(purrr::map_dbl(ors, 'p.value'))

  return(dx_code_scan)
}
