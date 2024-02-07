# # old dx_code_scan
# turns out this is much faster than the fisher_phewas function
phewas_case_control = function(case_pids, control_pids, phecode_counts) {

  phecode_counts %>%
    filter(person_id %in% c(case_pids, control_pids)) %>%
    mutate(case_or_control = ifelse(person_id %in% case_pids, 'case', 'control')) %>%
    select(person_id, case_or_control, phecode, phecode_count) %>%
    group_by(phecode) %>%
    summarise(
      case_w = sum(phecode_count >= 2 & case_or_control == 'case'),
      case_wo = length(case_pids) - sum(phecode_count >= 1 & case_or_control == 'case'),
      control_w = sum(phecode_count >= 2 & case_or_control == 'control'),
      control_wo = length(control_pids) - sum(phecode_count >= 1 & case_or_control == 'control')
    ) ->
    dx_code_scan

  ms = list()
  ors = list()
  for (i in 1:nrow(dx_code_scan)) {
    ms[[i]] = matrix(
      c(dx_code_scan$control_wo[i] + 1,
        dx_code_scan$control_w[i] + 1,
        dx_code_scan$case_wo[i] + 1,
        dx_code_scan$case_w[i]) + 1,
      nrow = 2, ncol = 2)
    ors[[i]] = fisher.test(ms[[i]])
  }

  dx_code_scan$or = unlist(map(ors, 'estimate'))
  dx_code_scan$or_low = unlist(map(map(ors, 'conf.int'), function(x) { x[1] } ))
  dx_code_scan$or_hi = unlist(map(map(ors, 'conf.int'), function(x) { x[2] } ))
  dx_code_scan$p_val = unlist(map(ors, 'p.value'))

  return(dx_code_scan)
}
