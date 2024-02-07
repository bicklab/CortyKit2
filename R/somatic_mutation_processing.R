conditionally <- function(fun){
  function(first_arg, ..., execute){
    if(execute) return(fun(first_arg, ...))
    else return(first_arg)
  }
}

cond_filter <- conditionally(dplyr::filter)

filter_sm_one_gene = function(sm_table, demog) {

  stopifnot(lu(sm_table$Gene.refGene) == 1)
  stopifnot(lu(sm_table$Chr) == 1)

  is_on_X = sm_table$Chr[1] == 'chrX'

  sm_table %>%
  	dplyr::mutate(
      binom_p_val = purrr::map2_dbl(.x = maxAD, .y = minAD, .f = binom_p_val),
      person_id = readr::parse_number(Sample)
    ) %>%
    tidyr::unnest(binom_p_val) %>%
    dplyr::select(
      person_id, v,
      chr = Chr, pos = Start, ref = Ref, alt = Alt,
      gene_name = Gene.refGene, func = Func.refGene,
      DP, AD, F1R2, F2R1, SB, AF_main, AF_alt, binom_p_val
    ) %>%
    dplyr::inner_join(demog_plus, by = 'person_id') %>%
  	dplyr::mutate(dragen_sex_ploidy = dplyr::case_when(
      is.na(dragen_sex_ploidy) & sex_at_birth == 'Male' ~ 'XY',
      is.na(dragen_sex_ploidy) & sex_at_birth == 'Female' ~ 'XX',
      TRUE ~ dragen_sex_ploidy)) %>%
    # filter out SM with VAF near 50%, with two exceptions
    # if multiallelic SM, keep it
    # if on X, keep all men
    cond_filter(execute = is_on_X,  binom_p_val < 0.01 | !is.na(AF_alt) | dragen_sex_ploidy == 'XY') %>%
    cond_filter(execute = !is_on_X, binom_p_val < 0.01 | !is.na(AF_alt))
}

