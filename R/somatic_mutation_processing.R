#' conditionally <- function(fun){
#'   function(first_arg, ..., execute){
#'     if(execute) return(fun(first_arg, ...))
#'     else return(first_arg)
#'   }
#' }
#'
#' cond_filter <- conditionally(dplyr::filter)
#'
#'
#' #' @title Filter SM one gene
#' #'
#' #' @param sm_table somatic mutation tibble with the following columns:
#' #' 'person_id' of type character contains person ID's
#' #' 'chr' of type character is the chromosome of the variant
#' #' 'pos' of type integer is the genomic position of the variant
#' #' 'ref' of type character is the reference allele
#' #' 'alt' of type character is the alternate allele
#' #' 'gene_name' of type character is the name of the gene the variants maps to
#' #' 'func' of type character is the functional significance of the variant
#' #'
#' #' 'DP' of type integer conatains total read depth
#' #' 'AD_1' of type integer contains read depth for most common allele
#' #' 'AD_2' of type integer contains read depth for second most common allele
#' #' 'AD_3' of type integer contains read depth for the third most commone allele or NA for the many bi-allelic variants
#' #' 'watson_2' of type integer is the forward read depth of the second most common allele
#' #' 'crick_2' of type integer is the reverse read depth of the second most common allele
#' #'
#' #'	person ID must be in a column named 'person_id' of type character
#' #'	read depth must be in a column named 'DP' of type integer
#' #'
#' #' @param demog demographics tibble with the following columns:
#' #' 'person_id' of type character contains person ID's
#' #' 'bio_sex' of type character which contains biological sex ('XX' or 'XY')
#' #'
#' #' @return filtered SM table
#' #' @export
#' #' @importFrom rlang .data
#' filter_sm_one_gene = function(sm_table, demog) {
#'
#'   stopifnot(lu(sm_table$Gene.refGene) == 1)
#'   stopifnot(lu(sm_table$Chr) == 1)
#'
#'   is_on_X = sm_table$Chr[1] == 'chrX'
#'
#'   sm_table %>%
#'   	dplyr::filter(
#'   		.data$DP >= 20,
#'   		.data$AD_2 >= 3,
#'   		.data$watson_2 >= 1,
#'   		.data$crick_2 >= 1) %>%
#'   	dplyr::mutate(
#'       binom_p_val = purrr::map2_dbl(
#'       	.x = .data$AD_1,
#'       	.y = .data$AD_2,
#'       	.f = binom_p_val)) %>%
#'     tidyr::unnest(binom_p_val) %>%
#'     # dplyr::select(
#'     #   person_id = .data$person_id,
#'     #   chr = .data$Chr,
#'     #   pos = .data$Start,
#'     #   ref = .data$Ref,
#'     #   alt = .data$Alt,
#'     #   gene_name = .data$Gene.refGene,
#'     #   func = .data$Func.refGene,
#'     #   DP = .data$DP,
#'     #   AD_1 = .data$AD_1,
#'     #   AD_2 = .data$AD_2,
#'     #   binom_p_val = .data$binom_p_val
#'     # ) %>%
#'     dplyr::inner_join(demog, by = 'person_id') %>%
#'   	# dplyr::mutate(
#'   	# 	dragen_sex_ploidy = dplyr::case_when(
#'   	# 		is.na(.data$dragen_sex_ploidy) & .data$sex_at_birth == 'Male' ~ 'XY',
#'   	# 		is.na(.data$dragen_sex_ploidy) & .data$sex_at_birth == 'Female' ~ 'XX',
#'   	# 		TRUE ~ dragen_sex_ploidy)) %>%
#'   	# filter out SM with VAF near 50%, with two exceptions
#'   	# if multiallelic SM, keep it
#'   	# if on X, keep all men
#'   	cond_filter(execute = is_on_X,  .data$binom_p_val < 0.01 | !is.na(.data$AD_3) | .data$bio_sex == 'XY') %>%
#'   	cond_filter(execute = !is_on_X, .data$binom_p_val < 0.01 | !is.na(.data$AD_3))
#' }
#'
