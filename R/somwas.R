# somwas = function(case_ids, control_ids, somatic_variants) {
#
# 	somatic_variants |>
# 		filter(person_id %in% c(case_ids, control_ids)) ->
# 		svs
#
#
#
# 	for (this_gene in unique(somatic_variants$gene_name)) {
#
# 		svs |>
# 			filter(gene_name == this_gene) ->
# 			svs_this_gene
#
# 		glm(formula = is_case ~ sv_burden_this_gene,
# 				data = TODO)
#
# 	}
# }
