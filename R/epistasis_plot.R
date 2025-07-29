# epistasis_plot = function(df) {
#
# 	df |>
# 		dplyr::mutate(snp1 = factor(snp1, labels = c('0', '1', '2')),
# 					 snp2 = factor(snp2, labels = c('0', '1', '2'))) |>
# 	ggplot2::ggplot(mapping = ggplot2::aes(x = snp1,
# 																color = snp2,
# 																y = trait_mean,
# 																ymin = trait_mean - 2*trait_sem,
# 																ymax = trait_mean + 2*trait_sem)) +
# 		ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.1)) +
# 		ggplot2::theme_minimal()
# }
#
#
# d = tibble::tribble(
# 	~snp1, ~snp2, ~trait_mean, ~trait_sem,
# 	0, 0, -1, 1,
# 	0, 1, 0, 1,
# 	0, 2, 1, 1,
# 	1, 0, 0, 1,
# 	1, 1, 0, 1,
# 	1, 2, 0, 1,
# 	2, 0, 1, 1,
# 	2, 1, 0, 1,
# 	2, 2, -1, 1
# )
