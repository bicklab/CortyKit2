#' @title make prevalence plot
#'
#' @param cases tibble of cases
#' @param demog tibble of demographic info
#'
#' @return the plot
#' @export
make_prevalence_plot = function(cases, demog) {

  demog %>%
    dplyr::filter(sex_at_birth %in% c('Male', 'Female')) %>%
		dplyr::mutate(has_sm = person_id %in% cases$person_id) %>%
		dplyr::group_by(decade_at_biosample, sex_at_birth) %>%
		dplyr::summarise(p_sm = mean(has_sm), n = dplyr::n()) %>%
		dplyr::filter(n > 10) %>%
		dplyr::mutate(decade_at_biosample = factor(decade_at_biosample, levels = as.character(seq(20, 100, 10)))) %>%
		ggplot2::ggplot(ggplot2::aes(x = decade_at_biosample, y = p_sm)) +
		ggplot2::geom_bar(aes(fill = sex_at_birth), stat = 'identity', position = 'dodge') +
		ggplot2::scale_y_continuous(name = 'prevalence of\nsomatic mutation',
																labels = scales::number_format()) +
		ggplot2::labs(fill = 'sex', x = 'decade at biosample') +
		ggplot2::scale_fill_brewer(type = 'qual') +
		ggplot2::theme_bw()
}

#' @title make age-vaf plot
#'
#' @param cases tibble of cases
#'
#' @return the plot
#' @export
make_age_VAF_plot = function(cases) {

  cases %>%
    dplyr::mutate(genetic_ancestry = factor(
      x = ancestry_pred,
      levels = c('eur', 'afr', 'amr', 'eas', 'sas'),
      labels = c('European', 'African / African American', 'American Admixed / Latino', 'East Asian', 'South Asian')
    )) %>%
		ggplot2::ggplot(ggplot2::aes(x = age_at_biosample, y = AF_main)) +
		ggplot2::geom_point(ggplot2::aes(color = ancestry_pred, shape = dragen_sex_ploidy)) +
		ggplot2::theme_bw() +
		ggplot2::scale_x_continuous(name = 'age at blood draw') +
		ggplot2::scale_y_continuous(name = 'variant allele fraction', labels = scales::percent_format()) +
		ggplot2::scale_color_brewer(
      name = 'genetic ancestry',
      palette = 'Set1',

    ) +
		ggplot2::scale_shape_discrete(name = 'genetic sex')
}

#' @title make position-vaf plot
#'
#' @param cases tibble of cases
#'
#' @return the plot
#' @export
make_pos_VAF_plot = function(cases) {

  cases %>%
    dplyr::mutate(genetic_ancestry = factor(
      x = ancestry_pred,
      levels = c('eur', 'afr', 'amr', 'eas', 'sas'),
      labels = c('European', 'African / African American', 'American Admixed / Latino', 'East Asian', 'South Asian')
    )) %>%
		ggplot2::ggplot(ggplot2::aes(x = pos, y = AF_main)) +
		ggplot2::geom_point(ggplot2::aes(color = ancestry_pred, shape = dragen_sex_ploidy)) +
		ggplot2::theme_bw() +
		ggplot2::scale_x_continuous(name = 'genomic position') +
		ggplot2::scale_y_continuous(name = 'variant allele fraction', labels = scales::percent_format()) +
		ggplot2::scale_color_brewer(
      name = 'genetic ancestry',
      palette = 'Set1',

    ) +
		ggplot2::scale_shape_discrete(name = 'genetic sex')
}
