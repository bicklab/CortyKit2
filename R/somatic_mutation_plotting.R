
make_prevalence_plot = function(cases, demog) {

  demog %>%
    filter(sex_at_birth %in% c('Male', 'Female')) %>%
    mutate(has_sm = person_id %in% cases$person_id) %>%
    group_by(decade_at_biosample, sex_at_birth) %>%
    summarise(p_sm = mean(has_sm), n = n()) %>%
    filter(n > 10) %>%
    mutate(decade_at_biosample = factor(decade_at_biosample, levels = as.character(seq(20, 100, 10)))) %>%
    ggplot(aes(x = decade_at_biosample, y = p_sm)) +
    geom_bar(aes(fill = sex_at_birth), stat = 'identity', position = 'dodge') +
    scale_y_continuous(name = 'prevalence of\nsomatic mutation', labels = scales::number_format()) +
    labs(fill = 'sex', x = 'decade at biosample') +
    scale_fill_brewer(type = 'qual') +
    theme_bw()
}

make_age_VAF_plot = function(cases) {

  cases %>%
    mutate(genetic_ancestry = factor(
      x = ancestry_pred,
      levels = c('eur', 'afr', 'amr', 'eas', 'sas'),
      labels = c('European', 'African / African American', 'American Admixed / Latino', 'East Asian', 'South Asian')
    )) %>%
    ggplot(aes(x = age_at_biosample, y = AF_main)) +
    geom_point(aes(color = ancestry_pred, shape = dragen_sex_ploidy)) +
    theme_bw() +
    scale_x_continuous(name = 'age at blood draw') +
    scale_y_continuous(name = 'variant allele fraction', labels = scales::percent_format()) +
    scale_color_brewer(
      name = 'genetic ancestry',
      palette = 'Set1',

    ) +
    scale_shape_discrete(name = 'genetic sex')
}

make_pos_VAF_plot = function(cases) {

  cases %>%
    mutate(genetic_ancestry = factor(
      x = ancestry_pred,
      levels = c('eur', 'afr', 'amr', 'eas', 'sas'),
      labels = c('European', 'African / African American', 'American Admixed / Latino', 'East Asian', 'South Asian')
    )) %>%
    ggplot(aes(x = pos, y = AF_main)) +
    geom_point(aes(color = ancestry_pred, shape = dragen_sex_ploidy)) +
    theme_bw() +
    scale_x_continuous(name = 'genomic position') +
    scale_y_continuous(name = 'variant allele fraction', labels = scales::percent_format()) +
    scale_color_brewer(
      name = 'genetic ancestry',
      palette = 'Set1',

    ) +
    scale_shape_discrete(name = 'genetic sex')
}
