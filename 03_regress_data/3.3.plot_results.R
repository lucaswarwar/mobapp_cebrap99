### Setup ###

source("setup.R")

### Recover dataset #####################################################

df_age <- data.table::fread('03_regress_data/results/df_age.csv')
df_sex_race <- data.table::fread('03_regress_data/results/df_sex_race.csv')
df_strata <- data.table::fread('03_regress_data/results/df_strata.csv')
df_city <- data.table::fread('03_regress_data/results/df_city.csv')

### Plot results ----------

df_age %>%
  ggplot( aes(x = oddsratio, y = groups  )) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = 'dashed') +
  geom_linerange(
    aes(xmax = int97.5, xmin = int2.5, color = oddsratio), size = 1.05) +
  geom_point(aes(fill=oddsratio), size = 4,shape = 21) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1) +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0, 4), breaks = seq(-1, 4, 1)) +
  labs(
    title = 'Reference group: 15-24 years old',
    y = "", x = 'Odds ratio',
    color = 'Odds \nratio',fill = 'Odds \nratio') +
  facet_wrap(~transport )

ggsave('plot_age.png', path = '03_regress_data/img/')

df_sex_race %>%
  ggplot( aes(x = oddsratio, y = groups  )) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = 'dashed') +
  geom_linerange(
    aes(xmax = int97.5, xmin = int2.5, color = oddsratio), size = 1.05) +
  geom_point(aes(fill=oddsratio), size = 4,shape = 21) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1) +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0, 6.5), breaks = seq(-1, 6.5, 1)) +
  labs(
    title = 'Reference group: Homem Branco',
    y = "", x = 'Odds ratio',
    color = 'Odds \nratio',fill = 'Odds \nratio') +
  facet_wrap(~transport )

ggsave('plot_sex_race.png', path = '03_regress_data/img/')

df_strata %>%
  ggplot( aes(x = oddsratio, y = groups  )) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = 'dashed') +
  geom_linerange(
    aes(xmax = int97.5, xmin = int2.5, color = oddsratio), size = 1.05) +
  geom_point(aes(fill=oddsratio), size = 4,shape = 21) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1) +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0, 6.5), breaks = seq(-1, 6.5, 1)) +
  labs(
    title = 'Reference group: Apartamento Capital',
    y = "", x = 'Odds ratio',
    color = 'Odds \nratio',fill = 'Odds \nratio') +
  facet_wrap(~transport )

ggsave('plot_strata.png', path = '03_regress_data/img/')

df_city %>%
  filter(transport=='transporte_ind'|transport=='transporte_pub') %>%
  ggplot( aes(x = oddsratio, y = groups  )) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = 'dashed') +
  geom_linerange(
    aes(xmax = int97.5, xmin = int2.5, color = oddsratio), size = 1.05) +
  geom_point(aes(fill=oddsratio), size = 4,shape = 21) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1) +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0, 5), breaks = seq(-1, 5, 1)) +
  labs(
    title = 'Reference group: Porto Alegre',
    y = "", x = 'Odds ratio',
    color = 'Odds \nratio',fill = 'Odds \nratio') +
  facet_wrap(~transport,nrow = 1 )

ggsave('plot_city.png', path = '03_regress_data/img/')

