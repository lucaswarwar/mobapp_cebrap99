### Setup ###

source("setup.R")

### Recover dataset #####################################################

df_age <- data.table::fread('03_regress_data/results/df_age.csv')
df_sex_race <- data.table::fread('03_regress_data/results/df_sex_race.csv')
df_strata <- data.table::fread('03_regress_data/results/df_strata.csv')
df_city <- data.table::fread('03_regress_data/results/df_city.csv')

### Plot results ----------

### Plot results ----------

age <-
  df_age[transport=='ride_hailing'] %>% 
  ggplot( aes(x = odds_ratio, y = group  )) +
  geom_vline(aes(xintercept = 1), size = .5, linetype = 'dashed') +
  geom_linerange(
    aes(xmax = int97.5, xmin = int2.5, color = odds_ratio), size = 1.1) +
  geom_point(aes(fill=odds_ratio), size = 4,shape = 21) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 1.5), breaks = seq(-1, 1.5, .25)) +
  labs(
    title = 'Reference group: 15-24 years old',
    y = "", x = 'Odds ratio',
    color = 'Odds \nratio',fill = 'Odds \nratio') +
  theme(
    legend.background = element_rect(fill=NA),
    legend.position = c('.9','.8'),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1)))


sex_age <-
  df_sex_age[transport=='ride_hailing'] %>% 
  ggplot( aes(x = odds_ratio, y = group  )) +
  geom_vline(aes(xintercept = 1), size = .5, linetype = 'dashed') +
  geom_linerange(
    aes(xmax = int97.5, xmin = int2.5, color = odds_ratio), size = 1.1) +
  geom_point(aes(fill=odds_ratio), size = 4,shape = 21) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 2), breaks = seq(-1, 2, .25)) +
  labs(
    title = 'Reference group: Men 15-24 years old',
    y = "", x = 'Odds ratio',
    color = 'Odds \nratio',fill = 'Odds \nratio') +
  theme(
    legend.background = element_rect(fill=NA),
    legend.position = c('.9','.8'),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1)))


sex_race <-
  df_sex_race[transport=='ride_hailing'] %>% 
  ggplot( aes(x = odds_ratio, y = group  )) +
  geom_vline(aes(xintercept = 1), size = .5, linetype = 'dashed') +
  geom_linerange(
    aes(xmax = int97.5, xmin = int2.5, color = odds_ratio), size = 1.1) +
  geom_point(aes(fill=odds_ratio), size = 4,shape = 21) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 3.5), breaks = seq(-1, 3.5, .5)) +
  labs(
    title = 'Reference group: Men White',
    y = "", x = 'Odds ratio',
    color = 'Odds \nratio',fill = 'Odds \nratio') +
  theme(
    legend.background = element_rect(fill=NA),
    legend.position = c('.9','.8'),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1)))

race_age <-
  df_race_age[transport=='ride_hailing'] %>% 
  ggplot( aes(x = odds_ratio, y = group  )) +
  geom_vline(aes(xintercept = 1), size = .5, linetype = 'dashed') +
  geom_linerange(
    aes(xmax = int97.5, xmin = int2.5, color = odds_ratio), size = 1.1) +
  geom_point(aes(fill=odds_ratio), size = 4,shape = 21) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 3), breaks = seq(-1, 3, .5)) +
  labs(
    title = 'Reference group: White 15-24',
    y = "", x = 'Odds ratio',
    color = 'Odds \nratio',fill = 'Odds \nratio') +
  theme(
    legend.background = element_rect(fill=NA),
    legend.position = c('.9','.8'),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1)))

plot1 <-
  (age + sex_age) / (sex_race + race_age)

plot1
