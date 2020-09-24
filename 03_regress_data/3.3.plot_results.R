### Setup ###

source("setup.R")
source("colours.R")


### Recover dataset #####################################################

df_age <- data.table::fread('03_regress_data/results/df_age.csv')
df_sex_age <- data.table::fread('03_regress_data/results/df_sex_age.csv')
df_sex_race <- data.table::fread('03_regress_data/results/df_sex_race.csv')
df_race_age <- data.table::fread('03_regress_data/results/df_race_age.csv')
df_strata <- data.table::fread('03_regress_data/results/df_strata.csv')
df_city <- data.table::fread('03_regress_data/results/df_city2.csv')

### Plot results ----------

### Plot results ----------

age <-
  df_age[transport=='ride_hailing'] %>% 
  ggplot( aes(x = odds_ratio, y = reorder(group,odds_ratio)  )) +
  geom_vline(aes(xintercept = 1), size = .5, linetype = 'dashed') +
  geom_linerange(
    aes(xmax = int97.5, xmin = int2.5, color = odds_ratio), size = 1.1) +
  geom_point(aes(fill=odds_ratio), size = 4,shape = 21) +
  scale_fill_aop(palette = 'blue_red',discrete = F,labels = scales::percent) +
  scale_colour_aop(palette = 'blue_red',discrete = F,labels = scales::percent) +
  #paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  #paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 1.25), breaks = seq(-1, 1.25, .25)) +
  labs(
    title = 'Grupo de referência: 15-24 anos',
    y = "", x = 'Odds ratio',
    color = 'Odds \nratio',fill = 'Odds \nratio') +
  theme(
    legend.background = element_rect(fill=NA),
    legend.position = c('.9','.2'),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1)))

ggsave('plot_age.png',path = '03_regress_data/img')

sex_age <-
  df_sex_age[transport=='ride_hailing'] %>% 
  dplyr::mutate(group = gsub("_"," ",group)) %>% 
  ggplot( aes(x = odds_ratio, y = reorder(group,odds_ratio)  )) +
  geom_vline(aes(xintercept = 1), size = .5, linetype = 'dashed') +
  geom_linerange(
    aes(xmax = int97.5, xmin = int2.5, color = odds_ratio), size = 1.1) +
  geom_point(aes(fill=odds_ratio), size = 4,shape = 21) +
  scale_fill_aop(palette = 'blue_red',discrete = F,labels=scales::percent) +
  scale_colour_aop(palette = 'blue_red',discrete = F,labels=scales::percent) +
  #paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  #paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 2), breaks = seq(-1, 2, .25)) +
  labs(
    title = 'Grupo de referência: Homens, 15-24 anos',
    y = "", x = 'Odds ratio',
    color = 'Odds \nratio',fill = 'Odds \nratio') +
  theme(
    legend.background = element_rect(fill=NA),
    legend.position = c('.9','.2'),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1)))

ggsave('plot_sex_age.png',path = '03_regress_data/img')

p1<- age+sex_age
p1 + plot_annotation(tag_levels = 'A')

ggsave('plot_age_combo.png',path = '03_regress_data/img')


sex_race <-
  df_sex_race[transport=='ride_hailing'] %>% 
  dplyr::mutate(group = gsub("_"," ",group)) %>% 
  ggplot( aes(x = odds_ratio, y = reorder(group,odds_ratio)  )) +
  geom_vline(aes(xintercept = 1), size = .5, linetype = 'dashed') +
  geom_linerange(
    aes(xmax = int97.5, xmin = int2.5, color = odds_ratio), size = 1.1) +
  geom_point(aes(fill=odds_ratio), size = 4,shape = 21) +
  scale_fill_aop(palette = 'blue_red',discrete = F,labels=scales::percent) +
  scale_colour_aop(palette = 'blue_red',discrete = F,labels=scales::percent) +
  #paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  #paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 3.5), breaks = seq(-1, 3.5, .5)) +
  labs(
    title = 'Grupo de referência: Homem, Branco',
    y = "", x = 'Odds ratio',
    color = 'Odds \nratio',fill = 'Odds \nratio') +
  theme(
    legend.background = element_rect(fill=NA),
    legend.position = c('.9','.2'),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1)))

ggsave('plot_sex_race.png',path = '03_regress_data/img')

race_age <-
  df_race_age[transport=='ride_hailing'] %>% 
  dplyr::mutate(group = gsub("_"," ",group)) %>% 
  ggplot( aes(x = odds_ratio, y = reorder(group,odds_ratio)  )) +
  geom_vline(aes(xintercept = 1), size = .5, linetype = 'dashed') +
  geom_linerange(
    aes(xmax = int97.5, xmin = int2.5, color = odds_ratio), size = 1.1) +
  geom_point(aes(fill=odds_ratio), size = 4,shape = 21) +
  scale_fill_aop(palette = 'blue_red',discrete = F,labels=scales::percent) +
  scale_colour_aop(palette = 'blue_red',discrete = F,labels=scales::percent) +
  #paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  #paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 3), breaks = seq(-1, 3, .5)) +
  labs(
    title = 'Grupo de referência: Brancos, 15-24',
    y = "", x = 'Odds ratio',
    color = 'Odds \nratio',fill = 'Odds \nratio') +
  theme(
    legend.background = element_rect(fill=NA),
    legend.position = c('.9','.2'),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1)))

ggsave('plot_sex_race.png',path = '03_regress_data/img')

plot1 <- sex_race + race_age
plot1 + plot_annotation(tag_levels = 'A')

ggsave('plot_race_combo.png',path = '03_regress_data/img')

strata <-
  df_strata[transport=='ride_hailing'] %>% 
  dplyr::mutate(group = gsub("_"," ",group)) %>% 
  ggplot( aes(x = odds_ratio, y = reorder(group,odds_ratio)  )) +
  geom_vline(aes(xintercept = 1), size = .5, linetype = 'dashed') +
  geom_linerange(
    aes(xmax = int97.5, xmin = int2.5, color = odds_ratio), size = 1.1) +
  geom_point(aes(fill=odds_ratio), size = 4,shape = 21) +
  scale_fill_aop(palette = 'blue_red',discrete = F,labels=scales::percent) +
  scale_colour_aop(palette = 'blue_red',discrete = F,labels=scales::percent) +
  #paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  #paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 1.5), breaks = seq(-1, 1.5, .25)) +
  labs(
    title = 'Grupo de referência: Apartamento Capital',
    y = "", x = 'Odds ratio',
    color = 'Odds \nratio',fill = 'Odds \nratio') +
  theme(
    legend.background = element_rect(fill=NA),
    legend.position = c('.9','.2'),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = rel(.9)),
    axis.text = element_text(size = rel(.9)),
    legend.title = element_text(size = rel(.9)))

city <-
  df_city[transport=='ride_hailing'] %>% 
  dplyr::mutate(groups = gsub("_"," ",groups)) %>% 
  ggplot( aes(x = oddsratio, y = reorder(groups,oddsratio)  )) +
  geom_vline(aes(xintercept = 1), size = .5, linetype = 'dashed') +
  geom_linerange(
    aes(xmax = int97.5, xmin = int2.5, color = oddsratio), size = 1.1) +
  geom_point(aes(fill=oddsratio), size = 4,shape = 21) +
  scale_fill_aop(palette = 'blue_red',discrete = F,labels=scales::percent) +
  scale_colour_aop(palette = 'blue_red',discrete = F,labels=scales::percent) +
  #paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  #paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,labels=scales::percent) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 16), breaks = seq(-1, 16, 2)) +
  labs(
    title = 'Grupo de referência: Brasil Urbano',
    y = "", x = 'Odds ratio',
    color = 'Odds \nratio',fill = 'Odds \nratio') +
  theme(
    legend.background = element_rect(fill=NA),
    legend.position = c('.9','.2'),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = rel(.9)),
    axis.text = element_text(size = rel(.9)),
    legend.title = element_text(size = rel(.9)))

plot2 <- strata+city
plot2 + plot_annotation(tag_levels = 'A')
ggsave('plot_city_combo.png',path = '03_regress_data/img')
