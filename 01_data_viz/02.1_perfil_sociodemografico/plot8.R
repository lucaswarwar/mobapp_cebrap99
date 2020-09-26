######################

source("setup.R")
source("colours.R")

### Recover dataset ###

pof_data <- 
  readr::read_rds("00_prepare_data/mobapp_individuo.rds")

# Plot 8: frequency and consumption by city ---------------------------------

# I. frequncy ----------------------------------

plot8_frequencia <-
  pof_data %>% 
  filter(Modo == 'Ride-hailing') %>% 
  group_by(RM) %>% 
  mutate(media = weighted.mean(despesas_mes,PESO_FINAL)) %>% 
  group_by(RM, Estrato) %>% 
  summarise(
    frequencia = weighted.mean(despesas_mes,PESO_FINAL),
    media = weighted.mean(media,PESO_FINAL)) %>% 
  filter(
    RM == 'Recife' |RM == 'São Paulo' |RM == 'Salvador' |
    RM == 'Brasília' |RM == 'Porto Alegre' |RM == 'Rio de Janeiro' |
    RM == 'Belo Horizonte' |RM == 'Fortaleza' |RM == 'Manaus' |
    RM == 'Campo Grande'|RM == 'Goiânia'|RM == 'Cuiabá')

p1 <-
  plot8_frequencia  %>% 
  ggplot(aes(frequencia, reorder(RM,media))) +
  geom_path(aes(group = RM),linetype = 'dotted') +
  geom_point(
    aes(fill = as.factor(Estrato)), shape = 21, size = 3.5, alpha = 1) +
  #geom_point(
   # aes(media, reorder(RM,media)),
    #shape = 21, size = 3.5, alpha = 1, fill = 'grey20') +
  scale_fill_aop() +
  labs(fill = "", y = "",x='Nº médio de viagens por mês') +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'top',
    #axis.text.x = element_blank(),
    #axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0))

# II. consumption ----------------------------------

plot8_gasto <-
  pof_data %>% 
  filter(Modo == 'Ride-hailing') %>% 
  group_by(RM) %>% 
  mutate(media = weighted.mean(gasto_avg,PESO_FINAL)) %>% 
  group_by(RM, Estrato) %>% 
  summarise(
    gasto = weighted.mean(gasto_avg,PESO_FINAL),
    media = weighted.mean(media,PESO_FINAL)) %>% 
  filter(
    RM == 'Recife' |RM == 'São Paulo' |RM == 'Salvador' |
      RM == 'Brasília' |RM == 'Porto Alegre' |RM == 'Rio de Janeiro' |
      RM == 'Belo Horizonte' |RM == 'Fortaleza' |RM == 'Manaus' |
      RM == 'Campo Grande'|RM == 'Goiânia'|RM == 'Cuiabá')

p2 <-
  plot8_gasto  %>% 
  ggplot(aes(gasto, reorder(RM,media))) +
  geom_path(aes(group = RM),linetype = 'dotted') +
  geom_point(
    aes(fill = as.factor(Estrato)), shape = 21, size = 3.5, alpha = 1) +
  #geom_point(
   # aes(media, reorder(RM,media)),
    #shape = 21, size = 3.5, alpha = 1, fill = 'grey20') +
  scale_fill_aop() +
  labs(fill = "", y = "",x='Custo médio da viagem (R$)') +
  scale_x_continuous(limits = c(10,35)) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'top',
    #axis.text.x = element_blank(),
    #axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0))

# Plot composition -----------------------------------

p<-p1|p2
p+plot_annotation(tag_levels = 'A')

ggsave("plot8.png", path = "01_data_viz/02.1_perfil_sociodemografico/img")
rm(list = ls())