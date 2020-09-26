######################

source("setup.R")
source("colours.R")

### Recover dataset ###

pof_data <- 
  readr::read_rds("00_prepare_data/mobapp_individuo.rds")

# Plot 5: frequency and consumption in urban areas ------------------

# Frequency ------------

plot5_estrato <- pof_data %>% 
  group_by(Estrato, casa, Modo) %>% 
  summarise(frequencia = weighted.mean(despesas_mes, PESO_FINAL)) %>% 
  filter(Modo == 'Ride-hailing')

p1 <-
plot5_estrato %>% 
  filter(Estrato != 'Interior Rural') %>% 
  ggplot(aes(frequencia, reorder(Estrato,frequencia))) +
  geom_path(aes(group = Estrato), linetype = 'dotted') +
  geom_point(
    aes(fill = as.factor(casa)),shape = 21, size = 3.5, alpha = 1) +
scale_fill_aop() +
  labs(fill = "", y = "", x = "Nº médio de viagens por mês") +
  theme_minimal() +
  theme(
    legend.position = 'top',
    axis.title.y = element_text(angle = 0))

# Consumption ---------------------

# Estrato ------

plot5b_estrato <- pof_data %>% 
  group_by(Estrato,Modo) %>% 
  mutate(media = weighted.mean(gasto_avg, PESO_FINAL)) %>% 
  group_by(quintil_renda, Estrato, Modo,media) %>% 
  summarise(gasto = weighted.mean(gasto_avg, PESO_FINAL)) %>% 
  filter(Modo == 'Ride-hailing') %>% 
  ungroup()

p2 <-
  plot5b_estrato %>% filter(Estrato!='Interior Rural') %>% 
  ggplot(aes(as.factor(quintil_renda), gasto, group = Estrato)) +
  geom_hline(aes(color = Estrato,yintercept=media),linetype = 'dotted',size=1.1)+
  geom_line(aes(color = Estrato),size=1.1) +
  geom_point(aes(fill = Estrato), size=3.5, shape=21)+
scale_colour_aop() +
scale_fill_aop() +
  labs(x='Quintil de renda', y='Valor médio da viagem (R$)', color = 'Estrato', fill='Estrato') +
  theme_minimal()

# Housetype ------

plot5b_casa <- pof_data %>% 
  group_by(casa,Modo) %>% 
  mutate(media = weighted.mean(gasto_avg, PESO_FINAL)) %>% 
  group_by(quintil_renda, casa, Modo,media) %>% 
  summarise(gasto = weighted.mean(gasto_avg, PESO_FINAL)) %>% 
  filter(Modo == 'Ride-hailing') %>% 
  ungroup()

p3<-
  plot5b_casa %>% filter(casa!='Habitação Irregular') %>% 
  ggplot(aes(as.factor(quintil_renda), gasto, group = casa)) +
  geom_hline(aes(color = casa,yintercept=media),linetype = 'dotted',size=1.1)+
  geom_line(aes(color = casa),size=1.1) +
  geom_point(aes(fill = casa), size=3.5, shape=21)+
scale_colour_aop() +
scale_fill_aop() +
  labs(x='Quintil de renda', y='Valor médio da viagem (R$)', color = 'Habitação', fill='Habitação') +
  theme_minimal()

# Plot composition --------------

p<-p1|(p2/p3)

p + plot_annotation(tag_levels = 'A')

ggsave("plot5.png", path = "01_data_viz/02.1_perfil_sociodemografico/img")
rm(list = ls())

