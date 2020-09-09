######################

source("00_setup.R.R")

### Recover dataset ###

pof_data <- 
  readr::read_rds("01_prepare_data/mobapp_individuo.rds")

# Plot 3: sociodemografics: frequency and consumption--------------------------

# 3.1 Frequency -------------------

# Age -------

plot3_idade <- pof_data %>% 
  group_by(quintil_renda, faixa_etaria, Modo) %>% 
  summarise(frequencia = weighted.mean(despesas_mes, PESO_FINAL)) %>% 
  filter(Modo == 'Ride-hailing')

p1a <-
plot3_idade %>% 
  filter(faixa_etaria != '0-14') %>% 
  ggplot() +
  geom_path(
    aes(frequencia, as.factor(faixa_etaria),group = faixa_etaria),
    linetype = 'dotted') +
  geom_point(
    aes(frequencia, as.factor(faixa_etaria),
        fill = as.factor(quintil_renda)),
    shape = 21, size = 4, alpha = 1) +
  scale_fill_brewer(palette = 'Spectral') +
  scale_x_continuous(limits = c(3,12)) +
  labs(fill = "Quintil de Renda") +
  theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

# Gender -------

plot3_sexo <- pof_data %>% 
  group_by(quintil_renda, sexo, Modo) %>% 
  summarise(frequencia = weighted.mean(despesas_mes, PESO_FINAL)) %>% 
  filter(Modo == 'Ride-hailing')

p1b <-
  plot3_sexo %>% 
  ggplot() +
  geom_path(
    aes(frequencia, as.factor(sexo),group = sexo),
    linetype = 'dotted') +
  geom_point(
    aes(frequencia, as.factor(sexo),
        fill = as.factor(quintil_renda)),
    shape = 21, size = 4, alpha = 1) +
  scale_fill_brewer(palette = 'Spectral') +
  scale_x_continuous(limits = c(3,12)) +
  theme_minimal() +
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

# Race -------

plot3_cor <- pof_data %>% 
  group_by(quintil_renda, cor, Modo) %>% 
  summarise(frequencia = weighted.mean(despesas_mes, PESO_FINAL)) %>% 
  filter(Modo == 'Ride-hailing')

p1c <-
  plot3_cor %>% 
  ggplot() +
  geom_path(
    aes(frequencia, as.factor(cor),group = cor),
    linetype = 'dotted') +
  geom_point(
    aes(frequencia, as.factor(cor),
        fill = as.factor(quintil_renda)),
    shape = 21, size = 4, alpha = 1) +
  scale_fill_brewer(palette = 'Spectral') +
  scale_x_continuous(limits = c(3,12)) +
  theme_minimal() +
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

s <-p1a/p1b/p1c
s

# 3.2 Consumption -------------------

# Gender ----------------------

plot3b_sexo <-
  pof_data %>% 
  group_by(quintil_renda, sexo, Modo) %>% 
  summarise(gasto = weighted.mean(gasto_avg, PESO_FINAL)) %>% 
  filter(Modo == 'Ride-hailing') %>% 
  group_by(sexo) %>% 
  mutate(media = mean(gasto)) %>% 
  ungroup()

p2a <-
  plot3b_sexo %>% 
  ggplot(aes(as.factor(quintil_renda), gasto, group = sexo)) +
  geom_hline(aes(color = sexo,yintercept=media),linetype = 'dotted',size=1.1)+
  geom_line(aes(color = sexo),size=1.1) +
  geom_point(aes(fill = sexo), size=3.5, shape=21)+
  ggsci::scale_color_locuszoom() +
  ggsci::scale_fill_locuszoom() +
  labs(x='Quintil de Renda', y='Valor médio da viagem (R$)', color = 'Sexo', fill='Sexo') +
  theme_minimal()

# Race ----------------------

plot3b_cor <-
  pof_data %>% 
  group_by(quintil_renda, cor, Modo) %>% 
  summarise(gasto = weighted.mean(gasto_avg, PESO_FINAL)) %>% 
  filter(Modo == 'Ride-hailing') %>% 
  group_by(cor) %>% 
  mutate(media = mean(gasto)) %>% 
  ungroup()

p2b <-
plot3b_cor %>% 
  filter(cor!='Amarela, Indígena ou outra') %>% 
  ggplot(aes(as.factor(quintil_renda), gasto, group = cor)) +
  geom_hline(aes(color = cor,yintercept=media),linetype = 'dotted',size=1.1)+
  geom_line(aes(color = cor),size=1.1) +
  geom_point(aes(fill = cor), size=3.5, shape=21)+
  ggsci::scale_color_locuszoom() +
  ggsci::scale_fill_locuszoom() +
  labs(x='Quintil de Renda', y='Valor médio da viagem (R$)', color = 'Cor', fill='Cor') +
  theme_minimal()

# Plot composition ---------------------
p<-s|(p2a/p2b)

p[[1]] <- p[[1]] + plot_layout(tag_level = 'new')

p + plot_annotation(tag_levels = c('A', '1'))

ggsave("plot3.png", path = "02_plot_data/02.1_perfil_sociodemografico/img")
rm(list = ls())