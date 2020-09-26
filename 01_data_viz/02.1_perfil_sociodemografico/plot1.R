### Setup ###

source("setup.R")
source("colours.R")

### Recover dataset ###

pof_data <- readr::read_rds("00_prepare_data/pof_mobapp.rds")

# Plot 1: % of people that consumes ride-hailing ----------------------------------------------

# I. Income -------------

plot1_renda <- pof_data %>% 
  dplyr::group_by(quintil_renda, Estrato) %>% 
  dplyr::mutate(n_quintil = n_distinct(ID_MORADOR)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(quintil_renda, Modo, Estrato) %>% 
  dplyr::summarise(prop = mean(n_distinct(ID_MORADOR)/n_quintil)) %>% 
  dplyr::filter(Modo == 'Ride-hailing') %>% 
  dplyr::filter(Estrato != "Interior Rural")

p1 <-  
plot1_renda  %>% 
  ggplot() +
  geom_path(
    aes(prop, as.factor(quintil_renda),group = quintil_renda), linetype = 'dotted') +
  geom_point(
    aes(prop, as.factor(quintil_renda), fill = as.factor(Estrato)),
    shape = 21, size = 3.5, alpha = 1) +
  scale_x_continuous(labels = scales::percent, limits = c(0,.1)) +
  scale_fill_aop() +
  labs(fill = "", y = "Quintil de Renda") +
  theme_minimal() +
  theme(
    legend.position = 'top',
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

# II. Age -------------

plot1_idade <- pof_data %>% 
  group_by(faixa_etaria, Estrato) %>% 
  mutate(n_quintil = n_distinct(ID_MORADOR)) %>% 
  ungroup() %>% 
  group_by(faixa_etaria, Modo, Estrato) %>% 
  summarise(prop = mean(n_distinct(ID_MORADOR)/n_quintil)) %>% 
  filter(Modo == 'Ride-hailing') %>% 
  filter(Estrato != "Interior Rural")

p2 <-  
  plot1_idade  %>% filter(faixa_etaria != "0-14") %>% 
  ggplot() +
  geom_path(
    aes(prop, as.factor(faixa_etaria),group = faixa_etaria),
    linetype = 'dotted') +
  geom_point(
    aes(prop, as.factor(faixa_etaria),
        fill = as.factor(Estrato)),
    shape = 21, size = 3.5, alpha = 1) +
  scale_x_continuous(labels = scales::percent, limits = c(0,.1)) +
  scale_fill_aop()+
  labs(fill = "", y = "Faixa Etária") +
  theme_minimal() +
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

# III. Gender -------------

plot1_sexo <- pof_data %>% 
  group_by(sexo, Estrato) %>% 
  mutate(n_quintil = n_distinct(ID_MORADOR)) %>% 
  ungroup() %>% 
  group_by(sexo, Modo, Estrato) %>% 
  summarise(prop = mean(n_distinct(ID_MORADOR)/n_quintil)) %>% 
  filter(Modo == 'Ride-hailing') %>% 
  filter(Estrato != "Interior Rural")

p3 <-  
  plot1_sexo  %>% 
  ggplot() +
  geom_path(
    aes(prop, as.factor(sexo),group = sexo),
    linetype = 'dotted') +
  geom_point(
    aes(prop, as.factor(sexo),
        fill = as.factor(Estrato)),
    shape = 21, size = 3.5, alpha = 1) +
  scale_x_continuous(labels = scales::percent, limits = c(0,.1)) +
  scale_fill_aop() +
  labs(fill = "", y = "Sexo") +
  theme_minimal() +
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

# IV. Race -------------

plot1_cor <- pof_data %>% 
  group_by(cor, Estrato) %>% 
  mutate(n_quintil = n_distinct(ID_MORADOR)) %>% 
  ungroup() %>% 
  group_by(cor, Modo, Estrato) %>% 
  summarise(prop = mean(n_distinct(ID_MORADOR)/n_quintil)) %>% 
  filter(Modo == 'Ride-hailing') %>% 
  filter(Estrato != "Interior Rural")

p4 <-  
  plot1_cor  %>% 
  ggplot() +
  geom_path(
    aes(prop, as.factor(cor),group = cor),
    linetype = 'dotted') +
  geom_point(
    aes(prop, as.factor(cor),
        fill = as.factor(Estrato)),
    shape = 21, size = 3.5, alpha = 1) +
  scale_x_continuous(labels = scales::percent, limits = c(0,.1)) +
  scale_fill_aop() +
  labs(fill = "", y = "Cor", x="% do total") +
  theme_minimal() +
  theme(
    legend.position = 'none',
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.title.y = element_blank())

# Plot composition --------------------------------------

p <- p1/p2/p3/p4

p + patchwork::plot_annotation(tag_levels = "A")

ggsave("plot1.png", path = "01_data_viz/02.1_perfil_sociodemografico/img")
rm(list = ls()) 

