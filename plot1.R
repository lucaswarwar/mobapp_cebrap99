library(tidyverse)
library(patchwork)

plot1_renda <-
# proporção de pessoas que consomem transporte que fazem algum uso de app
pof_z %>% 
  group_by(quintil_renda, Estrato) %>% 
  mutate(n_quintil = n_distinct(ID_MORADOR)) %>% 
  ungroup() %>% 
  group_by(quintil_renda, Modo, Estrato) %>% 
  summarise(prop = mean(n_distinct(ID_MORADOR)/n_quintil)) %>% 
  filter(Modo == 'Aplicativo') %>% 
  filter(Estrato != "Interior Rural")

p1<-  
plot1_renda  %>% 
  ggplot() +
  geom_path(
    aes(prop, as.factor(quintil_renda),group = quintil_renda),
    linetype = 'dotted') +
  geom_point(
    aes(prop, as.factor(quintil_renda),
        fill = as.factor(Estrato)),
    shape = 21, size = 4.5, alpha = 1) +
  scale_x_continuous(labels = scales::percent, limits = c(0,.12)) +
  ggsci::scale_fill_locuszoom() +
  labs(fill = "", y = "Quintil de Renda") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'top',
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0))

plot1_idade <-
  # proporção de pessoas que consomem transporte que fazem algum uso de app
  pof_z %>% 
  group_by(faixa_etaria, Estrato) %>% 
  mutate(n_quintil = n_distinct(ID_MORADOR)) %>% 
  ungroup() %>% 
  group_by(faixa_etaria, Modo, Estrato) %>% 
  summarise(prop = mean(n_distinct(ID_MORADOR)/n_quintil)) %>% 
  filter(Modo == 'Aplicativo') %>% 
  filter(Estrato != "Interior Rural")

p2<-  
  plot1_idade  %>% 
  ggplot() +
  geom_path(
    aes(prop, as.factor(faixa_etaria),group = faixa_etaria),
    linetype = 'dotted') +
  geom_point(
    aes(prop, as.factor(faixa_etaria),
        fill = as.factor(Estrato)),
    shape = 21, size = 4.5, alpha = 1) +
  scale_x_continuous(labels = scales::percent, limits = c(0,.12)) +
  ggsci::scale_fill_locuszoom() +
  labs(fill = "", y = "Faixa Etária") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0))

plot1_sexo <-
  # proporção de pessoas que consomem transporte que fazem algum uso de app
  pof_z %>% 
  group_by(sexo, Estrato) %>% 
  mutate(n_quintil = n_distinct(ID_MORADOR)) %>% 
  ungroup() %>% 
  group_by(sexo, Modo, Estrato) %>% 
  summarise(prop = mean(n_distinct(ID_MORADOR)/n_quintil)) %>% 
  filter(Modo == 'Aplicativo') %>% 
  filter(Estrato != "Interior Rural")

p3<-  
  plot1_sexo  %>% 
  ggplot() +
  geom_path(
    aes(prop, as.factor(sexo),group = sexo),
    linetype = 'dotted') +
  geom_point(
    aes(prop, as.factor(sexo),
        fill = as.factor(Estrato)),
    shape = 21, size = 4.5, alpha = 1) +
  scale_x_continuous(labels = scales::percent, limits = c(0,.12)) +
  ggsci::scale_fill_locuszoom() +
  labs(fill = "", y = "Sexo") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0))

plot1_cor <-
  # proporção de pessoas que consomem transporte que fazem algum uso de app
  pof_z %>% 
  group_by(cor, Estrato) %>% 
  mutate(n_quintil = n_distinct(ID_MORADOR)) %>% 
  ungroup() %>% 
  group_by(cor, Modo, Estrato) %>% 
  summarise(prop = mean(n_distinct(ID_MORADOR)/n_quintil)) %>% 
  filter(Modo == 'Aplicativo') %>% 
  filter(Estrato != "Interior Rural")

p4<-  
  plot1_cor  %>% 
  ggplot() +
  geom_path(
    aes(prop, as.factor(cor),group = cor),
    linetype = 'dotted') +
  geom_point(
    aes(prop, as.factor(cor),
        fill = as.factor(Estrato)),
    shape = 21, size = 4.5, alpha = 1) +
  scale_x_continuous(labels = scales::percent, limits = c(0,.12)) +
  ggsci::scale_fill_locuszoom() +
  labs(fill = "", y = "Cor", x="% do total") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'none',
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.title.y = element_text(angle = 0))

p <- p1/p2/p3/p4

p
=
  

