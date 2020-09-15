######################

source("00_setup.R.R")

### Recover dataset ###

pof_data <- 
  readr::read_rds("01_prepare_data/mobapp_individuo.rds")

# Plot 2: sociodemografics of ride-hailing users---------------------------------

# I. Income ----------------------

plot2_renda <- pof_data %>% 
  group_by(Modo) %>% 
  mutate(total = n()) %>% 
  group_by(decil_renda, Modo) %>% 
  summarise(share = mean(n()/total)) %>% 
  na.omit()

p1 <-
  plot2_renda %>% filter(Modo != "Transporte Alternativo") %>% 
  ggplot(
    aes(as.factor(decil_renda), share, group = Modo)) +
  geom_line(aes(color = Modo), size = 1.1) +
  geom_point(aes(fill = Modo), shape = 21, size = 3.5) +
  ggsci::scale_color_locuszoom() +
  ggsci::scale_fill_locuszoom() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Decil de Renda", y="% dos usuários") +
  theme(legend.position = 'bottom')

# II. Age ----------------------

plot2_idade <- pof_data %>% 
  mutate(Modo = ifelse(Modo != "Ride-hailing", "População",Modo)) %>% 
  group_by(Modo) %>% 
  mutate(total = n()) %>% 
  group_by(faixa_etaria, Modo) %>% 
  summarise(share = mean(n()/total)) %>% 
  na.omit()

plot2_idade$Modo <- factor(plot2_idade$Modo, levels = c('Ride-hailing', 'População'))

p2 <-
  plot2_idade %>% filter(faixa_etaria != '0-14') %>% 
  ggplot(
    aes(as.factor(faixa_etaria), share)) +
  geom_col(aes(fill = Modo)) +
  ggsci::scale_fill_locuszoom() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Faixa Etária", y="") +
  theme(legend.position = 'none') +
  facet_wrap(~Modo, nrow = 1)

# III. Gender ----------------------

plot2_sexo <- pof_data %>% 
  mutate(Modo = ifelse(Modo != "Ride-hailing", "População",Modo)) %>% 
  group_by(Modo) %>% 
  mutate(total = n()) %>% 
  group_by(sexo, Modo) %>% 
  summarise(share = mean(n()/total)) %>% 
  na.omit()

plot2_sexo$Modo <- factor(plot2_sexo$Modo, levels = c('Ride-hailing', 'População'))

p3 <-
  plot2_sexo %>%  
  ggplot(
    aes(as.factor(sexo), share)) +
  geom_col(aes(fill = Modo)) +
  ggsci::scale_fill_locuszoom() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Sexo", y="") +
  theme(legend.position = 'none') +
  facet_wrap(~Modo, nrow = 1)

# IV. Race ----------------------

plot2_cor <- pof_data %>% 
  mutate(Modo = ifelse(Modo != "Ride-hailing", "População",Modo)) %>% 
  group_by(Modo) %>% 
  mutate(total = n()) %>% 
  group_by(cor, Modo) %>% 
  summarise(share = mean(n()/total)) %>% 
  na.omit()

plot2_cor$Modo <- factor(plot2_cor$Modo, levels = c('Ride-hailing', 'População'))

p4 <-
  plot2_cor %>% filter(cor != "Amarela, Indígena ou outra") %>% 
  ggplot(
    aes(as.factor(cor), share)) +
  geom_col(aes(fill = Modo)) +
  ggsci::scale_fill_locuszoom() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Cor", y="") +
  theme(legend.position = 'none') +
  facet_wrap(~Modo, nrow = 1)

# Plot composition -----------

p<- p1+(p2/p3/p4)

p + plot_annotation(tag_levels = 'A')

ggsave("plot2.png", path = "02_plot_data/02.1_perfil_sociodemografico/img")
rm(list = ls())