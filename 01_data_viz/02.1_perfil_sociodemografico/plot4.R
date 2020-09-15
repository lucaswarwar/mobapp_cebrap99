######################

source("00_setup.R.R")

### Recover dataset ###

pof_data <- 
  readr::read_rds("01_prepare_data/mobapp_individuo.rds")

# Plot 4: ride-hailing in urban areas ------------------

# I. % of people that uses ide-hailing

plot4a <- pof_data %>% 
  group_by(casa, Estrato) %>% 
  mutate(n_quintil = n_distinct(ID_MORADOR)) %>% 
  ungroup() %>% 
  group_by(casa, Modo, Estrato) %>% 
  summarise(prop = mean(n_distinct(ID_MORADOR)/n_quintil)) %>% 
  filter(Modo == 'Ride-hailing') %>% 
  filter(Estrato != "Interior Rural")

p1 <-
plot4a %>% 
  ggplot(aes(prop, reorder(as.factor(Estrato), prop),group = Estrato)) +
  geom_path(linetype = 'dotted') +
  geom_point(
    aes(fill = as.factor(casa)),
    shape = 21, size = 4.5, alpha = 1) +
  scale_x_continuous(labels = scales::percent, limits = c(0,.15)) +
  ggsci::scale_fill_locuszoom() +
  labs(fill = "", y = "", x = "% da população que consome ride-hailing") +
  theme_minimal() +
  theme(
    legend.position = 'top',
    axis.title.y = element_text(angle = 0))

# II. % of users of ride-hailing by estrato -------------------

plot4_estrato <- pof_data %>% 
  mutate(Modo = ifelse(Modo != "Ride-hailing", "População",Modo)) %>% 
  group_by(Modo) %>% 
  mutate(total = n()) %>% 
  group_by(Estrato, Modo) %>% 
  summarise(share = mean(n()/total)) %>% 
  na.omit()

plot4_estrato$Modo <- factor(plot4_estrato$Modo, levels = c('Ride-hailing', 'População'))

p2 <-
  plot4_estrato %>% 
  filter(Estrato != 'Interior Rural') %>% 
  ggplot(
    aes(as.factor(Estrato), share)) +
  geom_col(aes(fill = Modo)) +
  ggsci::scale_fill_locuszoom() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(x="", y="% dos usuários de ride-hailing") +
  theme(legend.position = 'none')+
  facet_wrap(~Modo,nrow = 1)

# III. % of users of ride-hailing by housetype -------------------

plot4_casa <- pof_data %>% 
  mutate(Modo = ifelse(Modo != "Ride-hailing", "População",Modo)) %>% 
  group_by(Modo) %>% 
  mutate(total = n()) %>% 
  group_by(casa, Modo) %>% 
  summarise(share = mean(n()/total)) %>% 
  na.omit()

plot4_casa$Modo <- factor(plot4_casa$Modo, levels = c('Ride-hailing', 'População'))

p3 <-
  plot4_casa %>% filter(casa != 'Habitação Irregular') %>%  
  ggplot(
    aes(as.factor(casa), share)) +
  geom_col(aes(fill = Modo)) +
  ggsci::scale_fill_locuszoom() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="% dos usuários de ride-hailing") +
  theme(legend.position = 'none') +
  facet_wrap(~Modo,nrow = 1)

# Plot composition ------------------

p<- p1/(p2+p3)

p[[2]] <- p[[2]] + plot_layout(tag_level = 'new')

p + plot_annotation(tag_levels = c('A', '1'))

ggsave("plot4.png", path = "02_plot_data/02.1_perfil_sociodemografico/img")
rm(list = ls())
