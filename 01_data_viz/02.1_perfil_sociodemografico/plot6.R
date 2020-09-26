######################

source("setup.R")
source("colours.R")

### Recover dataset ###

pof_data <- 
  readr::read_rds("00_prepare_data/mobapp_individuo.rds")

# Plot 6: ride-hailing usership by city ---------------------------------

# Ridership per city -----------

plot6_rm <-
  pof_data %>% 
  group_by(RM) %>% 
  mutate(pop = n_distinct(ID_MORADOR)) %>%
  ungroup() %>% 
  group_by(RM, Modo) %>% 
  mutate(n = n_distinct(ID_MORADOR),media=n/pop) %>% 
  ungroup() %>%
  group_by(quintil_renda, RM) %>% 
  mutate(n_quintil = n_distinct(ID_MORADOR)) %>% 
  ungroup() %>% 
  group_by(quintil_renda, Modo, RM,media) %>% 
  summarise(prop = mean(n_distinct(ID_MORADOR)/n_quintil)) %>% 
  filter(Modo == 'Ride-hailing') %>% 
  ungroup()

p1<-
plot6_rm  %>% 
  #filter(quintil_renda!=2 & quintil_renda!=4) %>% 
  ggplot(aes(prop, reorder(RM,media))) +
  geom_path(aes(group = RM), linetype = 'dotted') +
  geom_point(
    aes(fill = as.factor(quintil_renda)),
    shape = 21, size = 3.5, alpha = 1) +
  #geom_point(
   # aes(media, reorder(RM,media)),
    #shape = 21, size = 3.5, alpha = 1, fill = 'black') +
  scale_x_continuous(labels = scales::percent, limits = c(0,.18)) +
  scale_fill_aop(palette = 'blue_red') +
  labs(fill = "Quintil de Renda", y = "",x='% da população que consome ride-haling') +
  theme_minimal() +
  theme(
    legend.position = 'top',
    axis.title.y = element_text(angle = 0))

# % of users by city -------------------------------

plot6b_rm <- pof_data %>% 
  group_by(Modo) %>% 
  mutate(total = n()) %>% 
  group_by(RM, Modo) %>% 
  summarise(share = mean(n()/total)) %>% 
  filter(Modo == "Ride-hailing") %>% 
  na.omit()

p2<-
plot6b_rm %>% filter(RM!='Brasil Urbano' & RM!='Zona Rural') %>% 
  ggplot(aes(reorder(RM,share),share)) +
  geom_col(aes(fill = Modo)) +
  scale_fill_aop() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="% dos usuários de ride-hailing") +
  theme(legend.position = 'none') +
  coord_flip()

# Plot Composition ----------------------

p<-p1|p2

p+plot_annotation(tag_levels = 'A')

ggsave("plot6.png", path = "01_data_viz/02.1_perfil_sociodemografico/img")
rm(list = ls())
