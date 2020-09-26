######################

source("setup.R")
source("colours.R")

### Recover dataset ###

pof_data <- 
  readr::read_rds("00_prepare_data/mobapp_individuo.rds")

# Plot 7: composition of users by city ---------------------------------

# I. Income ----------------------------------

plot7_renda <- pof_data %>% 
  filter(Modo == 'Ride-hailing') %>% 
  group_by(RM) %>% 
  mutate(pop = n_distinct(ID_MORADOR)) %>% 
  group_by(RM, quintil_renda) %>% 
  summarise(share = mean(n()/pop)) %>% 
  mutate(names = RM) %>% 
  ungroup() %>% 
  filter(
    RM == 'Recife'   |       RM == 'São Paulo'|    RM == 'Salvador' |
    RM == 'Brasília' |       RM == 'Porto Alegre'| RM == 'Rio de Janeiro' |
    RM == 'Belo Horizonte' | RM == 'Fortaleza' |   RM == 'Manaus' |
    RM == 'Campo Grande')

p1<-
  plot7_renda %>%   
  ggplot(aes(quintil_renda,share))+
  geom_line(
    data = plot7_renda %>% select(-RM),
    aes(quintil_renda,share, group = names),
    alpha = .5) +
  geom_line(aes(group = RM), color = '#c88300' ,size=1.1) +
  geom_point(aes(group = RM), color = '#c88300' ,size=2.5) +
  scale_y_continuous(labels = scales::percent)+
  labs(x='Quintil de Renda',y="% dos usuários de ride-hailing")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())+
  facet_wrap(~RM, nrow = 5)

# II. Age ----------------------------------

plot7_idade <- pof_data %>% 
  filter(Modo == 'Ride-hailing') %>% 
  group_by(RM) %>% 
  mutate(pop = n_distinct(ID_MORADOR)) %>% 
  group_by(RM, faixa_etaria) %>% 
  summarise(share = mean(n()/pop)) %>% 
  mutate(names = RM) %>% 
  ungroup() %>% 
  filter(
    RM == 'Recife'   |       RM == 'São Paulo'|    RM == 'Salvador' |
      RM == 'Brasília' |       RM == 'Porto Alegre'| RM == 'Rio de Janeiro' |
      RM == 'Belo Horizonte' | RM == 'Fortaleza' |   RM == 'Manaus' |
      RM == 'Campo Grande')

p2<-
  plot7_idade %>% filter(faixa_etaria != '0-14') %>% 
  ggplot(aes(faixa_etaria,share))+
  geom_line(
    data = plot7_idade %>% select(-RM) %>% filter(faixa_etaria != '0-14'),
    aes(faixa_etaria,share, group = names),
    alpha = .5) +
  geom_line(aes(group = RM), color = '#00324a' ,size=1.1) +
  geom_point(aes(group = RM), color = '#00324a' ,size=2.5) +
  scale_y_continuous(labels = scales::percent)+
  labs(x='Faixa Etária',y="% dos usuários de ride-hailing")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())+
  facet_wrap(~RM, nrow = 5)

# Plot composition ---------------

p<-(p1|p2)

p+plot_annotation(tag_levels = 'A')

ggsave("plot7.png", path = "01_data_viz/02.1_perfil_sociodemografico/img")
rm(list = ls())

