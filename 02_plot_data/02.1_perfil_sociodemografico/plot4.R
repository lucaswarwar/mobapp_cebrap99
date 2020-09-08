plot4a <- #proporção do total de pessoas que usa por estrato
  pof_z %>% 
  group_by(casa, Estrato) %>% 
  mutate(n_quintil = n_distinct(ID_MORADOR)) %>% 
  ungroup() %>% 
  group_by(casa, Modo, Estrato) %>% 
  summarise(prop = mean(n_distinct(ID_MORADOR)/n_quintil)) %>% 
  filter(Modo == 'Aplicativo') %>% 
  filter(Estrato != "Interior Rural")

plot4a  <-
  plot4a %>% 
  filter(
    casa == "Apartamento Pavimentada" |
    casa == "Casa Pavimentada"
  ) %>%
  mutate(
    casa = case_when(
      casa == "Apartamento Pavimentada" ~ "Apartamento",
      casa == "Casa Pavimentada" ~ "Casa"
  ))
p1<-
plot4a %>% 
  ggplot(aes(prop, reorder(as.factor(Estrato), prop),group = Estrato)) +
  geom_path(
    
    linetype = 'dotted') +
  geom_point(
    aes(
        fill = as.factor(casa)),
    shape = 21, size = 4.5, alpha = 1) +
  scale_x_continuous(labels = scales::percent, limits = c(0,.15)) +
  ggsci::scale_fill_locuszoom() +
  labs(fill = "", y = "", x = "% da população que consome ride-hailing") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'top',
    #axis.text.x = element_blank(),
    #axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0))

estrato <-
data.table::fread("df's/Estrato.csv")

p2<-
estrato %>% 
  filter(Modo == "Aplicativo") %>% 
  filter(Estrato != 'Interior Rural') %>% 
  ggplot(
    aes(as.factor(Estrato), share)) +
  geom_col(aes(fill = Modo)) +
  ggsci::scale_fill_locuszoom() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="% dos usuários de ride-hailing") +
  theme(legend.position = 'none')

casa <-
  data.table::fread("df's/casa.csv")

casa  <-
  casa %>% 
  filter(
    casa == "Apartamento Pavimentada" |
    casa == "Casa Pavimentada"
  ) %>%
  mutate(
    casa = case_when(
      casa == "Apartamento Pavimentada" ~ "Apartamento",
      casa == "Casa Pavimentada" ~ "Casa"
    ))
p3<-
casa %>% 
  filter(Modo == "Aplicativo") %>% 
  filter(casa != 'Apartamento Não Pavimentada') %>% 
  ggplot(
    aes(as.factor(casa), share)) +
  geom_col(aes(fill = Modo)) +
  ggsci::scale_fill_locuszoom() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="") +
  theme(legend.position = 'none')

p<- p1/(p2+p3)
p
p[[2]] <- p[[2]] + plot_layout(tag_level = 'new')

p + plot_annotation(tag_levels = c('A', '1'))
