plot5_estrato <-
  pof_z %>% 
  group_by(
    Estrato, casa, Modo) %>% 
  summarise(
    frequencia = weighted.mean(despesas_mes, PESO_FINAL)
  ) %>% 
  filter(Modo == 'Aplicativo')

plot5_estrato  <-
  plot5_estrato %>% 
  filter(
    casa == "Apartamento Pavimentada" |
      casa == "Casa Pavimentada"
  ) %>%
  mutate(
    casa = case_when(
      casa == "Apartamento Pavimentada" ~ "Apartamento",
      casa == "Casa Pavimentada" ~ "Casa"
    ))
e1<-
plot5_estrato %>% 
  filter(Estrato != 'Interior Rural') %>% 
  ggplot(aes(frequencia, reorder(Estrato,frequencia))) +
  geom_path(
    aes(group = Estrato),
    linetype = 'dotted') +
  geom_point(
    aes(fill = as.factor(casa)),
    shape = 21, size = 4.5, alpha = 1) +
  ggsci::scale_fill_locuszoom() +
  labs(fill = "", y = "", x = "Nº médio de viagens por mês") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'top',
    #axis.text.x = element_blank(),
    #axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0))

plot5b_casa <-
  pof_z %>% 
  group_by(
    quintil_renda, casa, Modo) %>% 
  summarise(
    gasto = weighted.mean(gasto_avg, PESO_FINAL)
  ) %>% 
  filter(Modo == 'Aplicativo') %>% 
  group_by(casa) %>% 
  mutate(media = mean(gasto)) %>% 
  ungroup()
plot5b_casa  <-
  plot5b_casa %>% 
  filter(
    casa == "Apartamento Pavimentada" |
      casa == "Casa Pavimentada"
  ) %>%
  mutate(
    casa = case_when(
      casa == "Apartamento Pavimentada" ~ "Apartamento",
      casa == "Casa Pavimentada" ~ "Casa"
    ))
e3<-
  plot5b_casa %>% 
  filter(casa!='Casa Não Pavimentada' &
           casa!='Apartamento Não Pavimentada') %>% 
  ggplot(aes(as.factor(quintil_renda), gasto, group = casa)) +
  geom_hline(aes(color = casa,yintercept=media),linetype = 'dotted',size=1.1)+
  geom_line(aes(color = casa),size=1.1) +
  geom_point(aes(fill = casa), size=3.5, shape=21)+
  ggsci::scale_color_locuszoom() +
  ggsci::scale_fill_locuszoom() +
  labs(x='Quintil de renda', y='Valor médio da viagem (R$)', color = 'Habitação', fill='Habitação') +
  theme_minimal()

e<-e1|(e2/e3)

e[[1]] <- e[[1]] + plot_layout(tag_level = 'new')

e + plot_annotation(tag_levels = 'A')

