idade_rm <-
  idade_rm  %>% 
  filter(RM != 'Curitiba') %>% 
  filter(RM != 'Belém') %>% 
  ungroup()

idade_rm$RM2 <- idade_rm$RM
  

renda_rm %>% 
  na.omit() %>% 
  ggplot(aes(quintil_renda, RM)) +
  geom_tile(aes(fill = frequencia)) +
  facet_wrap(~Modo)

idade_rm <- idade_rm %>% ungroup()

idade_rm %>% 
  ggplot() +
  geom_line(
    data = idade_rm %>% select(-RM),
    aes(faixa_etaria, gasto/12, group = RM2),
    alpha = .4, color = 'gray20') +
  geom_line(aes(faixa_etaria, gasto/12, group = RM,
    color = Modo), size = 1) +
  geom_point(aes(faixa_etaria, gasto/12,color = Modo),size=2) +
  scale_color_viridis_d() +
  facet_wrap(~RM, nrow = 3) +
  labs(
    x='Faixa Etária', y = 'Gasto médio (R$)',
    title = 'Regiões metropolitanas: usuários de ride-hailing e idade',
    subtitle = 'Gasto mensal médio por faixa etária por Região Metropolitana.')+
  theme_bw() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(
    legend.position = 'none',
    panel.grid.minor = element_blank())