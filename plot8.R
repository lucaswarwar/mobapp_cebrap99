plot8 <-
  pof_z %>% 
  filter(Modo == 'Aplicativo') %>% 
  group_by(RM) %>% 
  mutate(media = weighted.mean(despesas_mes,PESO_FINAL)) %>% 
  group_by(RM, Estrato) %>% 
  summarise(
    gasto = weighted.mean(despesas_mes,PESO_FINAL),
    media = weighted.mean(media,PESO_FINAL)
  )


plot8<-
  plot8 %>% 
filter(
  RM == 'Recife' |RM == 'São Paulo' |RM == 'Salvador' |
    RM == 'Brasília' |RM == 'Porto Alegre' |RM == 'Rio de Janeiro' |
    RM == 'Belo Horizonte' |RM == 'Fortaleza' |RM == 'Manaus' |
    RM == 'Campo Grande')
p1<-
  plot8  %>% 
  ggplot(aes(gasto, reorder(RM,media))) +
  geom_path(aes(group = RM),
            linetype = 'dotted') +
  geom_point(
    aes(fill = as.factor(Estrato)),
    shape = 21, size = 3.5, alpha = 1) +
  geom_point(
    aes(media, reorder(RM,media)),
    shape = 21, size = 3.5, alpha = 1, fill = 'black') +
  ggsci::scale_fill_locuszoom() +
  labs(fill = "", y = "",x='Nº médio de viagens por mês') +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'top',
    #axis.text.x = element_blank(),
    #axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0))

p<-p1|p2
p+plot_annotation(tag_levels = 'A')
