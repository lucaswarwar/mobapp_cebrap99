plot7_rm <-
  # proporção de pessoas que consomem transporte que fazem algum uso de app
  pof_z %>% 
  group_by(quintil_renda, RM) %>% 
  mutate(n_quintil = n_distinct(ID_MORADOR)) %>% 
  ungroup() %>% 
  group_by(quintil_renda, Modo, RM) %>% 
  summarise(prop = mean(n_distinct(ID_MORADOR)/n_quintil)) %>% 
  filter(Modo == 'Aplicativo')
plot7b_rm <-
  # proporção de pessoas que consomem transporte que fazem algum uso de app
  pof_z %>% 
  group_by(RM) %>% 
  mutate(n_quintil = n_distinct(ID_MORADOR)) %>% 
  ungroup() %>% 
  group_by(Modo, RM) %>% 
  summarise(prop = mean(n_distinct(ID_MORADOR)/n_quintil)) %>% 
  filter(Modo == 'Aplicativo')
plot7_rm <-
  plot7b_rm %>% 
  rename(media = prop) %>% 
  right_join(plot7_rm, by = 'RM')
p1<-
plot7_rm  %>% 
  filter(RM!='Zona Rural' & RM!='Brasil Urbano') %>% 
  ggplot(aes(prop, reorder(RM,media))) +
  geom_path(aes(group = RM),
            linetype = 'dotted') +
  geom_point(
    aes(fill = as.factor(quintil_renda)),
    shape = 21, size = 3, alpha = 1) +
  geom_point(
    aes(media, reorder(RM,media)),
    shape = 21, size = 3, alpha = 1, fill = 'black') +
  scale_x_continuous(labels = scales::percent, limits = c(0,.21)) +
  scale_fill_brewer(palette = 'Spectral') +
  labs(fill = "Quintil de Renda", y = "",x='% da população que consome ride-haling') +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'top',
    #axis.text.x = element_blank(),
    #axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0))

RM<-fread("df's/rm.csv")
p2<-
  RM %>% 
  filter(Modo == 'Aplicativo') %>% 
  filter(RM!='Zona Rural' & RM!='Brasil Urbano') %>% 
  ggplot(aes(reorder(RM,frequencia),share)) +
  geom_col(aes(fill = Modo)) +
  ggsci::scale_fill_locuszoom() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="% dos usuários de ride-hailing") +
  theme(legend.position = 'none') +
  coord_flip()
plot7b <-
  pof_z %>% 
  filter(Modo == 'Aplicativo') %>% 
  group_by(RM) %>% 
  mutate(pop = n_distinct(ID_MORADOR)) %>% 
  group_by(RM, quintil_renda) %>% 
  summarise(
    share = mean(n()/pop)
  )

plot7b$names<-plot7b$RM

plot7b<-
  plot7b %>% ungroup()
filter(
  RM == 'Recife' |RM == 'São Paulo' |RM == 'Salvador' |
    RM == 'Brasília' |RM == 'Porto Alegre' |RM == 'Rio de Janeiro' |
    RM == 'Belo Horizonte' |RM == 'Fortaleza' |RM == 'Manaus' |
    RM == 'Campo Grande')
p3<-
  plot7b %>%   
  ggplot(aes(quintil_renda,share))+
  geom_line(
    data = plot7b %>% select(-RM),
    aes(quintil_renda,share, group = names),
    alpha = .5) +
  geom_line(aes(group = RM), color = '#ff0028' ,size=1.1) +
  geom_point(aes(group = RM), color = '#ff0028' ,size=2.5) +
  scale_y_continuous(labels = scales::percent)+
  labs(x='Quintil de Renda',y="% dos usuários de ride-hailing")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())+
  facet_wrap(~RM, nrow = 5)
p24<-(p2|p3)
p24+plot_annotation(tag_levels = 'A')
