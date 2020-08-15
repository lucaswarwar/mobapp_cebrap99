plot6_uf <-
  # proporção de pessoas que consomem transporte que fazem algum uso de app
  pof_z %>% 
  group_by(quintil_renda, UF) %>% 
  mutate(n_quintil = n_distinct(ID_MORADOR)) %>% 
  ungroup() %>% 
  group_by(quintil_renda, Modo, UF) %>% 
  summarise(prop = mean(n_distinct(ID_MORADOR)/n_quintil)) %>% 
  filter(Modo == 'Aplicativo')
plot6b_uf <-
  # proporção de pessoas que consomem transporte que fazem algum uso de app
  pof_z %>% 
  group_by(UF) %>% 
  mutate(n_quintil = n_distinct(ID_MORADOR)) %>% 
  ungroup() %>% 
  group_by(Modo, UF) %>% 
  summarise(prop = mean(n_distinct(ID_MORADOR)/n_quintil)) %>% 
  filter(Modo == 'Aplicativo')

plot6b_uf %>% 
  ggplot(aes(reorder(UF,-prop),prop)) +
  geom_col(aes(fill=Modo)) +
  ggsci::scale_fill_locuszoom() +
  theme_minimal()

plot6_teste <-
  plot6b_uf %>% 
  rename(media = prop) %>% 
  right_join(plot6_uf, by = 'UF')
p1<-
plot6_teste  %>% 
  #filter(quintil_renda!=2 & quintil_renda!=4) %>% 
  ggplot(aes(prop, reorder(UF,media))) +
  geom_path(aes(group = UF),
    linetype = 'dotted') +
  geom_point(
    aes(fill = as.factor(quintil_renda)),
    shape = 21, size = 3.5, alpha = 1) +
  geom_point(
    aes(media, reorder(UF,media)),
    shape = 21, size = 3.5, alpha = 1, fill = 'black') +
  scale_x_continuous(labels = scales::percent, limits = c(0,.12)) +
  scale_fill_brewer(palette = 'Spectral') +
  labs(fill = "Quintil de Renda", y = "",x='% da população que consome ride-haling') +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'top',
    #axis.text.x = element_blank(),
    #axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0))

uf<-fread("df's/uf.csv")
p2<-
UF %>% 
  ggplot(aes(reorder(UF,share),share)) +
  geom_col(aes(fill = Modo)) +
  ggsci::scale_fill_locuszoom() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="% dos usuários de ride-hailing") +
  theme(legend.position = 'none') +
  coord_flip()

p24<-p1|p2
p24+plot_annotation(tag_levels = 'A')
