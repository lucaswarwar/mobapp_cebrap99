renda <-
  data.table::fread("df's/renda.csv")
idade <-
  data.table::fread("df's/idade.csv")
sexo <-
  data.table::fread("df's/sexo.csv")
cor <-
  data.table::fread("df's/cor.csv")
p1<-
renda %>% 
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
p2<-
idade %>% 
  filter(Modo == "Aplicativo") %>% 
  filter(faixa_etaria != '0-14') %>% 
  ggplot(
    aes(as.factor(faixa_etaria), share)) +
  geom_col(aes(fill = Modo)) +
  ggsci::scale_fill_locuszoom() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Faixa Etária", y="") +
  theme(legend.position = 'none')
p3<-
sexo %>% 
  filter(Modo == "Aplicativo") %>% 
  ggplot(
    aes(as.factor(sexo), share)) +
  geom_col(aes(fill = Modo)) +
  ggsci::scale_fill_locuszoom() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Sexo", y="") +
  theme(legend.position = 'none')
p4<-
cor %>% 
  filter(Modo == "Aplicativo") %>% 
  filter(cor != 'Amarela, Indigena ou outra') %>% 
  ggplot(
    aes(as.factor(cor), share)) +
  geom_col(aes(fill = Modo)) +
  ggsci::scale_fill_locuszoom() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Cor", y="") +
  theme(legend.position = 'none')

p<- p1+(p2/p3/p4)
p + plot_annotation(tag_levels = 'A')
patchwork::