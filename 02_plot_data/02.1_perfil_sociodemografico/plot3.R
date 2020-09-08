source('setup.R')

plot3_idade <-
  pof_z %>% 
  group_by(
    quintil_renda, faixa_etaria, Modo) %>% 
  summarise(
    frequencia = weighted.mean(despesas_mes, PESO_FINAL)
  ) %>% 
  filter(Modo == 'Aplicativo')
s1<-
plot3_idade %>% 
  filter(faixa_etaria != '0-14') %>% 
  ggplot() +
  geom_path(
    aes(frequencia, as.factor(faixa_etaria),group = faixa_etaria),
    linetype = 'dotted') +
  geom_point(
    aes(frequencia, as.factor(faixa_etaria),
        fill = as.factor(quintil_renda)),
    shape = 21, size = 4.5, alpha = 1) +
  scale_fill_brewer(palette = 'Spectral') +
  scale_x_continuous(limits = c(3,11)) +
  labs(fill = "Quintil de Renda", y = "Faixa Etária") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'top',
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0))

plot3_sexo <-
  pof_z %>% 
  group_by(
    quintil_renda, sexo, Modo) %>% 
  summarise(
    frequencia = weighted.mean(despesas_mes, PESO_FINAL)
  ) %>% 
  filter(Modo == 'Aplicativo')
s2<-
  plot3_sexo %>% 
  ggplot() +
  geom_path(
    aes(frequencia, as.factor(sexo),group = sexo),
    linetype = 'dotted') +
  geom_point(
    aes(frequencia, as.factor(sexo),
        fill = as.factor(quintil_renda)),
    shape = 21, size = 4.5, alpha = 1) +
  scale_fill_brewer(palette = 'Spectral') +
  scale_x_continuous(limits = c(3,11)) +
  labs(fill = "Quintil de Renda", y = "Sexo") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0))

plot3_cor <-
  pof_z %>% 
  group_by(
    quintil_renda, cor, Modo) %>% 
  summarise(
    frequencia = weighted.mean(despesas_mes, PESO_FINAL)
  ) %>% 
  filter(Modo == 'Aplicativo')
s3<-
  plot3_cor %>% 
  ggplot() +
  geom_path(
    aes(frequencia, as.factor(cor),group = cor),
    linetype = 'dotted') +
  geom_point(
    aes(frequencia, as.factor(cor),
        fill = as.factor(quintil_renda)),
    shape = 21, size = 4.5, alpha = 1) +
  scale_fill_brewer(palette = 'Spectral') +
  scale_x_continuous(limits = c(3,11)) +
  labs(fill = "Quintil de Renda", y = "Cor", x = 'Nº médio de viagens por mês') +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'none',
    axis.title.y = element_text(angle = 0))

s<-s1/s2/s3
s


plot3b_cor <-
  pof_z %>% 
  group_by(
    quintil_renda, cor, Modo) %>% 
  summarise(
    gasto = weighted.mean(gasto_avg, PESO_FINAL)
  ) %>% 
  filter(Modo == 'Aplicativo') %>% 
  group_by(cor) %>% 
  mutate(media = mean(gasto)) %>% 
  ungroup()
p3<-
plot3b_cor %>% 
  filter(cor!='Amarela, Indígena ou outra') %>% 
  ggplot(aes(as.factor(quintil_renda), gasto, group = cor)) +
  geom_hline(aes(color = cor,yintercept=media),linetype = 'dotted',size=1.1)+
  geom_line(aes(color = cor),size=1.1) +
  geom_point(aes(fill = cor), size=3.5, shape=21)+
  ggsci::scale_color_locuszoom() +
  ggsci::scale_fill_locuszoom() +
  labs(x='Quintil de Renda', y='Valor médio da viagem (R$)', color = 'Cor', fill='Cor') +
  theme_minimal()


pf<-s|(p2/p3)
pf+plot_annotation(tag_levels = 'A')

pf[[1]] <- pf[[1]] + plot_layout(tag_level = 'new')

pf + plot_annotation(tag_levels = c('A', '1'))

