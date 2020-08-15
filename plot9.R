
plot9<-
  pof_z  %>% 
    filter(Modo == 'Aplicativo') %>% 
    filter(cor == 'Branca' | cor == 'Preta' ) %>% 
    #mutate(cor = ifelse(cor == 'Branca', 'Branca', 'Preta')) %>% 
    group_by(cor, sexo, RM) %>% 
    summarise(frequencia = weighted.mean(despesas_mes, PESO_FINAL))
  
plot9 <-
  plot9 %>% 
    mutate(
      grupo = paste(sexo, cor, sep = " "))
  
plot9$grupo <-
    recode(
      plot9$grupo,
      'Homem Branca' = 'Homem Branco',
      'Homem Preta' = 'Homem Preto',
      'Mulher Branca' = 'Mulher Branca',
      'Mulher Preta' = 'Mulher Preta'
    )

plot9<-
  plot9 %>% 
  filter(
    RM == 'Recife' |RM == 'São Paulo' |RM == 'Salvador' |
      RM == 'Brasília' |RM == 'Porto Alegre' |RM == 'Rio de Janeiro' |
      RM == 'Belo Horizonte' |RM == 'Fortaleza' |RM == 'Manaus' |
      RM == 'Campo Grande')
 pp1<- 
  plot9 %>% 
    ggplot(aes(frequencia, reorder(RM, frequencia), group = RM)) +
    geom_path(
      aes(group = interaction(RM, sexo)),
      position = position_dodge(width = .5),
      linetype = 'dotted') +
    geom_point(
      aes(frequencia, reorder(RM, frequencia), fill = grupo, group = sexo),
      size = 3.5, alpha = 1, shape = 21,
      position = position_dodge(width = .5)) +
    scale_fill_brewer(palette = 'Paired') +
    scale_color_brewer(palette = 'Paired') +
    theme_minimal() +
    guides(fill = guide_legend(ncol = 2)) +
    labs(
      x = 'Nº médio de viagens por mês', y="",
      fill = '')+
    theme(
      legend.position = 'top',
      panel.grid.minor = element_blank())
 
 plot9<-
   pof_z  %>% 
   filter(Modo=='Aplicativo') %>% 
   filter(cor == 'Branca' | cor == 'Preta'|cor=='Parda') %>% 
   mutate(cor = ifelse(cor == 'Branca', 'Branca', 'Preta')) %>% 
   group_by(cor, sexo, RM) %>% 
   summarise(frequencia = weighted.mean(gasto_avg, PESO_FINAL))
 
 plot9 <-
   plot9 %>% 
   mutate(
     grupo = paste(sexo, cor, sep = " "))
 
 plot9$grupo <-
   recode(
     plot9$grupo,
     'Homem Branca' = 'Homem Branco',
     'Homem Preta' = 'Homem Preto',
     'Mulher Branca' = 'Mulher Branca',
     'Mulher Preta' = 'Mulher Preta'
   )
 
 plot9<-
   plot9 %>% 
   filter(
     RM == 'Recife' |RM == 'São Paulo' |RM == 'Salvador' |
       RM == 'Brasília' |RM == 'Porto Alegre' |RM == 'Rio de Janeiro' |
       RM == 'Belo Horizonte' |RM == 'Fortaleza' |RM == 'Manaus' |
       RM == 'Campo Grande')
 pp2<- 
   plot9 %>% 
   ggplot(aes(frequencia, reorder(RM, frequencia), group = RM)) +
   geom_path(
     aes(group = interaction(RM, sexo)),
     position = position_dodge(width = .5),
     linetype = 'dotted') +
   geom_point(
     aes(frequencia, reorder(RM, frequencia), fill = grupo, group = sexo),
     size = 3.5, alpha = 1, shape = 21,
     position = position_dodge(width = .5)) +
   scale_fill_brewer(palette = 'Paired') +
   scale_color_brewer(palette = 'Paired') +
   theme_minimal() +
   guides(fill = guide_legend(ncol = 2)) +
   labs(
     x = 'Custo médio da viagem (R$)', y="",
     fill = '')+
   theme(
     legend.position = 'top',
     panel.grid.minor = element_blank())
 
ppp<-pp1|pp2
ppp+plot_annotation(tag_levels = 'A')
