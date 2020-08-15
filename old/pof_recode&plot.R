pof_app <-
   pof_uber %>% 
   filter(Modo == 'Aplicativo')
pof_app_individuo <-
   pof_app %>% 
   group_by(
     PESO_FINAL, ID_MORADOR, aluguel, CONDICAO, renda_pc,
     faixa_etaria, sexo, cor, regiao, UF, Estrato, Modo, ocup,
     commute, horas, casa, comodos_morador, status_casa, status_pessoa,
     escolaridade, Celular, Internet, Carro, Bike, RM, decil_renda, quintil_renda
   ) %>% 
   summarise(
     gasto_avg = mean(valor_despesa),
     gasto_anual = sum(valor_despesa*FATOR_ANUALIZACAO),
     despesas_mes = sum(FATOR_ANUALIZACAO)/12)

pof_taxi <-
  pof_uber %>% 
  filter(Modo == 'Táxi')
pof_taxi_individuo <-
  pof_taxi %>% 
  group_by(
    PESO_FINAL, ID_MORADOR, aluguel, CONDICAO, renda_pc,
    faixa_etaria, sexo, cor, regiao, UF, Estrato, Modo, ocup,
    commute, horas, casa, comodos_morador, status_casa, status_pessoa,
    escolaridade, Celular, Internet, Carro, Bike, RM, decil_renda, quintil_renda
  ) %>% 
  summarise(
    gasto_avg = mean(valor_despesa),
    gasto_anual = sum(valor_despesa*FATOR_ANUALIZACAO),
    despesas_mes = sum(FATOR_ANUALIZACAO)/12)

pof_tp <-
  pof_uber %>% 
  filter(Modo == 'Transporte Público')
pof_tp_individuo <-
  pof_tp %>% 
  group_by(
    PESO_FINAL, ID_MORADOR, aluguel, CONDICAO, renda_pc,
    faixa_etaria, sexo, cor, regiao, UF, Estrato, Modo, ocup,
    commute, horas, casa, comodos_morador, status_casa, status_pessoa,
    escolaridade, Celular, Internet, Carro, Bike, RM, decil_renda, quintil_renda
  ) %>% 
  summarise(
    gasto_avg = mean(valor_despesa),
    gasto_anual = sum(valor_despesa*FATOR_ANUALIZACAO),
    despesas_mes = sum(FATOR_ANUALIZACAO)/12)

pof_auto <-
  pof_uber %>% 
  filter(Modo == 'Transporte Privado')
pof_auto_individuo <-
  pof_auto %>% 
  group_by(
    PESO_FINAL, ID_MORADOR, aluguel, CONDICAO, renda_pc,
    faixa_etaria, sexo, cor, regiao, UF, Estrato, Modo, ocup,
    commute, horas, casa, comodos_morador, status_casa, status_pessoa,
    escolaridade, Celular, Internet, Carro, Bike, RM, decil_renda, quintil_renda
  ) %>% 
  summarise(
    gasto_avg = mean(valor_despesa),
    gasto_anual = sum(valor_despesa*FATOR_ANUALIZACAO),
    despesas_mes = sum(FATOR_ANUALIZACAO)/12)

###################################################################################

pof_z <-
  bind_rows(pof_app_individuo,pof_auto_individuo, pof_taxi_individuo, pof_tp_individuo)

pof_z <-
  pof_z %>% 
  group_by(Modo) %>% 
  mutate(total_modo = n_distinct(ID_MORADOR)) %>% 
  ungroup() 

faixa_etaria <-
  pof_z %>% 
  group_by(Modo, faixa_etaria) %>% 
  summarise(
    share = mean(n()/total_modo),
    frequencia = weighted.mean(despesas_mes, PESO_FINAL),
    frequencia_sd = sd(despesas_mes),
    ticket = weighted.mean(gasto_avg, PESO_FINAL),
    ticket_sd = sd(gasto_avg),
    gasto = weighted.mean(gasto_anual, PESO_FINAL),
    gasto_sd = sd(gasto_anual)
  )


p1<-
faixa_etaria %>% 
  ggplot(aes(faixa_etaria, share, group = Modo)) +
  geom_line(
    data = faixa_etaria %>% filter(Modo != 'Aplicativo'),
    aes(faixa_etaria, share, group = Modo, color = Modo),
    linetype = 'dashed', size = 1) +
  geom_point(
    data = faixa_etaria %>% filter(Modo != 'Aplicativo'),
    aes(faixa_etaria, share, fill = Modo), shape = 21, size = 3)  +
  geom_line(
    data = faixa_etaria %>% filter(Modo == 'Aplicativo'),
    aes(faixa_etaria, share, group = Modo, color = Modo),
    size = 1.2) +
  geom_point(
    data = faixa_etaria %>% filter(Modo == 'Aplicativo'),
    aes(faixa_etaria, share, fill = Modo), shape = 21, size = 3.5)  +
  ggsci::scale_color_lancet() +
  ggsci::scale_fill_lancet() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = 'Faixa Etária', y = 'Porcentagem dos Usuários',
    title = 'Distribuição dos usuários por idade') +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank())
p2<-
faixa_etaria %>% 
  ggplot(aes(faixa_etaria, frequencia, group = Modo)) +
  geom_line(
    data = faixa_etaria %>% filter(Modo != 'Aplicativo'),
    aes(faixa_etaria, frequencia, group = Modo, color = Modo),
    linetype = 'dashed', size = 1) +
  geom_point(
    data = faixa_etaria %>% filter(Modo != 'Aplicativo'),
    aes(faixa_etaria, frequencia, fill = Modo), shape = 21, size = 3)  +
  geom_line(
    data = faixa_etaria %>% filter(Modo == 'Aplicativo'),
    aes(faixa_etaria, frequencia, group = Modo, color = Modo),
    size = 1.2) +
  geom_point(
    data = faixa_etaria %>% filter(Modo == 'Aplicativo'),
    aes(faixa_etaria, frequencia, fill = Modo), shape = 21, size = 3.5)  +
  ggsci::scale_color_lancet() +
  ggsci::scale_fill_lancet() +
  labs(
    x = 'Faixa Etária', y = 'Nº de despesas',
    title = 'Número médio de despesas por mês por idade') +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank())
p3<-
faixa_etaria %>% 
  ggplot(aes(faixa_etaria, ticket, group = Modo)) +
  geom_line(
    data = faixa_etaria %>% filter(Modo != 'Aplicativo'),
    aes(faixa_etaria, ticket, group = Modo, color = Modo),
    linetype = 'dashed', size = 1) +
  geom_point(
    data = faixa_etaria %>% filter(Modo != 'Aplicativo'),
    aes(faixa_etaria, ticket, fill = Modo), shape = 21, size = 3)  +
  geom_line(
    data = faixa_etaria %>% filter(Modo == 'Aplicativo'),
    aes(faixa_etaria, ticket, group = Modo, color = Modo),
    size = 1.2) +
  geom_point(
    data = faixa_etaria %>% filter(Modo == 'Aplicativo'),
    aes(faixa_etaria, ticket, fill = Modo), shape = 21, size = 3.5)  +
  ggsci::scale_color_lancet() +
  ggsci::scale_fill_lancet() +
  labs(
    x = 'Faixa Etária', y = 'Ticket médio (R$)',
    title = 'Valor médio da despesa por idade') +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank())
p4<-
faixa_etaria %>% 
  ggplot(aes(faixa_etaria, gasto/12, group = Modo)) +
  geom_line(
    data = faixa_etaria %>% filter(Modo != 'Aplicativo'),
    aes(faixa_etaria, gasto/12, group = Modo, color = Modo),
    linetype = 'dashed', size = 1) +
  geom_point(
    data = faixa_etaria %>% filter(Modo != 'Aplicativo'),
    aes(faixa_etaria, gasto/12, fill = Modo), shape = 21, size = 3)  +
  geom_line(
    data = faixa_etaria %>% filter(Modo == 'Aplicativo'),
    aes(faixa_etaria, gasto/12, group = Modo, color = Modo),
    size = 1.2) +
  geom_point(
    data = faixa_etaria %>% filter(Modo == 'Aplicativo'),
    aes(faixa_etaria, gasto/12, fill = Modo), shape = 21, size = 3.5)  +
  ggsci::scale_color_lancet() +
  ggsci::scale_fill_lancet() +
  labs(
    x = 'Faixa Etária', y = 'Gasto mensal médio (R$)',
    title = 'Gasto mensal médio por idade') +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank())

p <-
  (p1+p2)/(p3+p4)

p

##################################################
decil_renda$decil_renda <- as.factor(decil_renda$decil_renda)
decil_renda <- decil_renda %>% na.omit()
p1<-
  decil_renda %>% 
  ggplot(aes(decil_renda, share, group = Modo)) +
  geom_line(
    data = decil_renda %>% filter(Modo != 'Aplicativo'),
    aes(decil_renda, share, group = Modo, color = Modo),
    linetype = 'dashed', size = 1) +
  geom_point(
    data = decil_renda %>% filter(Modo != 'Aplicativo'),
    aes(decil_renda, share, fill = Modo), shape = 21, size = 3)  +
  geom_line(
    data = decil_renda %>% filter(Modo == 'Aplicativo'),
    aes(decil_renda, share, group = Modo, color = Modo),
    size = 1.2) +
  geom_point(
    data = decil_renda %>% filter(Modo == 'Aplicativo'),
    aes(decil_renda, share, fill = Modo), shape = 21, size = 3.5)  +
  ggsci::scale_color_lancet() +
  ggsci::scale_fill_lancet() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = 'Decil de renda', y = 'Porcentagem dos Usuários',
    title = 'Distribuição dos usuários por decil de renda') +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank())
p2<-
  decil_renda %>% 
  ggplot(aes(decil_renda, frequencia, group = Modo)) +
  geom_line(
    data = decil_renda %>% filter(Modo != 'Aplicativo'),
    aes(decil_renda, frequencia, group = Modo, color = Modo),
    linetype = 'dashed', size = 1) +
  geom_point(
    data = decil_renda %>% filter(Modo != 'Aplicativo'),
    aes(decil_renda, frequencia, fill = Modo), shape = 21, size = 3)  +
  geom_line(
    data = decil_renda %>% filter(Modo == 'Aplicativo'),
    aes(decil_renda, frequencia, group = Modo, color = Modo),
    size = 1.2) +
  geom_point(
    data = decil_renda %>% filter(Modo == 'Aplicativo'),
    aes(decil_renda, frequencia, fill = Modo), shape = 21, size = 3.5)  +
  ggsci::scale_color_lancet() +
  ggsci::scale_fill_lancet() +
  labs(
    x = 'Decil de Renda', y = 'Nº de despesas',
    title = 'Número médio de despesas por mês por decil de renda') +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank())
p3<-
  decil_renda %>% 
  ggplot(aes(decil_renda, ticket, group = Modo)) +
  geom_line(
    data = decil_renda %>% filter(Modo != 'Aplicativo'),
    aes(decil_renda, ticket, group = Modo, color = Modo),
    linetype = 'dashed', size = 1) +
  geom_point(
    data = decil_renda %>% filter(Modo != 'Aplicativo'),
    aes(decil_renda, ticket, fill = Modo), shape = 21, size = 3)  +
  geom_line(
    data = decil_renda %>% filter(Modo == 'Aplicativo'),
    aes(decil_renda, ticket, group = Modo, color = Modo),
    size = 1.2) +
  geom_point(
    data = decil_renda %>% filter(Modo == 'Aplicativo'),
    aes(decil_renda, ticket, fill = Modo), shape = 21, size = 3.5)  +
  ggsci::scale_color_lancet() +
  ggsci::scale_fill_lancet() +
  labs(
    x = 'Decil de Renda', y = 'Ticket médio (R$)',
    title = 'Valor médio da despesa por decil de renda') +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank())
p4<-
  decil_renda %>% 
  ggplot(aes(decil_renda, gasto/12, group = Modo)) +
  geom_line(
    data = decil_renda %>% filter(Modo != 'Aplicativo'),
    aes(decil_renda, gasto/12, group = Modo, color = Modo),
    linetype = 'dashed', size = 1) +
  geom_point(
    data = decil_renda %>% filter(Modo != 'Aplicativo'),
    aes(decil_renda, gasto/12, fill = Modo), shape = 21, size = 3)  +
  geom_line(
    data = decil_renda %>% filter(Modo == 'Aplicativo'),
    aes(decil_renda, gasto/12, group = Modo, color = Modo),
    size = 1.2) +
  geom_point(
    data = decil_renda %>% filter(Modo == 'Aplicativo'),
    aes(decil_renda, gasto/12, fill = Modo), shape = 21, size = 3.5)  +
  ggsci::scale_color_lancet() +
  ggsci::scale_fill_lancet() +
  labs(
    x = 'Decil de Renda', y = 'Gasto mensal médio (R$)',
    title = 'Gasto mensal médio por decil de renda') +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank())

p <-
  (p1+p2)/(p3+p4)

p