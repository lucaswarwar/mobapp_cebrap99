#########################################################
### EDA Preliminar                                    ###
### 4 gráficos por categoria, 2 de coluna e 2 boxplot ###
### Coluna: distribuição e frequência média.          ###
### Boxplot: ticket médio e gasto mensal médio.       ###
#########################################################

# Faixa Etária ----------------------------------

faixa_etaria <-
  pof_app_individuo %>% 
  group_by(faixa_etaria) %>% 
  summarise(
    n = n()/1458,
    freq = weighted.mean(despesas_mes, PESO_FINAL))

fe1<-
  faixa_etaria %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(faixa_etaria, n), fill = '#ffd723') + #ffd723
  scale_y_continuous(labels = scales::percent, limits = c(0,.3)) +
  labs(
    x = 'Faixa Etária', y = 'Porcentagem dos Usuários',
    title = 'Usuários de ride-hailing por idade') +
  theme_minimal()

fe2<-
faixa_etaria %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(faixa_etaria, freq), fill = '#ffd723') + #ffd723
  scale_y_continuous(limits = c(0,8)) +
  labs(
    x = 'Faixa Etária', y = 'Viagens por mês',
    title = 'Freqûencia de uso por idade') +
  theme_minimal()

fe3 <-
pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(faixa_etaria,gasto_avg, weight = PESO_FINAL), color = '#00d042') +
  scale_y_continuous(limits = c(0,100)) +
  labs(
    x = 'Faixa Etária', y = 'Ticket Médio (R$)',
    title = 'Custo médio das viagens por idade') +
  theme_minimal()

fe4 <-
pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(faixa_etaria,gasto_anual/12, weight = PESO_FINAL), color = '#00d042') +
  scale_y_continuous(limits = c(0,500)) +
  labs(
    x = 'Faixa Etária', y = 'Gasto mensal médio (R$)',
    title = 'Gasto mensal médio por idade') +
  theme_minimal()

fe_final <-
  (fe1 + fe2)/(fe3 + fe4)

fe_final

##############################################################################################

# Sexo ----------------------------------

sexo <-
  pof_app_individuo %>% 
  group_by(sexo) %>% 
  summarise(
    n = n()/1458,
    freq = weighted.mean(despesas_mes, PESO_FINAL))

s1<-
  sexo %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(sexo, n), fill = '#ffd723') + #ffd723
  scale_y_continuous(labels = scales::percent, limits = c(0,.7)) +
  labs(
    x = 'Sexo', y = 'Porcentagem dos Usuários',
    title = 'Usuários de ride-hailing por sexo') +
  theme_minimal()

s2<-
  sexo %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(sexo, freq), fill = '#ffd723') + #ffd723
  scale_y_continuous(limits = c(0,8)) +
  labs(
    x = 'Sexo', y = 'Viagens por mês',
    title = 'Freqûencia de uso por sexo') +
  theme_minimal()

s3 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(sexo,gasto_avg, weight = PESO_FINAL), color = '#00d042') +
  scale_y_continuous(limits = c(0,100)) +
  labs(
    x = 'Sexo', y = 'Ticket Médio (R$)',
    title = 'Custo médio das viagens por sexo') +
  theme_minimal()

s4 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(sexo,gasto_anual/12, weight = PESO_FINAL), color = '#00d042') +
  scale_y_continuous(limits = c(0,500)) +
  labs(
    x = 'Sexo', y = 'Gasto mensal médio (R$)',
    title = 'Gasto mensal médio por sexo') +
  theme_minimal()

s_final <-
  (s1 + s2)/(s3 + s4)

s_final

##############################################################################

# Cor ----------------------------------

cor <-
  pof_app_individuo %>% 
  group_by(cor) %>% 
  summarise(
    n = n()/1458,
    freq = weighted.mean(despesas_mes, PESO_FINAL))

c1<-
  cor %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(cor, n), fill = '#ffd723') + #ffd723
  scale_y_continuous(labels = scales::percent, limits = c(0,.6)) +
  labs(
    x = 'Cor', y = 'Porcentagem dos Usuários',
    title = 'Usuários de ride-hailing por cor') +
  theme_minimal()

c2<-
  cor %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(cor, freq), fill = '#ffd723') + #ffd723
  scale_y_continuous(limits = c(0,10)) +
  labs(
    x = 'Cor', y = 'Viagens por mês',
    title = 'Freqûencia de uso por cor') +
  theme_minimal()

c3 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(cor,gasto_avg, weight = PESO_FINAL), color = '#00d042') +
  scale_y_continuous(limits = c(0,100)) +
  labs(
    x = 'Cor', y = 'Ticket Médio (R$)',
    title = 'Custo médio das viagens por cor') +
  theme_minimal()

c4 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(cor,gasto_anual/12, weight = PESO_FINAL), color = '#00d042') +
  scale_y_continuous(limits = c(0,500)) +
  labs(
    x = 'Cor', y = 'Gasto mensal médio (R$)',
    title = 'Gasto mensal médio por cor') +
  theme_minimal()

c_final <-
  (c1 + c2)/(c3 + c4)

c_final

##############################################################################

# Renda ----------------------------------

renda <-
  pof_app_individuo %>% 
  group_by(decil_renda) %>% 
  summarise(
    n = n()/1458,
    freq = weighted.mean(despesas_mes, PESO_FINAL))

r1<-
  renda %>% 
  na.omit() %>% 
  mutate(decil_renda = as.factor(decil_renda)) %>% 
  ggplot() +
  geom_col(aes(decil_renda, n), fill = '#ffd723') + #ffd723
  scale_y_continuous(labels = scales::percent, limits = c(0,.2)) +
  labs(
    x = 'Decil de Renda', y = 'Porcentagem dos Usuários',
    title = 'Usuários de ride-hailing por decil de renda') +
  theme_minimal()

r2<-
  renda %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(as.factor(decil_renda), freq), fill = '#ffd723') + #ffd723
  scale_y_continuous(limits = c(0,8)) +
  labs(
    x = 'Decil de Renda', y = 'Viagens por mês',
    title = 'Freqûencia de uso por decil de renda') +
  theme_minimal()

r3 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(decil_renda),gasto_avg, weight = PESO_FINAL), color = '#00d042') +
  scale_y_continuous(limits = c(0,100)) +
  labs(
    x = 'Decil de Renda', y = 'Ticket Médio (R$)',
    title = 'Custo médio das viagens por decil de renda') +
  theme_minimal()

r4 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(as.factor(decil_renda),gasto_anual/12, weight = PESO_FINAL), color = '#00d042') +
  scale_y_continuous(limits = c(0,500)) +
  labs(
    x = 'Decil de Renda', y = 'Gasto mensal médio (R$)',
    title = 'Gasto mensal médio por decil de renda') +
  theme_minimal()

r_final <-
  (r1 + r2)/(r3 + r4)

r_final

# Escolaridade ----------------------------------

escolaridade <-
  pof_app_individuo %>% 
  group_by(escolaridade) %>% 
  summarise(
    n = n()/1458,
    freq = weighted.mean(despesas_mes, PESO_FINAL))

e1<-
  escolaridade %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(escolaridade, n), fill = '#ffd723') + #ffd723
  scale_y_continuous(labels = scales::percent, limits = c(0,.5)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Escolaridade', y = 'Porcentagem dos Usuários',
    title = 'Usuários de ride-hailing por escolaridade') +
  theme_minimal()

e2<-
  escolaridade %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(escolaridade, freq), fill = '#ffd723') + #ffd723
  scale_y_continuous(limits = c(0,10)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Escolaridade', y = 'Viagens por mês',
    title = 'Freqûencia de uso por escolaridade') +
  theme_minimal()

e3 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(escolaridade,gasto_avg, weight = PESO_FINAL), color = '#00d042') +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Escolaridade', y = 'Ticket Médio (R$)',
    title = 'Custo médio das viagens por escolaridade') +
  theme_minimal()

e4 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(escolaridade,gasto_anual/12, weight = PESO_FINAL), color = '#00d042') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(limits = c(0,500)) +
  labs(
    x = 'Escolaridade', y = 'Gasto mensal médio (R$)',
    title = 'Gasto mensal médio por escolaridade') +
  theme_minimal()

e_final <-
  (e1 + e2)/(e3 + e4)

e_final

# Ocupação ----------------------------------

ocup <-
  pof_app_individuo %>% 
  group_by(ocup) %>% 
  summarise(
    n = n()/1458,
    freq = weighted.mean(despesas_mes, PESO_FINAL))

o1<-
  ocup %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(ocup, n), fill = '#ffd723') + #ffd723
  scale_y_continuous(labels = scales::percent, limits = c(0,.5)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Ocupação', y = 'Porcentagem dos Usuários',
    title = 'Usuários de ride-hailing por ocupação') +
  theme_minimal()

o2<-
  ocup %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(ocup, freq), fill = '#ffd723') + #ffd723
  scale_y_continuous(limits = c(0,10)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Ocupação', y = 'Viagens por mês',
    title = 'Freqûencia de uso por ocupação') +
  theme_minimal()

o3 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(ocup,gasto_avg, weight = PESO_FINAL), color = '#00d042') +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Ocupação', y = 'Ticket Médio (R$)',
    title = 'Custo médio das viagens por ocupação') +
  theme_minimal()

o4 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(ocup,gasto_anual/12, weight = PESO_FINAL), color = '#00d042') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(limits = c(0,500)) +
  labs(
    x = 'Ocupação', y = 'Gasto mensal médio (R$)',
    title = 'Gasto mensal médio por ocupação') +
  theme_minimal()

o_final <-
  (o1 + o2)/(o3 + o4)

o_final


# Commute ----------------------------------

commute <-
  pof_app_individuo %>% 
  group_by(commute) %>% 
  summarise(
    n = n()/1458,
    freq = weighted.mean(despesas_mes, PESO_FINAL))

o1<-
  commute %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(commute, n), fill = '#ffd723') + #ffd723
  scale_y_continuous(labels = scales::percent, limits = c(0,.5)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Tempo de deslocamento', y = 'Porcentagem dos Usuários',
    subtitle = 'Usuários de ride-hailing por tempo de deslocamento até o trabalho') +
  theme_minimal()

o2<-
  commute %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(commute, freq), fill = '#ffd723') + #ffd723
  scale_y_continuous(limits = c(0,15)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Tempo de deslocamento', y = 'Viagens por mês',
    subtitle = 'Freqûencia de uso por tempo de deslocamento até o trabalho') +
  theme_minimal()

o3 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(commute,gasto_avg, weight = PESO_FINAL), color = '#00d042') +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Tempo de deslocamento', y = 'Ticket Médio (R$)',
    subtitle = 'Custo médio das viagens por tempo de deslocamento até o trabalho') +
  theme_minimal()

o4 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(commute,gasto_anual/12, weight = PESO_FINAL), color = '#00d042') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(limits = c(0,500)) +
  labs(
    x = 'Tempo de deslocamento', y = 'Gasto mensal médio (R$)',
    subtitle  = 'Gasto mensal médio por tempo de deslocamento até o trabalho') +
  theme_minimal()

o_final <-
  (o1 + o2)/(o3 + o4)

o_final

# Salarios ----------------------------------

salario <-
  pof_app_individuo %>% 
  group_by(salario) %>% 
  summarise(
    n = n()/1458,
    freq = weighted.mean(despesas_mes, PESO_FINAL))

o1<-
  salario %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(salario, n), fill = '#ffd723') + #ffd723
  scale_y_continuous(labels = scales::percent, limits = c(0,.3)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Salário', y = 'Porcentagem dos Usuários',
    title = 'Usuários de ride-hailing por salário') +
  theme_minimal()

o2<-
  salario %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(salario, freq), fill = '#ffd723') + #ffd723
  scale_y_continuous(limits = c(0,10)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Salário', y = 'Viagens por mês',
    title = 'Freqûencia de uso por salário') +
  theme_minimal()

o3 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(salario,gasto_avg, weight = PESO_FINAL), color = '#00d042') +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Salário', y = 'Ticket Médio (R$)',
    subtitle = 'Custo médio das viagens por salário') +
  theme_minimal()

o4 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(salario,gasto_anual/12, weight = PESO_FINAL), color = '#00d042') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(limits = c(0,500)) +
  labs(
    x = 'Salário', y = 'Gasto mensal médio (R$)',
    subtitle  = 'Gasto mensal médio por salário') +
  theme_minimal()

o_final <-
  (o1 + o2)/(o3 + o4)

o_final

# regiaos ----------------------------------

regiao <-
  pof_app_individuo %>% 
  group_by(regiao) %>% 
  summarise(
    n = n()/1458,
    freq = weighted.mean(despesas_mes, PESO_FINAL))

o1<-
  regiao %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(regiao, n), fill = '#ffd723') + #ffd723
  scale_y_continuous(labels = scales::percent, limits = c(0,.5)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Região', y = 'Porcentagem dos Usuários',
    title = 'Usuários de ride-hailing por região') +
  theme_minimal()

o2<-
  regiao %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(regiao, freq), fill = '#ffd723') + #ffd723
  scale_y_continuous(limits = c(0,10)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Região', y = 'Viagens por mês',
    title = 'Freqûencia de uso por região') +
  theme_minimal()

o3 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(regiao,gasto_avg, weight = PESO_FINAL), color = '#00d042') +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Região', y = 'Ticket Médio (R$)',
    title = 'Custo médio das viagens por região') +
  theme_minimal()

o4 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(regiao,gasto_anual/12, weight = PESO_FINAL), color = '#00d042') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(limits = c(0,500)) +
  labs(
    x = 'Região', y = 'Gasto mensal médio (R$)',
    title  = 'Gasto mensal médio por região') +
  theme_minimal()

o_final <-
  (o1 + o2)/(o3 + o4)

o_final

# Status ----------------------------------

status <-
  pof_app_individuo %>% 
  group_by(status) %>% 
  summarise(
    n = n()/1458,
    freq = weighted.mean(despesas_mes, PESO_FINAL))

o1<-
  status %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(status, n), fill = '#ffd723') + #ffd723
  scale_y_continuous(labels = scales::percent, limits = c(0,.8)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Status', y = 'Porcentagem dos Usuários',
    title = 'Usuários de ride-hailing por status') +
  theme_minimal()

o2<-
  status %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(status, freq), fill = '#ffd723') + #ffd723
  scale_y_continuous(limits = c(0,10)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Status', y = 'Viagens por mês',
    title = 'Freqûencia de uso por status') +
  theme_minimal()

o3 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(status,gasto_avg, weight = PESO_FINAL), color = '#00d042') +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Status', y = 'Ticket Médio (R$)',
    title = 'Custo médio das viagens por status') +
  theme_minimal()

o4 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(status,gasto_anual/12, weight = PESO_FINAL), color = '#00d042') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(limits = c(0,500)) +
  labs(
    x = 'Status', y = 'Gasto mensal médio (R$)',
    title  = 'Gasto mensal médio por status') +
  theme_minimal()

o_final <-
  (o1 + o2)/(o3 + o4)

o_final

# Casa ----------------------------------

casa <-
  pof_app_individuo %>% 
  group_by(casa) %>% 
  summarise(
    n = n()/1458,
    freq = weighted.mean(despesas_mes, PESO_FINAL))

o1<-
  casa %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(casa, n), fill = '#ffd723') + #ffd723
  scale_y_continuous(labels = scales::percent, limits = c(0,.6)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Habitação', y = 'Porcentagem dos Usuários',
    title = 'Usuários de ride-hailing por tipo de habitação e rua') +
  theme_minimal()

o2<-
  casa %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(casa, freq), fill = '#ffd723') + #ffd723
  scale_y_continuous(limits = c(0,10)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Habitação', y = 'Viagens por mês',
    title = 'Freqûencia de uso por tipo de habitação e rua') +
  theme_minimal()

o3 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(casa,gasto_avg, weight = PESO_FINAL), color = '#00d042') +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = 'Habitação', y = 'Ticket Médio (R$)',
    title = 'Custo médio das viagens por tipo de habitação e rua') +
  theme_minimal()

o4 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(casa,gasto_anual/12, weight = PESO_FINAL), color = '#00d042') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(limits = c(0,500)) +
  labs(
    x = 'Habitação', y = 'Gasto mensal médio (R$)',
    title  = 'Gasto mensal médio por tipo de habitação e rua') +
  theme_minimal()

o_final <-
  (o1 + o2)/(o3 + o4)

o_final



# Casa ----------------------------------

rm <-
  pof_app_individuo %>% 
  group_by(RM) %>% 
  summarise(
    n = n()/1458,
    freq = weighted.mean(despesas_mes, PESO_FINAL))

o1<-
  rm %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(RM, n), fill = '#ffd723') + #ffd723
  scale_y_continuous(labels = scales::percent, limits = c(0,.3)) +
  labs(
    x = '', y = 'Porcentagem dos Usuários',
    title = 'Usuários de ride-hailing por região metropolitana') +
  coord_flip() +
  theme_minimal()

o2<-
  rm %>% 
  na.omit() %>% 
  ggplot() +
  geom_col(aes(RM, freq), fill = '#ffd723') + #ffd723
  scale_y_continuous(limits = c(0,10)) +
  coord_flip() +
  labs(
    x = '', y = 'Viagens por mês',
    title = 'Freqûencia de uso por região metropolitana') +
  theme_minimal()

o3 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(RM,gasto_avg, weight = PESO_FINAL), color = '#00d042') +
  scale_y_continuous(limits = c(0,100)) +
  coord_flip() +
  labs(
    x = '', y = 'Ticket Médio (R$)',
    title = 'Custo médio das viagens por região metropolitana') +
  theme_minimal()

o4 <-
  pof_app_individuo %>% 
  na.omit() %>% 
  ggplot() +
  geom_boxplot(aes(RM,gasto_anual/12, weight = PESO_FINAL), color = '#00d042') +
  coord_flip() +
  scale_y_continuous(limits = c(0,500)) +
  labs(
    x = '', y = 'Gasto mensal médio (R$)',
    title  = 'Gasto mensal médio por região metropolitana') +
  theme_minimal()

o_final <-
  (o1 + o2)/(o3 + o4)

o_final