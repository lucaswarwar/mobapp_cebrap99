### Setup

source("setup.R")

setwd("C:/Users/lucas/Desktop/MobApp_CEBRAP-99")

pof_uber <- fread('pof_uber.csv')

pof_uber <-
  # Cria df Brasil Urbano
  pof_uber %>%
  # Nomeia Regiões Metropolitanas
  dplyr::mutate(
   Ano = '2017',
   RM = 
   ifelse(Estrato == "Interior Rural", "Zona Rural",
   ifelse(Estrato != "Interior Urbano" & UF == "SP", "São Paulo",
   ifelse(Estrato != "Interior Urbano" & UF == "RJ", "Rio de Janeiro",
   ifelse(Estrato != "Interior Urbano" & UF == "PR", "Curitiba",
   ifelse(Estrato != "Interior Urbano" & UF == "RS", "Porto Alegre",
   ifelse(Estrato != "Interior Urbano" & UF == "BA", "Salvador",
   ifelse(Estrato != "Interior Urbano" & UF == "PE", "Recife",
   ifelse(Estrato != "Interior Urbano" & UF == "CE", "Fortaleza",
   ifelse(Estrato != "Interior Urbano" & UF == "PA", "Belém",
   ifelse(Estrato != "Interior Urbano" & UF == "MG", "Belo Horizonte",
   ifelse(Estrato != "Interior Urbano" & UF == "DF", "Brasília",
   ifelse(Estrato != "Interior Urbano" & UF == "SC", "Florianópolis",
   ifelse(Estrato != "Interior Urbano" & UF == "ES", "Vitória",
   ifelse(Estrato != "Interior Urbano" & UF == "MT", "Cuiabá",
   ifelse(Estrato != "Interior Urbano" & UF == "MS", "Campo Grande",
   ifelse(Estrato != "Interior Urbano" & UF == "GO", "Goiânia",
   ifelse(Estrato != "Interior Urbano" & UF == "AM", "Manaus",
   ifelse(Estrato != "Interior Urbano" & UF == "MA", "São Luís",
   ifelse(Estrato != "Interior Urbano" & UF == "AL", "Maceió",
   "Brasil Urbano")))))))))))))))))))
  )

pof_uber <-
  pof_uber %>% 
  as.data.table()

pof_uber[, decil_renda := cut(
  x = renda_pc,
  breaks = Hmisc::wtd.quantile(
    x = renda_pc, weights = PESO_FINAL, probs = 0:10/10,
    type = c('quantile','(i-1)/(n-1)', 'i/(n+1)','i/n'),
    normwt = F, na.rm = T),
  labels = F, include.lowest = T),
  by = .(Ano, RM)]

pof_uber[, quintil_renda := cut(
  x = renda_pc,
  breaks = Hmisc::wtd.quantile(
    x = renda_pc, weights = PESO_FINAL, probs = 0:5/5,
    type = c('quantile','(i-1)/(n-1)', 'i/(n+1)','i/n'),
    normwt = F, na.rm = T),
  labels = F, include.lowest = T),
  by = .(Ano, RM)]



pof_individuo2 <-
  pof_uber %>% 
  group_by(
    ID_MORADOR, PESO_FINAL, renda_pc, Modo, escolaridade,
    faixa_etaria, sexo, cor, regiao, UF, Estrato, RM, commute,
    ocup, decil_renda, quintil_renda, casa, comodos_morador,
    aluguel, CONDICAO, status_casa, status_pessoa,
    Celular, Bike, Internet, Carro
  ) %>% 
  summarise(
    gasto_avg = mean(valor_despesa),
    gasto_anual = sum(valor_despesa*FATOR_ANUALIZACAO),
    despesas_mes = sum(FATOR_ANUALIZACAO)/12
  )%>% 
   mutate(
     taxi_freq = ifelse(Modo == 'Táxi' & despesas_mes > 4, 1,0),
     tp_freq = ifelse(Modo == 'Transporte Público' & despesas_mes > 4, 1,0)
  ) %>% 
  group_by(ID_MORADOR) %>% 
  mutate(
    taxi_frequente = max(taxi_freq),
    tp_frequente = max(tp_freq)) %>% 
  ungroup() %>% 
  select(-taxi_freq,-tp_freq)

pof_app_individuo3 <-
  pof_app_individuo2 %>% 
  group_by(
    PESO_FINAL, ID_MORADOR, aluguel, CONDICAO, renda_pc,
    faixa_etaria, sexo, cor, regiao, UF, Estrato, Modo, ocup,
    commute, horas, casa, comodos_morador, status_casa, status_pessoa,
    escolaridade, Celular, Internet, Carro, Bike, RM, decil_renda, quintil_renda
  ) %>% 
  summarise(
    gasto_avg = mean(valor_despesa),
    gasto_anual = sum(valor_despesa*FATOR_ANUALIZACAO),
    despesas_mes = sum(FATOR_ANUALIZACAO)/12
  )

teste <-
  pof_uber %>% 
  group_by(Modo) %>% 
  summarise(n = n_distinct(ID_MORADOR))
  n_distinct(ID_MORADOR)