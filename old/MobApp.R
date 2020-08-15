source('setup.R')

pof_total <-
  fread('pof_total_urbano.csv') 

ano <- c('2002','2008','2017')

for (i in ano) {
  assign(paste0('pof_',i),
  pof %>% 
    subset(Ano == i))
}

pof_2002 <-
  pof_2002 %>% 
  select(-V1) %>% 
  mutate(
    QUADRO = 
    ifelse(
      QUADRO == '23' |
      QUADRO == '43' |
      QUADRO == '50' , 'Transporte Urbano',
    ifelse(QUADRO == '24', 'Alimentação',
    ifelse(QUADRO == '28', 'Cultura/Lazer/Esporte',
    ifelse(
      QUADRO == '34' |
      QUADRO == '35' |
      QUADRO == '36 '|
      QUADRO == '38' , 'Roupas e Calçados',
    ifelse(QUADRO == '41', 'Viagens',
    'drop')))))
  ) %>%
  filter(QUADRO != 'drop')

pof_2008 <-
  pof_2008 %>% 
  select(-V1) %>% 
  mutate(
    QUADRO = 
    ifelse(
      QUADRO == '23' |
      QUADRO == '43' |
      QUADRO == '50' , 'Transporte Urbano',
    ifelse(QUADRO == '24', 'Alimentação',
    ifelse(QUADRO == '28', 'Cultura/Lazer/Esporte',
    ifelse(
      QUADRO == '34' |
      QUADRO == '35' |
      QUADRO == '36 '|
      QUADRO == '38' , 'Roupas e Calçados',
    ifelse(QUADRO == '41', 'Viagens',
    'drop')))))
  ) %>%
  filter(QUADRO != 'drop')

pof_2017 <-
  pof_2017 %>% 
  select(-V1) %>% 
  mutate(
    QUADRO = 
      ifelse(
        QUADRO == '23' |
        QUADRO == '33' |
        QUADRO == '50' , 'Transporte Urbano',
      ifelse(QUADRO == '24', 'Alimentação',
      ifelse(QUADRO == '28', 'Cultura/Lazer/Esporte',
      ifelse(
        QUADRO == '34' |
        QUADRO == '35' |
        QUADRO == '36' |
        QUADRO == '38' , 'Roupas e Calçados',
      ifelse(QUADRO == '41', 'Viagens',
      'drop')))))
  ) %>% 
  filter(QUADRO != 'drop')

plot1 <- 
  bind_rows(
    pof_2002,pof_2008,pof_2017
  ) %>% 
  group_by(Ano, QUADRO, ID_FAMILIA, renda_total, PESO_FINAL) %>% 
  summarise(
    gasto = sum(valor_total)
  ) %>% 
  mutate(prop = gasto / renda_total) %>% 
  group_by(Ano, QUADRO) %>% 
  summarise(prop = weighted.mean(prop, PESO_FINAL))

plot1 %>% 
  ggplot(aes(as.factor(Ano), prop, group = QUADRO)) +
  geom_line(aes(color = QUADRO))
