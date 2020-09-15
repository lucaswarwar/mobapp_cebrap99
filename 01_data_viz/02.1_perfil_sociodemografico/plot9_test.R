######################

source("00_setup.R.R")

### Recover dataset ###

pof_data <- 
  readr::read_rds("01_prepare_data/mobapp_individuo.rds")

# Plot 9: frequency vs cost by city ---------------------------------

# frequency vs cost by estrato

plot9 <-
pof_data %>% 
  filter(Modo == 'Ride-hailing') %>% 
  mutate(
    frequencia_avg = weighted.mean(despesas_mes,PESO_FINAL),
    custo_avg = weighted.mean(gasto_avg,PESO_FINAL)) %>% 
  group_by(UF, Estrato, sexo) %>% 
  summarise(
    frequencia_avg = mean(frequencia_avg),
    custo_avg = mean(custo_avg),
    frequencia = weighted.mean(despesas_mes,PESO_FINAL),
    custo = weighted.mean(gasto_avg,PESO_FINAL))


plot9$Estrato <- factor(plot9$Estrato, levels = c('Capital','RM da Capital','Interior Urbano'))

p1<-
plot9 %>%
  filter(Estrato != "Interior Rural") %>% 
  ggplot() +
  geom_jitter(aes(frequencia, custo, fill=Estrato), shape=21,size=3.5) +
  ggsci::scale_fill_locuszoom() +
  theme_minimal() +
  scale_x_continuous(limits = c(4.5,20)) +
  scale_y_continuous(limits = c(5,50))+
  labs(x= "Nº médio de viagens por mês", y="Custo médio da viagem (R$)")+
  theme(legend.position = 'bottom')

# frequency by rent -----------------------

plot10 <-
  pof_data %>% 
  na.omit() %>% 
  filter(Modo == 'Ride-hailing') %>% 
  mutate(
    aluguel_mensal = aluguel_anual/12,
    aluguel = case_when(
      aluguel_mensal < 500 ~ 'Até 500 reais',
      aluguel_mensal < 1000 ~ '500 - 1.000 reais',
      aluguel_mensal < 2500 ~ '1.000 - 2.500 reais',
      aluguel_mensal <= 5000 ~ '2.500 - 5.000 reais',
      aluguel_mensal > 5000 ~ 'Mais de 5.000 reais')) %>% 
  group_by(aluguel, RM) %>% 
  summarise(
    frequencia = weighted.mean(despesas_mes,PESO_FINAL,na.rm = T),
    gasto = weighted.mean(gasto_avg,PESO_FINAL,na.rm = T))

plot10$aluguel <- factor(plot10$aluguel, levels = c(
  'Até 500 reais', '500 - 1.000 reais','1.000 - 2.500 reais',
      '2.500 - 5.000 reais','Mais de 5.000 reais'))

p2<-
plot10 %>% 
  ggplot() +
  geom_jitter(aes(frequencia, gasto, fill=aluguel), shape=21,size=3.5) +
  
  scale_fill_viridis_d(option = 'magma',direction = -1)+
  theme_minimal() +
  scale_x_continuous(limits = c(4.5,15)) +
  scale_y_continuous(limits = c(7.5,50))+
  labs(x= "Nº médio de viagens por mês", y="Custo médio da viagem (R$)",fill="Aluguel mensal")+
  theme(legend.position = 'bottom') 

p<-p1/p2
p
