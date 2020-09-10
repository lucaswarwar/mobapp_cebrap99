######################

source("00_setup.R.R")

### Recover dataset ###

pof_data <- 
  readr::read_rds("01_prepare_data/mobapp_individuo.rds")

# Plot 9: frequency vs cost by city ---------------------------------

plot9 <-
pof_data %>% 
  filter(Modo == 'Ride-hailing') %>% 
  mutate(
    frequencia_avg = weighted.mean(despesas_mes,PESO_FINAL),
    custo_avg = weighted.mean(gasto_avg,PESO_FINAL)) %>% 
  group_by(UF, Estrato) %>% 
  summarise(
    frequencia_avg = mean(frequencia_avg),
    custo_avg = mean(custo_avg),
    frequencia = weighted.mean(despesas_mes,PESO_FINAL),
    custo = weighted.mean(gasto_avg,PESO_FINAL))

View(plot9)

plot9$Estrato <- factor(plot9$Estrato, levels = c('Capital','RM da Capital','Interior Urbano'))

plot9 %>%
  filter(Estrato != "Interior Rural") %>% 
  ggplot() +
  geom_point(aes(frequencia, custo, fill=Estrato), shape=21,size=3.5) +
  ggsci::scale_fill_locuszoom() +
  theme_minimal() +
  theme(legend.position = 'bottom')