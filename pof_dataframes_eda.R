source('Setup.R')

pof_z <-
  fread('pof_eda.csv')

renda_rm <-
  pof_z %>% 
  group_by(Modo, RM) %>% 
  mutate(total_modo = n()) %>% 
  ungroup() %>% 
  group_by(Modo, RM, quintil_renda) %>% 
  summarise(
    share = mean(n()/total_modo),
    frequencia = weighted.mean(despesas_mes, PESO_FINAL),
    frequencia_sd = sd(despesas_mes),
    ticket = weighted.mean(gasto_avg, PESO_FINAL),
    ticket_sd = sd(gasto_avg),
    gasto = weighted.mean(gasto_anual, PESO_FINAL),
    gasto_sd = sd(gasto_anual)
  )

csv <- list(
  casa, condicao, cor, decil_renda, UF,
  escolaridade, estrato, faixa_etaria, ocup,
  RM, sexo, renda_rm, status_casa, status_pessoa
)

write.csv(casa, 'casa.csv')
write.csv(condicao, 'condicao.csv')
write.csv(cor, 'cor.csv')
write.csv(decil_renda, 'renda.csv')
write.csv(UF, 'uf.csv')
write.csv(escolaridade, 'escolaridade.csv')
write.csv(estrato, 'estrato.csv')
write.csv(faixa_etaria, 'idade.csv')
write.csv(ocup, 'ocupacao.csv')
write.csv(RM, 'rm.csv')
write.csv(sexo, 'sexo.csv')
write.csv(renda_rm, 'renda_rm.csv')
write.csv(status_casa, 'status_casa.csv')
write.csv(status_pessoa, 'status_pessoa.csv')
write.csv(pof_uber, 'pof_uber.csv')
