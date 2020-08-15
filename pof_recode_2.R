


pof_uber <-
  pof_uber %>% 
  mutate(
    celular = ifelse(Modo == 'Celular',1,0),
    internet = ifelse(Modo == 'Streaming',1,0),
    bike = ifelse(Modo == 'Gastos com Bicicleta',1,0),
    carro = ifelse(Modo == 'Impostos e Taxas Veiculares',1,0),
    casa = paste(tipo_dom,rua,sep = " "),
    comodos_morador = comodos/n_moradores,
    status_casa = 
      ifelse(agua == 'Outra Forma' & energia == "Não" & esgoto == 'Não', "Muito Vulnerável",
      ifelse(
        agua == 'Outra Forma' & energia == "Não" |
        agua == 'Outra Forma' & esgoto == "Não"  |
        esgoto == 'Não' & energia == 'Não', "Vulnerável",
      ifelse(agua == 'Outra Forma' | energia == "Não" | esgoto == 'Não', 'Pouco Vulnerável',
      "Não Vulnerável"))),
    escolaridade = 
      ifelse(concluiu == "Sim", ultimo_curso,
      ifelse(graduacao == "Sim", "Superior",
      ifelse(estudante_dummy == "Não", ultimo_curso,
      ifelse(estudante_dummy == "Sim", curso_atual,
      ifelse(ANOS_ESTUDO <= 10, "Ensino Fundamental",
      ifelse(ANOS_ESTUDO <= 16, "Ensino Médio",
      "Sem declaração")))))),
    status_pessoa = 
      ifelse(estudante_dummy == "Sim" & trabalho == "Sim", "Trabalha e Estuda",
      ifelse(estudante_dummy == "Sim" & trabalho == "Não", "Só Estuda",
      ifelse(estudante_dummy == "Não" & trabalho == "Sim", "Só Trabalha",
      "Nem-Nem"))),
    ocup = 
      ifelse(is.na(ocup) == T & trabalho == "Não", "Não Trabalha",
      ifelse(is.na(ocup) == T & trabalho == "Sim", "Sem declaração",
      ocup))
  ) %>% 
  group_by(ID_MORADOR) %>% 
  mutate(
    Celular = max(celular),
    Internet = max(celular),
    Bike = max(celular),
    Carro = max(celular)
  ) %>% 
  ungroup() %>% 
  filter(
    Modo != 'Celular' & 
    Modo != 'Streaming' &
    Modo != 'Gastos com Bicicleta' &
    Modo != 'Impostos e Taxas Veiculares') %>% 
  select(
    -celular,-internet,-bike,-carro, -Ano, -tipo_dom, -rua, 
    -comodos, -n_moradores, -trabalho, -graduacao,
    -agua,-esgoto,-energia, -concluiu, -ultimo_curso, 
    -curso_atual, -estudante_dummy,-salario, -ANOS_ESTUDO)