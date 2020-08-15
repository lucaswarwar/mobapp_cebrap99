### Setup

source("setup.R")


######## 1. Dowload and Clean data ----------------------------------------------------------

#####################
### POF 2017-2018 ###
#####################

setwd("~/POF/2017_2018")

# Despesa Individual ----------------

despesa_individual <- # Recuperando microdados de despesa
  readRDS("DESPESA_INDIVIDUAL.rds")

total_despesas_transporte <- 
  despesa_individual %>% 
  # Seleciona Variáveis
  dplyr::select(           
    UF,
    ESTRATO_POF,
    COD_UPA,
    NUM_DOM,
    NUM_UC,
    COD_INFORMANTE,
    QUADRO,
    COD_ITEM = V9001,
    TP_PAG = V9002,
    motivo_viagem = V4104,
    destino_viagem = V4105,
    valor_despesa = V8000_DEFLA,
    FATOR_ANUALIZACAO,
    PESO_FINAL,
    RENDA_TOTAL
  ) %>%
  dplyr::mutate(
  # Cria ID para FAMÍLIA, DOMICÍLIO e INDIVÍDUO
    ID_DOM = paste(COD_UPA, NUM_DOM, sep = ""),
    ID_FAMILIA = paste(COD_UPA, NUM_DOM, NUM_UC, sep = ""),
    ID_MORADOR = paste(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = "")
  )  %>%
  dplyr::filter(
  # Filtra Despesas de interesse  
    QUADRO == "23"                            | # Transporte em geral
    QUADRO == '50'                            | # Taxas e Impostos Veiculares
    COD_ITEM >= 2803001 & COD_ITEM <= 2803016 | # Serviços de Streaming
    COD_ITEM >= 3300401 & COD_ITEM <= 3302301 | # Gastos com veículo
    COD_ITEM >= 3300301 & COD_ITEM <= 3300312 | # Gastos com bike
    COD_ITEM >= 4100201 & COD_ITEM <= 4101102 | 
    COD_ITEM == '4106401'                     | # Gastos com transporte em viagens
    COD_ITEM >= 4400201 & COD_ITEM <= 4400801   # Gastos com celular
  ) %>% 
  dplyr::select(
    UF,
    ESTRATO_POF,
    RENDA_TOTAL,
    QUADRO,
    COD_ITEM,
    TP_PAG,
    motivo_viagem,
    destino_viagem,
    PESO_FINAL,
    ID_DOM,
    ID_FAMILIA,
    ID_MORADOR,
    valor_despesa,
    FATOR_ANUALIZACAO
  )

# Moradores ------------------------------

moradores <- # Recupera microdados de características dos indivíduos
  readRDS("MORADOR.rds")

moradores <- # Seleciona variáveis
  moradores %>% # Calcula ID para o indivíduo
  dplyr::select(
    COD_UPA,
    NUM_DOM,
    NUM_UC,
    COD_INFORMANTE,
    CONDICAO = V0306,
    IDADE = V0403,
    SEXO = V0404,
    COR = V0405,
    trabalho = V0407,
    estudante = V0419,
    graduacao = V0423,
    ultimo_curso = V0425,
    concluiu = V0430,
    N_CC = V0409,
    ANOS_ESTUDO,
  ) %>%
  dplyr::mutate(
    ID_MORADOR = paste(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = "")
  )

total_despesas_transporte <- # Seleciona variáveis
  moradores %>% # Join com df de despesas com transporte
  dplyr::select(
    CONDICAO,
    IDADE,
    SEXO,
    COR,
    trabalho,
    estudante,
    graduacao,
    ultimo_curso,
    concluiu,
    N_CC,
    ANOS_ESTUDO,
    ID_MORADOR,
  ) %>%
  dplyr::right_join(total_despesas_transporte, by = "ID_MORADOR")

#######################################################################

trabalho <- # Recupera microdados de características dos indivíduos
  readRDS("RENDIMENTO_TRABALHO.rds")

trabalho <- # Seleciona variáveis
  trabalho %>% # Calcula ID para o indivíduo
  dplyr::select(
    COD_UPA,
    NUM_DOM,
    NUM_UC,
    COD_INFORMANTE,
    SUB_QUADRO,
    ocup = V5302,
    horas = V5314,
    commute = V5315,
    salario = V8500_DEFLA
  ) %>%
  dplyr::mutate(
    ID_MORADOR = paste(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = "")
  )

total_despesas_transporte <- # Seleciona variáveis
  trabalho %>% 
  dplyr::filter(SUB_QUADRO == 1) %>% # Join com df de despesas com transporte
  dplyr::select(
    ocup,
    commute,
    horas,
    salario,
    ID_MORADOR,
  ) %>%
  dplyr::right_join(total_despesas_transporte, by = "ID_MORADOR")

#######################################################################

domicilio <- # Recupera microdados de características dos indivíduos
  readRDS("DOMICILIO.rds")

domicilio <- # Seleciona variáveis
  domicilio %>% # Calcula ID para o indivíduo
  dplyr::select(
    COD_UPA,
    NUM_DOM,
    tipo_dom = V0201,
    comodos = V0205,
    agua = V0207,
    esgoto = V0212,
    energia = V02141,
    aluguel = V0217,
    rua = V0220
  ) %>%
  dplyr::mutate(
    ID_DOM = paste(COD_UPA, NUM_DOM, sep = "")
  )

total_despesas_transporte <- # Seleciona variáveis
  domicilio %>% 
  dplyr::select(
    tipo_dom,
    comodos,
    agua,
    esgoto,
    energia,
    aluguel,
    rua,
    ID_DOM
  ) %>%
  dplyr::right_join(total_despesas_transporte, by = "ID_DOM")

######## 2. Recode data -------------------------------------------------------------------

total_despesas_transporte <- # Calcula renda per capita familiar
  total_despesas_transporte %>% 
  dplyr::group_by(ID_FAMILIA) %>% 
  dplyr::mutate(renda_pc = RENDA_TOTAL / n_distinct(ID_MORADOR)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(ID_DOM) %>% 
  dplyr::mutate(n_moradores = n_distinct(ID_MORADOR)) %>% 
  dplyr::ungroup()


total_despesas_transporte <- # Recodifica variáveis com informações dos indivíduos
  total_despesas_transporte %>%
  dplyr::mutate(
    salario = case_when(
      salario <= 1044 ~ "Menos de um salário mínimo",
      salario %in% c(1045:2090) ~ "Entre 1 e 2",
      salario %in% c(2091:3135) ~ "Entre 2 e 3",
      salario %in% c(3136:4180) ~ "Entre 3 e 4",
      salario %in% c(4181:5225) ~ "Entre 4 e 5",
      salario >= 5226 ~ "Mais de 5"),
    faixa_etaria = case_when(
      IDADE < 15       ~ "0-14",
      IDADE %in% 15:24 ~ "15-24",
      IDADE %in% 25:34 ~ "25-34", 
      IDADE %in% 35:49 ~ "35-49", 
      IDADE %in% 50:64 ~ "50-64", 
      IDADE > 64 ~ "65+"),
    sexo = ifelse(SEXO == 1, "Homem", "Mulher"),
    cor = 
      ifelse(COR == 1, "Branca",
      ifelse(COR == 2, "Preta",
      ifelse(COR == 4, "Parda",
      "Amarela, Indígena ou outra"))),
    regiao = 
      ifelse(UF < 20, "Norte",
      ifelse(UF < 30, "Nordeste",
      ifelse(UF < 40, "Sudeste",
      ifelse(UF < 50, "Sul",
      "Centro-Oeste")))),
    TP_PAG = 
      ifelse(TP_PAG <= 2, "Monetária à vista",
      ifelse(TP_PAG <= 4, "Monetária a prazo",
      ifelse(TP_PAG <= 6, "Cartão de Crédito",
      "Outros"))),
    ocup = 
      ifelse(ocup == 1, "Trabalhador Doméstico",
      ifelse(ocup == 2, "Militar",
      ifelse(ocup == 3, "Setor Privado",
      ifelse(ocup == 4, "Setor Público",
      ifelse(ocup == 5, "Empregador",
      ifelse(ocup == 6, "Autônomo",
      "Não Remunerado")))))),
    commute = 
      ifelse(commute == 1, "Até 5min",
      ifelse(commute == 2, "De 6 a 30min",
      ifelse(commute == 3, "De 30min a 1h",
      ifelse(commute == 4, "De 1h a 2h",
      ifelse(commute == 5, "2h ou mais",
      "Sem Declaração"))))),
    motivo_viagem =
      ifelse(motivo_viagem == 3, 'Negócios', 
      ifelse(is.na(motivo_viagem) == F, 'Lazer', 'NA')),
    destino_viagem = 
      ifelse(destino_viagem == 1, 'Nacional', 
      ifelse(destino_viagem == 2, 'Internacional', 'NA')),
    trabalho = 
      ifelse(trabalho == '1', 'Sim', 'Não'),
    tipo_dom =
      ifelse(tipo_dom == 1, 'Casa',
      ifelse(tipo_dom == 2, 'Apartamento',
      'Habitação Irregular')),
    agua = 
      ifelse(agua == 1, 'Rede geral', 'Outra forma'),
    esgoto = 
      ifelse(esgoto == 1, 'Sim', 'Não'),
    energia = 
      ifelse(energia == 1, 'Sim', 'Não'),
    aluguel =
      ifelse(aluguel <= 2, 'Próprio',
      ifelse(aluguel == 3, 'Alugado', 'Outros')),
    rua = 
      ifelse(rua == 1, 'Pavimentada', 'Não Pavimentada'),
    estudante_dummy =
      ifelse(is.na(estudante) == F, 'Sim', 'Não'),
    curso_atual = 
      ifelse(estudante <= 5, 'Ensino Fundamental',
      ifelse(estudante <= 7, 'Ensino Médio',
      ifelse(estudante <= 9, 'Superior',
      ifelse(estudante <= 11, 'Pós-graduação',
      'Sem declaração')))),
    ultimo_curso = 
      ifelse(ultimo_curso <= 8, 'Ensino Fundamental',
      ifelse(ultimo_curso <= 11, 'Ensino Médio',
      ifelse(ultimo_curso <= 13, 'Superior',
      ifelse(ultimo_curso <= 15, 'Pós-graduação',
      'Sem declaração')))),
    graduacao = 
      ifelse(graduacao == 1, 'Sim', 'Não'),
    concluiu = 
      ifelse(concluiu == 1, 'Sim', 'Não'),
    CONDICAO =
      ifelse(CONDICAO == 1, 'Referência',
      ifelse(CONDICAO <= 3, 'Cônjuge',
      ifelse(CONDICAO <= 7, 'Filho(a)',
      ifelse(CONDICAO <= 9, 'Pai ou Mãe',
      ifelse(CONDICAO == 10, 'Neto(a)',
      ifelse(CONDICAO == 13, 'Avô ou Avó',
      'Outro Parente')))))),
    Ano = "2017"
  )

total_despesas_transporte <-
  total_despesas_transporte %>% # Cria classes para os Estratos Urbano, Rural, RM e interior
  dplyr::mutate(
    ESTRATO_POF = as.numeric(ESTRATO_POF),
    Estrato = ifelse(
    ESTRATO_POF == 1101 | ESTRATO_POF == 1102 |
    ESTRATO_POF == 1201 |
    ESTRATO_POF >= 1301 & ESTRATO_POF <= 1306 |
    ESTRATO_POF == 1401 & ESTRATO_POF == 1402 |
    ESTRATO_POF >= 1501 & ESTRATO_POF <= 1503 |
    ESTRATO_POF >= 1601 & ESTRATO_POF <= 1602 |
    ESTRATO_POF == 1701 |
    ESTRATO_POF >= 2101 & ESTRATO_POF <= 2103 |
    ESTRATO_POF >= 2201 & ESTRATO_POF <= 2203 |
    ESTRATO_POF >= 2301 & ESTRATO_POF <= 2306 |
    ESTRATO_POF >= 2401 & ESTRATO_POF <= 2402 |
    ESTRATO_POF >= 2501 & ESTRATO_POF <= 2503 |
    ESTRATO_POF >= 2601 & ESTRATO_POF <= 2603 |
    ESTRATO_POF >= 2701 & ESTRATO_POF <= 2703 |
    ESTRATO_POF >= 2801 & ESTRATO_POF <= 2802 |
    ESTRATO_POF >= 2901 & ESTRATO_POF <= 2906 |
    ESTRATO_POF >= 3101 & ESTRATO_POF <= 3106 |
    ESTRATO_POF >= 3201 & ESTRATO_POF <= 3202 |
    ESTRATO_POF >= 3301 & ESTRATO_POF <= 3309 |
    ESTRATO_POF >= 3501 & ESTRATO_POF <= 3509 |
    ESTRATO_POF >= 4101 & ESTRATO_POF <= 4105 |
    ESTRATO_POF >= 4201 & ESTRATO_POF <= 4202 |
    ESTRATO_POF >= 4301 & ESTRATO_POF <= 4306 |
    ESTRATO_POF >= 5001 & ESTRATO_POF <= 5003 |
    ESTRATO_POF >= 5101 & ESTRATO_POF <= 5102 |
    ESTRATO_POF >= 5201 & ESTRATO_POF <= 5203 |
    ESTRATO_POF >= 5301 & ESTRATO_POF <= 5306,
    "Capital",
      ifelse(
      ESTRATO_POF == 1307 |
      ESTRATO_POF >= 1504 & ESTRATO_POF <= 1505 |
      ESTRATO_POF == 1603 |
      ESTRATO_POF == 2104 |
      ESTRATO_POF >= 2307 & ESTRATO_POF <= 2309 |
      ESTRATO_POF == 2403 |
      ESTRATO_POF >= 2504 & ESTRATO_POF <= 2505 |
      ESTRATO_POF >= 2604 & ESTRATO_POF <= 2606 |
      ESTRATO_POF == 2704 |
      ESTRATO_POF == 2803 |
      ESTRATO_POF >= 2907 & ESTRATO_POF <= 2909 |
      ESTRATO_POF >= 3107 & ESTRATO_POF <= 3109 |
      ESTRATO_POF >= 3203 & ESTRATO_POF <= 3205 |
      ESTRATO_POF >= 3310 & ESTRATO_POF <= 3318 |
      ESTRATO_POF >= 3510 & ESTRATO_POF <= 3515 |
      ESTRATO_POF >= 4106 & ESTRATO_POF <= 4108 |
      ESTRATO_POF >= 4203 & ESTRATO_POF <= 4204 |
      ESTRATO_POF >= 4307 & ESTRATO_POF <= 4309 |
      ESTRATO_POF == 5103 |
      ESTRATO_POF >= 5204 & ESTRATO_POF <= 5206,
      'RM da Capital',
        ifelse(
        ESTRATO_POF == 1103 | ESTRATO_POF == 1107 |
        ESTRATO_POF == 1202 |
        ESTRATO_POF >= 1308 & ESTRATO_POF <= 1310 |
        ESTRATO_POF == 1403 |
        ESTRATO_POF >= 1506 & ESTRATO_POF <= 1511 |
        ESTRATO_POF == 1604 |
        ESTRATO_POF >= 1702 & ESTRATO_POF <= 1705 |
        ESTRATO_POF >= 2105 & ESTRATO_POF <= 2113 |
        ESTRATO_POF >= 2204 & ESTRATO_POF <= 2209 |
        ESTRATO_POF >= 2310 & ESTRATO_POF <= 2320 |
        ESTRATO_POF >= 2404 & ESTRATO_POF <= 2408 |
        ESTRATO_POF >= 2506 & ESTRATO_POF <= 2511 |
        ESTRATO_POF >= 2607 & ESTRATO_POF <= 2615 |
        ESTRATO_POF >= 2705 & ESTRATO_POF <= 2708 |
        ESTRATO_POF >= 2804 & ESTRATO_POF <= 2806 |
        ESTRATO_POF >= 2910 & ESTRATO_POF <= 2925 |
        ESTRATO_POF >= 3110 & ESTRATO_POF <= 3130 |
        ESTRATO_POF >= 3206 & ESTRATO_POF <= 3211 |
        ESTRATO_POF >= 3319 & ESTRATO_POF <= 3330 |
        ESTRATO_POF >= 3516 & ESTRATO_POF <= 3536 |
        ESTRATO_POF >= 4109 & ESTRATO_POF <= 4124 |
        ESTRATO_POF >= 4205 & ESTRATO_POF <= 4217 |
        ESTRATO_POF >= 4310 & ESTRATO_POF <= 4324 |
        ESTRATO_POF >= 5004 & ESTRATO_POF <= 5009 |
        ESTRATO_POF >= 5104 & ESTRATO_POF <= 5112 |
        ESTRATO_POF >= 5207 & ESTRATO_POF <= 5217,
       "Interior Urbano",
        "Interior Rural")))
 )

pof_uber <-
  total_despesas_transporte %>% 
  mutate(
    Viagem = ifelse(QUADRO == 41, 'Sim', 'Não'),
    Modo = 
      ifelse(
        COD_ITEM >= 3300301 & COD_ITEM <= 3300312, 'Gastos com Bicicleta',
      ifelse(QUADRO == 28, 'Streaming',
      ifelse(QUADRO == 44, 'Celular',
      ifelse(QUADRO == 50, 'Impostos e Taxas Veiculares',
      ifelse(
        COD_ITEM <= 2300401 |
        COD_ITEM >= 2300701 & COD_ITEM <= 2300901 |
        COD_ITEM >= 2301101 & COD_ITEM <= 2301301 |
        COD_ITEM >= 2302301 & COD_ITEM <= 2303001 | 
        COD_ITEM >= 4100201 & COD_ITEM <= 4100501 |
        COD_ITEM >= 4100901 & COD_ITEM <= 4101001 , "Transporte Público",
      ifelse(
        COD_ITEM >= 2300401 & COD_ITEM <= 2300404 |
        COD_ITEM >= 4100601 & COD_ITEM <= 4100605 , 'Transporte Alternativo',
      ifelse(
        COD_ITEM >= 2300501 & COD_ITEM <= 2300602 |
        COD_ITEM >= 4100701 & COD_ITEM <= 4100801 , "Táxi",
      ifelse(
        COD_ITEM == 2303101 | COD_ITEM == 2303102 |
        COD_ITEM == 4106401 , "Aplicativo", "Transporte Privado")))
  ))))))


pof_uber <- # filtra e seleciona variáveis
  pof_uber %>%
  dplyr::select(
    PESO_FINAL,
    ID_MORADOR,
    ID_FAMILIA,
    ID_DOM,
    n_moradores,
    tipo_dom,
    comodos,
    agua,
    esgoto,
    energia,
    aluguel,
    rua,
    CONDICAO,
    RENDA_TOTAL,
    valor_despesa,
    FATOR_ANUALIZACAO,
    COD_ITEM,
    renda_pc,
    faixa_etaria,
    sexo,
    cor,
    ultimo_curso,
    graduacao,
    concluiu,
    estudante_dummy,
    curso_atual,
    regiao,
    ANOS_ESTUDO,
    TP_PAG,
    N_CC,
    Ano,
    UF,
    Estrato,
    Modo,
    ocup,
    commute,
    trabalho,
    salario,
    horas,
    Viagem,
    motivo_viagem,
    destino_viagem
  )

pof_uber$UF <- # Recodifica nome dos estados
  dplyr::recode(pof_uber$UF,
                "11" =  "RO",
                "12" =  "AC",
                "13" =  "AM",
                "14" =  "RR",
                "15" =  "PA",
                "16" =  "AP",
                "17" =  "TO",
                "21" =  "MA",
                "22" =  "PI",
                "23" =  "CE",
                "24" =  "RN",
                "25" =  "PB",
                "26" =  "PE",
                "27" =  "AL",
                "28" =  "SE",
                "29" =  "BA",
                "31" =  "MG",
                "32" =  "ES",
                "33" =  "RJ",
                "35" =  "SP",
                "41" =  "PR",
                "42" =  "SC",
                "43" =  "RS",
                "50" =  "MS",
                "51" =  "MT",
                "52" =  "GO",
                "53" =  "DF"
  )

write.csv(pof_uber, 'pof_uber.csv')


