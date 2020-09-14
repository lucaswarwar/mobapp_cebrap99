### Setup

source("00_setup.R.R")

######## 1. Dowload and Clean data ----------------------------------------------------------

#####################
### POF 2017-2018 ###
#####################

setwd("~/POF/2017_2018")

# Despesa Individual ----------------

despesa_individual <- # Recuperando microdados de despesa
  readRDS("DESPESA_INDIVIDUAL.rds")

total_despesas_transporte <- 
  despesa_individual %>% # Seleciona Variáveis
  dplyr::select(           
    UF, ESTRATO_POF, COD_UPA, NUM_DOM,
    NUM_UC, COD_INFORMANTE, QUADRO,
    COD_ITEM = V9001, valor_despesa = V8000_DEFLA,
    FATOR_ANUALIZACAO, PESO_FINAL, RENDA_TOTAL
  ) %>%   # Cria ID para FAMÍLIA, DOMICÍLIO e INDIVÍDUO
  dplyr::mutate(
    ID_DOM = paste(COD_UPA, NUM_DOM, sep = ""),
    ID_FAMILIA = paste(COD_UPA, NUM_DOM, NUM_UC, sep = ""),
    ID_MORADOR = paste(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = "")
  )  %>% # Filtra Despesas de interesse 
  dplyr::filter(     
    QUADRO == "23"  # Transporte em geral
  ) %>% 
  dplyr::select(
    UF, ESTRATO_POF,COD_UPA, RENDA_TOTAL, QUADRO,
    COD_ITEM, PESO_FINAL, ID_DOM, ID_FAMILIA,
    ID_MORADOR, valor_despesa, FATOR_ANUALIZACAO
  )

# Moradores ------------------------------

moradores <- # Recupera microdados de características dos indivíduos
  readRDS("MORADOR.rds")

moradores <- # Seleciona variáveis
  moradores %>% # Calcula ID para o indivíduo
  dplyr::select(
    COD_UPA, NUM_DOM, NUM_UC,
    COD_INFORMANTE, IDADE = V0403,
    SEXO = V0404, COR = V0405
  ) %>%
  dplyr::mutate(
    ID_MORADOR = paste(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = "")
  )

total_despesas_transporte <- # Seleciona variáveis
  moradores %>% # Join com df de despesas com transporte
  dplyr::select(
    IDADE, SEXO,
    COR, ID_MORADOR,
  ) %>%
  dplyr::right_join(total_despesas_transporte, by = "ID_MORADOR")

# Domicílio ------------------------------

domicilio <- # Recupera microdados de características dos indivíduos
  readRDS("DOMICILIO.rds")

domicilio <- # Seleciona variáveis
  domicilio %>% # Calcula ID para o indivíduo
  dplyr::select(
    COD_UPA, NUM_DOM, tipo_dom = V0201
  ) %>%
  dplyr::mutate(
    ID_DOM = paste(COD_UPA, NUM_DOM, sep = "")
  )

total_despesas_transporte <- # Seleciona variáveis
  domicilio %>% 
  dplyr::select(
    tipo_dom, ID_DOM
  ) %>%
  dplyr::right_join(total_despesas_transporte, by = "ID_DOM")

# Aluguel Estimado ------------------------------

aluguel_estimado <- # Recupera microdados de características dos indivíduos
  readRDS("ALUGUEL_ESTIMADO.rds")

aluguel_estimado <-
  aluguel_estimado %>% 
  dplyr::select(             
    COD_UPA, NUM_DOM,
    NUM_UC, N_MESES = V9011,
    VALOR_DEFLA = V8000_DEFLA,
    FATOR_ANUALIZACAO,
  ) %>%
  dplyr::mutate(
    ID_FAMILIA = paste(COD_UPA, NUM_DOM, NUM_UC, sep = ""),
    aluguel_anual = 
      ifelse(is.na(N_MESES), 
      VALOR_DEFLA * FATOR_ANUALIZACAO,
      VALOR_DEFLA * N_MESES * FATOR_ANUALIZACAO)
  )

total_despesas_transporte <- # Seleciona variáveis
  aluguel_estimado %>% 
  dplyr::select(
    aluguel_anual, ID_FAMILIA
  ) %>%
  dplyr::right_join(total_despesas_transporte, by = "ID_FAMILIA")

######## 2. Recode data -------------------------------------------------------------------

total_despesas_transporte$UF <- # Recodifica nome dos estados
  dplyr::recode(total_despesas_transporte$UF,
    "11" =  "RO", "12" =  "AC", "13" =  "AM",
    "14" =  "RR", "15" =  "PA", "16" =  "AP",
    "17" =  "TO", "21" =  "MA", "22" =  "PI",
    "23" =  "CE", "24" =  "RN", "25" =  "PB",
    "26" =  "PE", "27" =  "AL", "28" =  "SE",
    "29" =  "BA", "31" =  "MG", "32" =  "ES",
    "33" =  "RJ", "35" =  "SP", "41" =  "PR",
    "42" =  "SC", "43" =  "RS", "50" =  "MS",
    "51" =  "MT", "52" =  "GO", "53" =  "DF"
  )

total_despesas_transporte <- # Calcula renda per capita familiar
  total_despesas_transporte %>% 
  dplyr::group_by(ID_FAMILIA) %>% 
  dplyr::mutate(renda_pc = RENDA_TOTAL / n_distinct(ID_MORADOR)) %>% 
  dplyr::ungroup()

total_despesas_transporte <-
  total_despesas_transporte %>% # Cria classes para os Estratos Urbano, Rural, RM e interior
  dplyr::mutate(
    ESTRATO_POF = as.numeric(ESTRATO_POF),
    Estrato = dplyr::case_when(
    ESTRATO_POF == 1101 | ESTRATO_POF == 1102 | ESTRATO_POF == 1201 |
    ESTRATO_POF >= 1301 & ESTRATO_POF <= 1306 | ESTRATO_POF == 1401 & ESTRATO_POF == 1402 |
    ESTRATO_POF >= 1501 & ESTRATO_POF <= 1503 | ESTRATO_POF >= 1601 & ESTRATO_POF <= 1602 |
    ESTRATO_POF == 1701 | ESTRATO_POF >= 2101 & ESTRATO_POF <= 2103 |
    ESTRATO_POF >= 2201 & ESTRATO_POF <= 2203 | ESTRATO_POF >= 2301 & ESTRATO_POF <= 2306 |
    ESTRATO_POF >= 2401 & ESTRATO_POF <= 2402 | ESTRATO_POF >= 2501 & ESTRATO_POF <= 2503 |
    ESTRATO_POF >= 2601 & ESTRATO_POF <= 2603 | ESTRATO_POF >= 2701 & ESTRATO_POF <= 2703 |
    ESTRATO_POF >= 2801 & ESTRATO_POF <= 2802 | ESTRATO_POF >= 2901 & ESTRATO_POF <= 2906 |
    ESTRATO_POF >= 3101 & ESTRATO_POF <= 3106 | ESTRATO_POF >= 3201 & ESTRATO_POF <= 3202 |
    ESTRATO_POF >= 3301 & ESTRATO_POF <= 3309 | ESTRATO_POF >= 3501 & ESTRATO_POF <= 3509 |
    ESTRATO_POF >= 4101 & ESTRATO_POF <= 4105 | ESTRATO_POF >= 4201 & ESTRATO_POF <= 4202 |
    ESTRATO_POF >= 4301 & ESTRATO_POF <= 4306 | ESTRATO_POF >= 5001 & ESTRATO_POF <= 5003 |
    ESTRATO_POF >= 5101 & ESTRATO_POF <= 5102 | ESTRATO_POF >= 5201 & ESTRATO_POF <= 5203 |
    ESTRATO_POF >= 5301 & ESTRATO_POF <= 5306 ~ "Capital",
    ESTRATO_POF == 1307 | ESTRATO_POF >= 1504 & ESTRATO_POF <= 1505 | ESTRATO_POF == 1603 |
    ESTRATO_POF == 2104 | ESTRATO_POF >= 2307 & ESTRATO_POF <= 2309 | ESTRATO_POF == 2403 |
    ESTRATO_POF >= 2504 & ESTRATO_POF <= 2505 | ESTRATO_POF >= 2604 & ESTRATO_POF <= 2606 |
    ESTRATO_POF == 2704 | ESTRATO_POF == 2803 |
    ESTRATO_POF >= 2907 & ESTRATO_POF <= 2909 | ESTRATO_POF >= 3107 & ESTRATO_POF <= 3109 |
    ESTRATO_POF >= 3203 & ESTRATO_POF <= 3205 | ESTRATO_POF >= 3310 & ESTRATO_POF <= 3318 |
    ESTRATO_POF >= 3510 & ESTRATO_POF <= 3515 | ESTRATO_POF >= 4106 & ESTRATO_POF <= 4108 |
    ESTRATO_POF >= 4203 & ESTRATO_POF <= 4204 | ESTRATO_POF >= 4307 & ESTRATO_POF <= 4309 |
    ESTRATO_POF == 5103 | ESTRATO_POF >= 5204 & ESTRATO_POF <= 5206 ~ 'RM da Capital',
    ESTRATO_POF == 1103 | ESTRATO_POF == 1107 | ESTRATO_POF == 1202 |
    ESTRATO_POF >= 1308 & ESTRATO_POF <= 1310 | ESTRATO_POF == 1403 |
    ESTRATO_POF >= 1506 & ESTRATO_POF <= 1511 | ESTRATO_POF == 1604 |
    ESTRATO_POF >= 1702 & ESTRATO_POF <= 1705 | ESTRATO_POF >= 2105 & ESTRATO_POF <= 2113 |
    ESTRATO_POF >= 2204 & ESTRATO_POF <= 2209 | ESTRATO_POF >= 2310 & ESTRATO_POF <= 2320 |
    ESTRATO_POF >= 2404 & ESTRATO_POF <= 2408 | ESTRATO_POF >= 2506 & ESTRATO_POF <= 2511 |
    ESTRATO_POF >= 2607 & ESTRATO_POF <= 2615 | ESTRATO_POF >= 2705 & ESTRATO_POF <= 2708 |
    ESTRATO_POF >= 2804 & ESTRATO_POF <= 2806 | ESTRATO_POF >= 2910 & ESTRATO_POF <= 2925 |
    ESTRATO_POF >= 3110 & ESTRATO_POF <= 3130 | ESTRATO_POF >= 3206 & ESTRATO_POF <= 3211 |
    ESTRATO_POF >= 3319 & ESTRATO_POF <= 3330 | ESTRATO_POF >= 3516 & ESTRATO_POF <= 3536 |
    ESTRATO_POF >= 4109 & ESTRATO_POF <= 4124 | ESTRATO_POF >= 4205 & ESTRATO_POF <= 4217 |
    ESTRATO_POF >= 4310 & ESTRATO_POF <= 4324 | ESTRATO_POF >= 5004 & ESTRATO_POF <= 5009 |
    ESTRATO_POF >= 5104 & ESTRATO_POF <= 5112 | 
    ESTRATO_POF >= 5207 & ESTRATO_POF <= 5217 ~ "Interior Urbano",
    TRUE                                      ~ "Interior Rural")
  )

total_despesas_transporte <- # Recodifica variáveis com informações dos indivíduos
  total_despesas_transporte %>%
  dplyr::mutate(
    faixa_etaria = dplyr::case_when(
      IDADE < 15       ~ "0-14",
      IDADE %in% 15:24 ~ "15-24",
      IDADE %in% 25:34 ~ "25-34", 
      IDADE %in% 35:49 ~ "35-49", 
      IDADE %in% 50:64 ~ "50-64", 
      IDADE > 64 ~ "65+"),
    sexo = ifelse(SEXO == 1, "Homem", "Mulher"),
    cor = dplyr::case_when(
      COR == 1 ~ "Branca",
      COR == 2 ~ "Preta",
      COR == 4 ~ "Parda",
      TRUE     ~ "Amarela, Indígena ou outra"),
    casa = dplyr::case_when(
      tipo_dom == 1 ~ 'Casa',
      tipo_dom == 2 ~ 'Apartamento',
      TRUE          ~ 'Habitação Irregular'),
    RM = dplyr::case_when( 
      Estrato == "Interior Rural" ~ "Zona Rural",
      Estrato != "Interior Urbano" & UF == "SP" ~ "São Paulo",
      Estrato != "Interior Urbano" & UF == "RJ" ~ "Rio de Janeiro",
      Estrato != "Interior Urbano" & UF == "PR" ~ "Curitiba",
      Estrato != "Interior Urbano" & UF == "RS" ~ "Porto Alegre",
      Estrato != "Interior Urbano" & UF == "BA" ~ "Salvador",
      Estrato != "Interior Urbano" & UF == "PE" ~ "Recife",
      Estrato != "Interior Urbano" & UF == "CE" ~ "Fortaleza",
      Estrato != "Interior Urbano" & UF == "PA" ~ "Belém",
      Estrato != "Interior Urbano" & UF == "MG" ~ "Belo Horizonte",
      Estrato != "Interior Urbano" & UF == "DF" ~ "Brasília",
      Estrato != "Interior Urbano" & UF == "SC" ~ "Florianópolis",
      Estrato != "Interior Urbano" & UF == "ES" ~ "Vitória",
      Estrato != "Interior Urbano" & UF == "MT" ~ "Cuiabá",
      Estrato != "Interior Urbano" & UF == "MS" ~ "Campo Grande",
      Estrato != "Interior Urbano" & UF == "GO" ~ "Goiânia",
      Estrato != "Interior Urbano" & UF == "AM" ~ "Manaus",
      Estrato != "Interior Urbano" & UF == "MA" ~ "São Luís",
      Estrato != "Interior Urbano" & UF == "AL" ~ "Maceió",
      TRUE                                      ~ "Brasil Urbano")
  )

total_despesas_transporte <-
  total_despesas_transporte %>% 
  mutate(
    Modo = dplyr::case_when(
      COD_ITEM <= 2300401 | COD_ITEM >= 2300701 & COD_ITEM <= 2300901 |
      COD_ITEM >= 2301101 & COD_ITEM <= 2301301 | 
      COD_ITEM >= 2302301 & COD_ITEM <= 2303001 | 
      COD_ITEM >= 4100201 & COD_ITEM <= 4100501 | 
      COD_ITEM >= 4100901 & COD_ITEM <= 4101001 ~ "Transporte Público",
      COD_ITEM >= 2300401 & COD_ITEM <= 2300404 |
      COD_ITEM >= 4100601 & COD_ITEM <= 4100605 ~ 'Transporte Alternativo',
      COD_ITEM >= 2300501 & COD_ITEM <= 2300602 |
      COD_ITEM >= 4100701 & COD_ITEM <= 4100801 ~ "Táxi",
      COD_ITEM == 2303101 | COD_ITEM == 2303102 |
      COD_ITEM == 4106401 ~ "Ride-hailing", 
      TRUE                ~ "Transporte Privado")
  )

total_despesas_transporte <- total_despesas_transporte %>% as.data.table()

total_despesas_transporte[, # Calcula decis de renda
 decil_renda := cut(x = renda_pc, breaks = Hmisc::wtd.quantile(
 x = renda_pc, weights = PESO_FINAL, probs = 0:10/10,
 type = c('quantile','(i-1)/(n-1)', 'i/(n+1)','i/n'),
 normwt = F, na.rm = T), labels = F, include.lowest = T),
 by = RM]

total_despesas_transporte[, 
  quintil_renda := cut( x = renda_pc, breaks = Hmisc::wtd.quantile(
  x = renda_pc, weights = PESO_FINAL, probs = 0:5/5,
  type = c('quantile','(i-1)/(n-1)', 'i/(n+1)','i/n'),
  normwt = F, na.rm = T), labels = F, include.lowest = T),
  by = RM]

pof_mobapp <- # filtra e seleciona variáveis
  total_despesas_transporte %>%
  dplyr::select(
  PESO_FINAL, ID_MORADOR, ID_FAMILIA,
  COD_UPA, ESTRATO_POF,
  casa, aluguel_anual, RENDA_TOTAL,
  decil_renda, quintil_renda, RM,
  valor_despesa, FATOR_ANUALIZACAO,
  renda_pc, faixa_etaria, sexo, cor, 
  UF, Estrato, Modo)

setwd("C:/Users/lucas/Desktop/R/mobapp_cebrap99/01_prepare_data")

readr::write_rds(pof_mobapp, 'pof_mobapp.rds')

# Final dataset -----------------------

mobapp_individuo <-
  pof_mobapp %>% 
  dplyr::group_by(
    ID_MORADOR, PESO_FINAL,  COD_UPA, ESTRATO_POF, renda_pc,
    Modo, faixa_etaria, sexo, cor, UF, Estrato, RM,
    decil_renda, quintil_renda, casa, aluguel_anual
  ) %>% 
  dplyr::summarise(
    gasto_avg = mean(valor_despesa),
    gasto_mensal = sum(valor_despesa*FATOR_ANUALIZACAO)/12,
    despesas_mes = sum(FATOR_ANUALIZACAO)/12
  )

readr::write_rds(mobapp_individuo, 'mobapp_individuo.rds')
