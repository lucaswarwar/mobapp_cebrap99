### Setup ###

source("setup.R")

### Recover dataset #####################################################

pof_data <- readr::read_rds("00_prepare_data/mobapp_individuo.rds")

### Prepare dataset for regression #####################################

# Create dummy outcome variables ---------

pof_model <-
  pof_data %>% 
  dplyr::mutate(
    ride_hailing = ifelse(Modo=='Ride-hailing',1,0),
    transporte_pub = ifelse(Modo=='Transporte Público',1,0),
    transporte_ind = ifelse(Modo=='Transporte Privado',1,0),
    taxi = ifelse(Modo=='Táxi',1,0)
    #, frequencia = dplyr::case_when(
    #  despesas_mes <= 5 ~ 'Baixa',
    #  despesas_mes <= 9 ~ 'Média',
    #  TRUE ~ 'Alta')
    )

#  Focus analysis on urban, adult people ---------

pof_model <-
  pof_model %>% na.omit() %>% 
  dplyr::filter(faixa_etaria!='0-14') %>% 
  dplyr::filter(casa != 'Habitação Irregular') %>% 
  #dplyr::filter(cor!='Amarela, Indígena ou outra') %>% 
  dplyr::filter(Estrato!='Interior Rural')

# Consider only strata with ride-hailing availability --------------------

#pof_model %>% 
#  dplyr::filter(Modo=='Ride-hailing') %>% 
#  dplyr::group_by(Modo) %>% 
#  dplyr::summarise(n=n_distinct(ESTRATO_POF))
#
#ride_hailing <- 
#  pof_model %>% 
#  dplyr::filter(Modo=='Ride-hailing')
#
#estratos <- unique(ride_hailing$ESTRATO_POF)

#pof_model <-
#  pof_model %>% 
#  dplyr::filter(ESTRATO_POF %in% estratos)

# Add reference groups and recode variables ---------

pof_model <-
  pof_model %>% 
  dplyr::mutate(
    strata = paste(casa,Estrato,sep = '_'),
    sex_race = paste(sexo,cor,sep = "_"),
    sex_age = paste(sexo,faixa_etaria,sep = '_'),
    race_age = paste(cor,faixa_etaria,sep = "_"),
    sex_strata = paste(sexo,Estrato,sep = '_'),
    age_strata = paste(faixa_etaria,Estrato,sep = '_'),
    race_strata = paste(cor,Estrato,sep = '_'),
    RM = ifelse(
      RM == 'Maceió' | RM == 'Vitória' | RM == 'Florianópolis' | 
      RM == 'São Luís'| RM == 'Brasil Urbano', 'Brasil Urbano', 
      ifelse(RM=='São Paulo', '0_SP', RM)))

pof_model$decil_renda <-
  factor(
    pof_model$decil_renda,
    levels = c('1','2','3','4','5','6','7','8','9','10'))

#pof_model$frequencia <-
#  factor(
#    pof_model$frequencia,
#    levels = c('Baixa','Média','Alta'))

#### Create survey design ###############################################

options(survey.lonely.psu = "adjust")  

# Recover post-estratification  and join with df ---------

post_pop <- data.table::fread('df/post_strat.csv')

pof_model <-
  post_pop %>% 
  dplyr::select(pos_estrato, tot_pop,COD_UPA) %>% 
  dplyr::right_join(pof_model, by = 'COD_UPA')

# Create survey design ----------------------

pof_design <- survey::svydesign(
  data = pof_model,
  id = ~COD_UPA,
  weights = ~PESO_FINAL,
  strata = ~ESTRATO_POF,
  nest = TRUE)

# Recover post-estratification ---------

post_pop <- data.frame(
    pos_estrato = unique(pof_model$pos_estrato), 
    Freq = unique(pof_model$tot_pop))

pof_design_pos <- survey::postStratify(
  pof_design, ~pos_estrato, post_pop)

# Subset survey design by income -----------------------

#pof_design_pos_bot <- srvyr::as_survey_design(pof_design_pos)%>% dplyr::filter(quintil_renda=="1º Quintil")
#pof_design_pos_mid <- srvyr::as_survey_design(pof_design_pos)%>% dplyr::filter(quintil_renda=="3º Quintil")
#pof_design_pos_top <- srvyr::as_survey_design(pof_design_pos)%>% dplyr::filter(quintil_renda=="5º Quintil")

# Subset survey design by gender -----------------------

#pof_design_pos_men <- srvyr::as_survey_design(pof_design_pos)%>% dplyr::filter(sexo=='Homem')
#pof_design_pos_wom <- srvyr::as_survey_design(pof_design_pos)%>% dplyr::filter(sexo=="Mulher")


### Save results ---------

readr::write_rds(pof_design_pos, '03_regress_data/pof_design_pof.rds')