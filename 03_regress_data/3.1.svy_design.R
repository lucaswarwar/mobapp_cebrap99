### Setup ###

source("00_setup.R.R")

### Recover dataset #####################################################

pof_data <- readr::read_rds("01_prepare_data/mobapp_individuo.rds")

### Prepare dataset for regression #####################################

# Create dummy outcome variables ---------

pof_model <-
  pof_data %>% 
  dplyr::mutate(
    ride_hailing = ifelse(Modo=='Ride-hailing',1,0),
    transporte_pub = ifelse(Modo=='Transporte Público',1,0),
    transporte_ind = ifelse(Modo=='Transporte Privado',1,0),
    taxi = ifelse(Modo=='Táxi',1,0)) 

#  Focus analysis on urban, adult people ---------

pof_model <-
  pof_model %>% 
  na.omit() %>% 
  dplyr::filter(faixa_etaria!='0-14') %>% 
  dplyr::filter(casa != 'Habitação Irregular') %>% 
  dplyr::filter(cor!='Amarela, Indígena ou outra') %>% 
  dplyr::filter(Estrato!='Interior Rural')

# Add reference groups and recode variables ---------

pof_model <-
  pof_model %>% 
  dplyr::mutate(
    strata = paste(casa,Estrato,sep = '_'),
    sex_race = paste(sexo,cor,sep = "_"),
    faixa_etaria = ifelse(faixa_etaria == "15-24","0_15-24",faixa_etaria),
    cor = ifelse(cor == 'Branca', "0_Branca", cor),
    RM = ifelse(
      RM == 'Maceió' | RM == 'Vitória' | RM == 'Florianópolis' | RM == 'São Luís',
      'Brasil Urbano', RM),
    RM = ifelse(RM == 'Brasil Urbano', '0_POA',RM))

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