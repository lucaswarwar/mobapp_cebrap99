### Setup ###

source("00_setup.R.R")

### Recover dataset #####################################################

pof_data <- readr::read_rds("01_prepare_data/mobapp_individuo.rds")

### Prepare dataset for regression #####################################

# Create dummy outcome variable ---------

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
  filter(faixa_etaria!='0-14') %>% 
  filter(casa != 'Habitação Irregular') %>% 
  #filter(cor!='Amarela, Indígena ou outra') %>% 
  filter(Estrato!='Interior Rural')

# Add reference groups ---------

pof_model <-
  pof_model %>% 
  mutate(
    inter = paste(casa,Estrato,sep = '_'),
    faixa_etaria = ifelse(faixa_etaria == "25-34","0_25-34",faixa_etaria),
    #quintil_renda = ifelse(decil_renda == '1', "0_1",quintil_renda),
    cor = ifelse(cor == 'Branca', "0_Branca", cor),
    RM = ifelse(
      RM == 'Maceió' | RM == 'Vitória' | RM == 'Florianópolis' | RM == 'São Luís',
      'Brasil Urbano', RM))

# Recode city names
unique(pof_model$RM)

#### Create survey design ###############################################

options( survey.lonely.psu = "adjust" )  

# Recover post-estratification  and join with df ---------

post_pop <- data.table::fread('df/post_strat.csv')

pof_model <-
  post_pop %>% 
  select(pos_estrato, tot_pop,COD_UPA) %>% 
  right_join(pof_model, by = 'COD_UPA')

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

# Subset survey design by sex

pof_design_pos_men <- srvyr::as_survey_design(pof_design_pos)%>% filter(sexo=="Homem")
pof_design_pos_wom <- srvyr::as_survey_design(pof_design_pos)%>% filter(sexo=="Mulher")

#### Regression functions  --------------------------

my_age_reg <- function( y, sexo){
  
  message(paste('working on, ', sexo, y))
  # y = 'ride-hailing' # sex = 'Homem'
  
  # select survey design for men or women
  if(sexo=='Homem'){temp_design <- pof_design_pos_men}
  if(sexo=='Mulher'){temp_design <- pof_design_pos_wom}
  
  # regression specification
  specification <- paste0( y, "  ~ faixa_etaria + as.factor(quintil_renda)")
  
  # SURVEY glm model
  reg1 <- svyglm(
    formula= as.formula(specification),
    design= temp_design,
    family= binomial,
    na.action = 'na.omit'
  )
  
  reg_summary <- summary(reg1)
  
  # create output table
  output = data.table( 
    transport = y,
    sex = sexo,
    groups = rownames(reg_summary$coefficients),
    coefficients= reg_summary$coefficients[,1],
    int2.5= confint(reg1, level = 0.95)[,1],
    int97.5= confint(reg1, level = 0.95)[,2]
  )
  
  # edit output table
  output <- output[2:5,]
  rownames(output) <- 1:nrow(output)
  output$groups <- c('15-24', '35-49', '50-64', '65+')
  
  # exponentiate to get odds ratio
  cols = names(output)[4:6]
  output[ , (cols) := lapply(.SD, exp), .SDcols = cols]
  setnames(output, 'coefficients', 'oddsratio')
  
  return(output)
}


#### Apply Regression functions  --------------------------

all_combinations <- purrr::cross2(
  .x=c('ride_hailing', 'transporte_pub', 'transporte_ind', 'taxi'), 
  .y=c('Homem', 'Mulher')) %>% rbindlist()

# age
df_age <- purrr::map2(.x= all_combinations$V1, .y= all_combinations$V2, .f=my_age_reg)
df_age <- rbindlist(df_age)

# race
df_race <- purrr::map2(.x= all_combinations$V1, .y= all_combinations$V2, .f=my_race_reg)
df_race <- rbindlist(df_race)

# city
df_city <- purrr::map2(.x= all_combinations$V1, .y= all_combinations$V2, .f=my_city_reg)
df_city <- rbindlist(df_city)

# strata + house
df_strata_house <- purrr::map2(.x= all_combinations$V1, .y= all_combinations$V2, .f=my_strata_house_reg)
df_strata_house <- rbindlist(df_strata_house)
