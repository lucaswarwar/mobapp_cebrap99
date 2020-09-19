### Setup ###

source("setup.R")

### Recover dataset #####################################################

pof_design_pos <- readr::read_rds("03_regress_data/pof_design_pof.rds")

#### Regression functions  --------------------------

# Age regression -------------

my_age_reg <- function(y){
  
  message(paste('working on, ', y))
  # y = 'ride-hailing' 
  
  # select survey design 
  temp_design <- pof_design_pos # here I can work on subsets of the survey design (gender, city...)
  
  # regression specification
  specification <- paste0( y, "  ~ faixa_etaria + as.factor(decil_renda)") # keep income fixed
  
  # survey glm model
  reg1 <- survey::svyglm(                     
    formula= as.formula(specification),
    design= temp_design,
    family= binomial,
    na.action = 'na.omit')
  
  # create table with results
  output <- broom::tidy(reg1)
  output <- output %>% 
    dplyr::filter((grepl('faixa_etaria',term))==T) %>% 
    dplyr::mutate(
      group = gsub('faixa_etaria','',term),
      transport = y,
      odds_ratio = exp(estimate),
      int2.5= exp(confint(reg1, level = 0.95)[2:5,1]),
      int97.5=exp(confint(reg1, level = 0.95)[2:5,2]),
      p_value = p.value) %>% 
    dplyr::select(
      group,transport,odds_ratio,int2.5,int97.5,p_value)
  
  return(output)
}

# Age x Sex regression --------------

my_sex_age_reg <- function(y){
  
  message(paste('working on, ', y))
  # y = 'ride-hailing' 
  
  # select survey design 
  temp_design <- pof_design_pos # here I can work on subsets of the survey design (gender, city...)
  
  # regression specification
  specification <- paste0( y, "  ~ sex_age + as.factor(decil_renda)") # keep income fixed
  
  # survey glm model
  reg1 <- survey::svyglm(                     
    formula= as.formula(specification),
    design= temp_design,
    family= binomial,
    na.action = 'na.omit')
  
  # create table with results
  output <- broom::tidy(reg1)
  output <- output %>% 
    dplyr::filter((grepl('sex_age',term))==T) %>% 
    dplyr::mutate(
      group = gsub('sex_age','',term),
      transport = y,
      odds_ratio = exp(estimate),
      int2.5= exp(confint(reg1, level = 0.95)[2:10,1]),
      int97.5=exp(confint(reg1, level = 0.95)[2:10,2]),
      p_value = p.value) %>% 
    dplyr::select(group,transport,odds_ratio,int2.5,int97.5,p_value)
  
  return(output)
}

# Race x Sex regression --------------

my_sex_race_reg <- function(y){
  
  message(paste('working on, ', y))
  # y = 'ride-hailing' 
  
  # select survey design 
  temp_design <- pof_design_pos # here I can work on subsets of the survey design (gender, city...)
  
  # regression specification
  specification <- paste0( y, "  ~ sex_race + as.factor(decil_renda)") # keep income fixed
  
  # survey glm model
  reg1 <- survey::svyglm(                     
    formula= as.formula(specification),
    design= temp_design,
    family= binomial,
    na.action = 'na.omit')
  
  # create table with results
  output <- broom::tidy(reg1)
  output <- output %>% 
    dplyr::filter((grepl('sex_race',term))==T) %>% 
    dplyr::mutate(
      group = gsub('sex_race','',term),
      transport = y,
      odds_ratio = exp(estimate),
      int2.5= exp(confint(reg1, level = 0.95)[2:6,1]),
      int97.5=exp(confint(reg1, level = 0.95)[2:6,2]),
      p_value = p.value) %>% 
    dplyr::select(group,transport,odds_ratio,int2.5,int97.5,p_value)
  
  return(output)
}

# strata regression --------------

my_strata_reg <- function(y){
  
  message(paste('working on, ', y))
  # y = 'ride-hailing' 
  
  # select survey design 
  temp_design <- pof_design_pos # here I can work on subsets of the survey design (gender, city...)
  
  # regression specification
  specification <- paste0( y, "  ~ strata + as.factor(decil_renda)") # keep income fixed
  
  # survey glm model
  reg1 <- survey::svyglm(                     
    formula= as.formula(specification),
    design= temp_design,
    family= binomial,
    na.action = 'na.omit')
  
  # create table with results
  output <- broom::tidy(reg1)
  output <- output %>% 
    dplyr::filter((grepl('strata',term))==T) %>% 
    dplyr::mutate(
      group = gsub('strata','',term),
      transport = y,
      odds_ratio = exp(estimate),
      int2.5= exp(confint(reg1, level = 0.95)[2:6,1]),
      int97.5=exp(confint(reg1, level = 0.95)[2:6,2]),
      p_value = p.value) %>% 
    dplyr::select(group,transport,odds_ratio,int2.5,int97.5,p_value)
  
  return(output)
}

# city regression --------------

my_city_reg <- function(y){
  
  message(paste('working on, ', y))
  # y = 'ride-hailing' 
  
  # select survey design 
  temp_design <- pof_design_pos # here I can work on subsets of the survey design (gender, city...)
  
  # regression specification
  specification <- paste0( y, "  ~ RM + as.factor(decil_renda)") # keep income fixed
  
  # survey glm model
  reg1 <- survey::svyglm(                     
    formula= as.formula(specification),
    design= temp_design,
    family= binomial,
    na.action = 'na.omit')
  
  # create table with results
  output <- broom::tidy(reg1)
  output <- output %>% 
    dplyr::filter((grepl('RM',term))==T) %>% 
    dplyr::mutate(
      group = gsub('RM','',term),
      transport = y,
      odds_ratio = exp(estimate),
      int2.5= exp(confint(reg1, level = 0.95)[2:15,1]),
      int97.5=exp(confint(reg1, level = 0.95)[2:15,2]),
      p_value = p.value) %>% 
    dplyr::select(group,transport,odds_ratio,int2.5,int97.5,p_value)
  
  return(output)
}

#### Apply Regression functions  --------------------------

transport <- list('ride_hailing', 'transporte_pub', 'transporte_ind', 'taxi')

# age 
df_age <- purrr::map(.x= transport, .f=my_age_reg) %>% rbindlist()
write.csv(df_age,'03_regress_data/results/df_age.csv')

# age + sex 
df_sex_age <- purrr::map(.x= transport, .f=my_sex_age_reg) %>% rbindlist()
write.csv(df_sex_age,'03_regress_data/results/df_sex_age.csv')

# sex + race
df_sex_race <- purrr::map(.x= transport, .f=my_sex_race_reg) %>% rbindlist()
write.csv(df_sex_race,'03_regress_data/results/df_sex_race.csv')

# strata
df_strata <- purrr::map(.x= transport, .f=my_strata_reg) %>% rbindlist()
write.csv(df_strata,'03_regress_data/results/df_strata.csv')

# city
df_city <- purrr::map(.x= transport, .f=my_city_reg) %>% rbindlist()
write.csv(df_city,'03_regress_data/results/df_city.csv')


#################################################################