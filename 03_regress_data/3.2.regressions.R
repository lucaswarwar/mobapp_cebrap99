### Setup ###

source("00_setup.R.R")

### Recover dataset #####################################################

pof_design_pos <- readr::read_rds("03_regress_data/pof_design_pof.rds")

#### Regression functions  --------------------------

my_age_reg <- function( y){
  
  message(paste('working on, ', y))
  # y = 'ride-hailing' 
  
  # select survey design 
  temp_design <- pof_design_pos # here I can work on subsets of the survey design (gender, city...)
  
  # regression specification
  specification <- paste0( y, "  ~ faixa_etaria + as.factor(decil_renda)") # keep income fixed
  
  # SURVEY glm model
  reg1 <- survey::svyglm(
    formula= as.formula(specification),
    design= temp_design,
    family= binomial,
    na.action = 'na.omit'
  )
  
  reg_summary <- summary(reg1)
  
  # create output table
  output = data.table( 
    transport = y,
    groups = rownames(reg_summary$coefficients),
    coefficients= reg_summary$coefficients[,1],
    int2.5= confint(reg1, level = 0.95)[,1],
    int97.5= confint(reg1, level = 0.95)[,2]
  )
  
  # edit output table
  output <- output[2:5,]
  rownames(output) <- 1:nrow(output)
  output$groups <- c('25-34', '35-49', '50-64', '65+')
  
  # exponentiate to get odds ratio
  cols = names(output)[3:5]
  output[ , (cols) := lapply(.SD, exp), .SDcols = cols]
  setnames(output, 'coefficients', 'oddsratio')
  
  return(output)
}

my_sex_race_reg <- function( y){
  
  message(paste('working on, ', y))
  # y = 'ride-hailing' 
  
  # select survey design for men or women
  temp_design <- pof_design_pos
  
  # regression specification
  specification <- paste0( y, "  ~ sex_race + as.factor(decil_renda)") # keep income fixed
  
  # SURVEY glm model
  reg1 <- survey::svyglm(
    formula= as.formula(specification),
    design= temp_design,
    family= binomial,
    na.action = 'na.omit'
  )
  
  reg_summary <- summary(reg1)
  
  # create output table
  output = data.table( 
    transport = y,
    groups = rownames(reg_summary$coefficients),
    coefficients= reg_summary$coefficients[,1],
    int2.5= confint(reg1, level = 0.95)[,1],
    int97.5= confint(reg1, level = 0.95)[,2]
  )
  
  # edit output table
  output <- output[2:6,]
  rownames(output) <- 1:nrow(output)
  output$groups <- c('Homem Pardo', 'Homem Preto', 'Mulher Branca', "Mulher Parda", 'Mulher Preta')
  
  # exponentiate to get odds ratio
  cols = names(output)[3:5]
  output[ , (cols) := lapply(.SD, exp), .SDcols = cols]
  setnames(output, 'coefficients', 'oddsratio')
  
  return(output)
}

my_strata_reg <- function( y){
  
  message(paste('working on, ', y))
  # y = 'ride-hailing' 
  
  # select survey design 
  temp_design <- pof_design_pos
  
  # regression specification
  specification <- paste0( y, "  ~ strata + as.factor(decil_renda)") # keep income fixed
  
  # SURVEY glm model
  reg1 <- survey::svyglm(
    formula= as.formula(specification),
    design= temp_design,
    family= binomial,
    na.action = 'na.omit'
  )
  
  reg_summary <- summary(reg1)
  
  # create output table
  output = data.table( 
    transport = y,
    groups = rownames(reg_summary$coefficients),
    coefficients= reg_summary$coefficients[,1],
    int2.5= confint(reg1, level = 0.95)[,1],
    int97.5= confint(reg1, level = 0.95)[,2]
  )
  
  # edit output table
  output <- output[2:6,]
  rownames(output) <- 1:nrow(output)
  output$groups <- c(
    'Apartamento - Interior', 'Apartamento - RM da Capital', 
    'Casa - Capital', 'Casa - Interior','Casa - RM da Capital')
  
  # exponentiate to get odds ratio
  cols = names(output)[3:5]
  output[ , (cols) := lapply(.SD, exp), .SDcols = cols]
  setnames(output, 'coefficients', 'oddsratio')
  
  return(output)
}

my_city_reg <- function( y ){
  
  message(paste('working on, ', y))
  # y = 'ride-hailing' 
  
  # select survey 
  temp_design <- pof_design_pos
  
  # regression specification
  specification <- paste0( y, "  ~ RM + as.factor(decil_renda)") # keep income fixed
  
  # SURVEY glm model
  reg1 <- survey::svyglm(
    formula= as.formula(specification),
    design= temp_design,
    family= binomial,
    na.action = 'na.omit'
  )
  
  reg_summary <- summary(reg1)
  
  # create output table
  output = data.table( 
    transport = y,
    groups = rownames(reg_summary$coefficients),
    coefficients= reg_summary$coefficients[,1],
    int2.5= confint(reg1, level = 0.95)[,1],
    int97.5= confint(reg1, level = 0.95)[,2]
  )
  
  # edit output table
  output <- output[2:15,]
  rownames(output) <- 1:nrow(output)
  output$groups <- c(
    "Belém",'Belo Horizonte',"Brasil Urbano", 'Brasília', 'Campo Grande', 
    'Cuiabá', 'Curitiba', 'Fortaleza','Goiânia','Manaus',
    'Recife','Rio de Janeiro','Salvador','São Paulo')
  
  # exponentiate to get odds ratio
  cols = names(output)[3:5]
  output[ , (cols) := lapply(.SD, exp), .SDcols = cols]
  setnames(output, 'coefficients', 'oddsratio')
  
  return(output)
}

my_city_reg2 <- function( y ){ #BR urbano as reference
  
  message(paste('working on, ', y))
  # y = 'ride-hailing' 
  
  # select survey 
  temp_design <- pof_design_pos
  
  # regression specification
  specification <- paste0( y, "  ~ RM + as.factor(decil_renda)") # keep income fixed
  
  # SURVEY glm model
  reg1 <- survey::svyglm(
    formula= as.formula(specification),
    design= temp_design,
    family= binomial,
    na.action = 'na.omit'
  )
  
  reg_summary <- summary(reg1)
  
  # create output table
  output = data.table( 
    transport = y,
    groups = rownames(reg_summary$coefficients),
    coefficients= reg_summary$coefficients[,1],
    int2.5= confint(reg1, level = 0.95)[,1],
    int97.5= confint(reg1, level = 0.95)[,2]
  )
  
  # edit output table
  output <- output[2:15,]
  rownames(output) <- 1:nrow(output)
  output$groups <- c(
    "Belém",'Belo Horizonte', 'Brasília', 'Campo Grande', 
    'Cuiabá', 'Curitiba', 'Fortaleza','Goiânia','Manaus',
    'Porto Alegre','Recife','Rio de Janeiro','Salvador','São Paulo')
  
  # exponentiate to get odds ratio
  cols = names(output)[3:5]
  output[ , (cols) := lapply(.SD, exp), .SDcols = cols]
  setnames(output, 'coefficients', 'oddsratio')
  
  return(output)
}

#### Apply Regression functions  --------------------------

transport <- list('ride_hailing', 'transporte_pub', 'transporte_ind', 'taxi')

# age 
df_age <- purrr::map(.x= transport, .f=my_age_reg)
df_age <- rbindlist(df_age)
write.csv(df_age,'03_regress_data/results/df_age.csv')

# sex + race
df_sex_race <- purrr::map(.x= transport, .f=my_sex_race_reg)
df_sex_race <- rbindlist(df_sex_race)
write.csv(df_sex_race,'03_regress_data/results/df_sex_race.csv')

# strata
df_strata <- purrr::map(.x= transport, .f=my_strata_reg)
df_strata <- rbindlist(df_strata)
write.csv(df_strata,'03_regress_data/results/df_strata.csv')

# city
df_city <- purrr::map(.x= transport, .f=my_city_reg)
df_city <- rbindlist(df_city)
write.csv(df_city,'03_regress_data/results/df_city.csv')

# city2
df_city2 <- purrr::map(.x= transport, .f=my_city_reg2)
df_city2 <- rbindlist(df_city2)
write.csv(df_city2,'03_regress_data/results/df_city2.csv')

#################################################################