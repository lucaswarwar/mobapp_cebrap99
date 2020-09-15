### Setup ###

source("00_setup.R.R")

### Recover dataset ###

pof_data <- readr::read_rds("01_prepare_data/pof_mobapp.rds")

### Prepare data for regression ###

# Create dummy outcome variable ---------

logit_pof <-
  pof_data %>% 
  dplyr::mutate(ride_hailing = ifelse(Modo=='Ride-hailing',1,0)) %>% 
  dplyr::group_by(ID_MORADOR) %>% 
  dplyr::mutate(dummy_app = ifelse(sum(ride_hailing)>=1,1,0)) %>% 
  dplyr::group_by(
    ID_MORADOR,PESO_FINAL, renda_pc, faixa_etaria,UF,Estrato,
    sexo,cor,RM,casa,quintil_renda, decil_renda) %>% 
  dplyr::summarise(dummy_app = max(dummy_app))

#  Remove outliers ---------

logit_pof_clean <-
  logit_pof %>% 
  na.omit() %>% 
  filter(faixa_etaria!='0-14') %>% 
  filter(casa != 'Habitação Irregular') %>% 
  filter(cor!='Amarela, Indígena ou outra') %>% 
  filter(Estrato!='Interior Rural') %>% 
  mutate(RM=factor(RM))

logit_pof_clean$decil_renda <- factor(
  logit_pof_clean$decil_renda,
  levels = c("1",'2','3','4','5','6','7','8','9','10'))

# Run logit model ------------------------------------------

logit_model <-
  glm(
    dummy_app ~ faixa_etaria+sexo+cor+casa+Estrato+decil_renda+RM, 
    data = logit_pof_clean, family = 'binomial')

logit_summary <- 
  broom::tidy(logit_model) # df with model stats

write.csv(logit_summary, "03_regress_data/summary/summary.csv")

# Compute odds ratios -------------------------------------

odds_ratio <-
  mfx::logitor(
  dummy_app ~ faixa_etaria+sexo+cor+casa+Estrato+decil_renda+RM, 
  data = logit_pof_clean)

odds <- as.data.frame(odds_ratio$oddsratio)

write.csv(odds, "03_regress_data/summary/odds_ratio.csv")

# Compute marginal effects -----------------------------------

marginal_effects <-
  mfx::logitmfx(
    dummy_app ~ faixa_etaria+sexo+cor+casa+Estrato+decil_renda+RM, 
    data = logit_pof_clean)

mfx <- as.data.frame(marginal_effects$mfxest)

write.csv(mfx, "03_regress_data/summary/marginal_effects.csv")

########################################################################