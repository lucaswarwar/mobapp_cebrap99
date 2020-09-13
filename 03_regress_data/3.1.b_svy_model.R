data <- readr::read_rds('01_prepare_data/mobapp_individuo.rds')

logit_pof_clean <- 
  logit_pof_clean %>% 
  mutate(
    control = stringr::str_sub(ID_MORADOR, 1,9))

library(survey)

pof_design <- svydesign( # need to add strata 
  ids = logit_pof_clean$control,
  weights = logit_pof_clean$PESO_FINAL,
  data = logit_pof_clean)

model_svy <- svyglm(
  formula = dummy_app ~ faixa_etaria+sexo+cor+casa+Estrato+decil_renda+RM, 
  design = pof_design, family = 'binomial')

summary_svy <- broom::tidy(model_svy)
summary_svy <-
  summary_svy %>% 
  mutate(odds_ratio = exp(coef(model_svy))) #odds ratio

write.csv(summary_svy, '03_regress_data/summary/summary_svy.csv')
