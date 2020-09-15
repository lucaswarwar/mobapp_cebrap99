data <- readr::read_rds('01_prepare_data/mobapp_individuo.rds')

logit_pof_clean <- 
  logit_pof_clean %>% 
  mutate(
    control = stringr::str_sub(ID_MORADOR, 1,9))


pof_design <- svydesign( # need to add strata 
  ids = logit_pof_clean$control,
  weights = logit_pof_clean$PESO_FINAL,
  data = logit_pof_clean)

model_svy_2 <- svyglm(
  formula = ride_hailing ~ faixa_etaria+decil_renda,
  design = pof_design_pos, family = 'binomial')

summary_svy2 <- broom::tidy(model_svy_2)
summary_svy <-
  summary_svy %>% 
  mutate(odds_ratio = exp(coef(model_svy))) #odds ratio

write.csv(summary_svy, '03_regress_data/summary/summary_svy.csv')
