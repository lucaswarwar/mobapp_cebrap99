### Setup ###

source("00_setup.R.R")

### Recover dataset ###

odds_ratio <- readr::read_csv("03_regress_data/summary_clean/odds_ratio_clean.csv")
mfx <- readr::read_csv("03_regress_data/summary_clean/marginal_effects_clean.csv")

# Plot 1: odds ratio idade, sexo, cor, estrato ----------------

# Idade -------------

plot_idade <-
  odds_ratio %>% 
  filter(variavel == 'faixa_etaria') %>% 
  ggplot(aes(odds_ratio, valor)) +
  geom_vline(aes(xintercept = 1), linetype = 'dashed') +
  geom_linerange(aes(xmin = odds_ratio - 1.96*std_err, xmax = odds_ratio + 1.96*std_err,color=odds_ratio),size=1.1,linetype='dotted') +
  geom_linerange(aes(xmin = odds_ratio - 1.64*std_err, xmax = odds_ratio + 1.64*std_err,color=odds_ratio),size=1.1) +  geom_point(aes(fill=odds_ratio),shape = 21, size = 4.5) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,trans = 'log') +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,trans = 'log') +
  labs(x = '', y = "", title = "Faixa Etária") +
  theme_bw() +
  theme(
    legend.position = 'none',
    plot.title = element_text(hjust = 0),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)))

# Sexo -------------

plot_sexo <-
  odds_ratio %>% 
  filter(variavel == 'sexo') %>% 
  ggplot(aes(odds_ratio, valor)) +
  geom_vline(aes(xintercept = 1), linetype = 'dashed') +
  geom_linerange(aes(xmin = odds_ratio - 1.96*std_err, xmax = odds_ratio + 1.96*std_err,color=odds_ratio),size=1.1,linetype='dotted') +
  geom_linerange(aes(xmin = odds_ratio - 1.64*std_err, xmax = odds_ratio + 1.64*std_err,color=odds_ratio),size=1.1) +  geom_point(aes(fill=odds_ratio),shape = 21, size = 4.5) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,trans = 'log') +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,trans = 'log') +
  labs(x = '', y = "", title = "Sexo") +
  theme_bw() +
  theme(
    legend.position = 'none',
    plot.title = element_text(hjust = 0),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)))

# Cor -------------

plot_cor <-
  odds_ratio %>% 
  filter(variavel == 'cor') %>% 
  ggplot(aes(odds_ratio, reorder(valor,odds_ratio))) +
  geom_vline(aes(xintercept = 1), linetype = 'dashed') +
  geom_linerange(aes(xmin = odds_ratio - 1.96*std_err, xmax = odds_ratio + 1.96*std_err,color=odds_ratio),size=1.1,linetype='dotted') +
  geom_linerange(aes(xmin = odds_ratio - 1.64*std_err, xmax = odds_ratio + 1.64*std_err,color=odds_ratio),size=1.1) +
  geom_point(aes(fill=odds_ratio),shape = 21, size = 4.5) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,trans = 'log') +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,trans = 'log') +
  labs(x = '', y = "", title = "Cor") +
  theme_bw() +
  theme(
    legend.position = 'none',
    plot.title = element_text(hjust = 0),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)))

# Estrato -------------

plot_estrato <-
  odds_ratio %>% 
  filter(variavel == 'Estrato') %>% 
  ggplot(aes(odds_ratio, reorder(valor,odds_ratio))) +
  geom_vline(aes(xintercept = 1), linetype = 'dashed') +
  geom_linerange(aes(xmin = odds_ratio - 1.96*std_err, xmax = odds_ratio + 1.96*std_err,color=odds_ratio),size=1.1,linetype='dotted') +
  geom_linerange(aes(xmin = odds_ratio - 1.64*std_err, xmax = odds_ratio + 1.64*std_err,color=odds_ratio),size=1.1) +
  geom_point(aes(fill=odds_ratio),shape = 21, size = 4.5) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,trans = 'log') +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,trans = 'log') +
  labs(x = '', y = "", title = "Estrato") +
  theme_bw() +
  theme(
    legend.position = 'none',
    plot.title = element_text(hjust = 0),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)))


p1 <- (plot_idade | plot_cor)/(plot_sexo|plot_estrato)

p1

ggsave('plot_odds_1.png', path = '03_regress_data/img/')


# Plot 2: odds ratio renda e rm -------------


# renda ----------------

plot_renda <-
  odds_ratio %>% 
  filter(variavel == 'decil_renda') %>% 
  mutate(valor = factor(valor,  levels = c("1",'2','3','4','5','6','7','8','9','10'))) %>% 
  ggplot(aes(odds_ratio, valor)) +
  geom_vline(aes(xintercept = 1), linetype = 'dashed') +
  geom_linerange(aes(xmin = odds_ratio - 1.96*std_err, xmax = odds_ratio + 1.96*std_err,color=odds_ratio),size=1.1,linetype='dotted') +
  geom_linerange(aes(xmin = odds_ratio - 1.64*std_err, xmax = odds_ratio + 1.64*std_err,color=odds_ratio),size=1.1) +
  geom_point(aes(fill=odds_ratio),shape = 21, size = 4.5) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,trans = 'log') +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,trans = 'log') +
  labs(x = '', y = "", title = "Decis de Renda") +
  theme_minimal() +
  theme(
    legend.position = 'none',
    plot.title = element_text(hjust = 0),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)))


# rm ----------------

plot_rm <-
  odds_ratio %>% 
  filter(variavel == 'RM') %>% 
  ggplot(aes(odds_ratio, reorder(valor,odds_ratio))) +
  geom_vline(aes(xintercept = 1), linetype = 'dashed') +
  geom_linerange(aes(xmin = odds_ratio - 1.96*std_err, xmax = odds_ratio + 1.96*std_err,color=odds_ratio),size=1.1,linetype='dotted') +
  geom_linerange(aes(xmin = odds_ratio - 1.64*std_err, xmax = odds_ratio + 1.64*std_err,color=odds_ratio),size=1.1) +
  geom_point(aes(fill=odds_ratio),shape = 21, size = 4.5) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,trans = 'log') +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1,trans = 'log') +
  labs(x = '', y = "", title = "Região Metropolitana") +
  theme_minimal() +
  theme(
    legend.position = 'none',
    plot.title = element_text(hjust = 0),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)))

p2 <- plot_renda | plot_rm

p2

ggsave('plot_odds_2.png', path = '03_regress_data/img/')
