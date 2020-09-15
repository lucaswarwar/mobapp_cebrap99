### Setup ###

source("00_setup.R.R")

### Recover dataset ###

mfx <- readr::read_csv("03_regress_data/summary_clean/mfx_clean.csv")
mfx <- readr::read_csv("03_regress_data/summary_clean/marginal_effects_clean.csv")

# Plot 1: odds ratio idade, sexo, cor, estrato ----------------

# Idade -------------

plot_idade <-
  mfx %>% 
  filter(variavel == 'faixa_etaria') %>% 
  ggplot(aes(mfx, valor)) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  geom_linerange(aes(xmin = mfx - 1.96*std_err, xmax = mfx + 1.96*std_err,color=mfx),size=1.1,linetype='dotted') +
  geom_linerange(aes(xmin = mfx - 1.64*std_err, xmax = mfx + 1.64*std_err,color=mfx),size=1.1) +  geom_point(aes(fill=mfx),shape = 21, size = 4.5) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,) +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1) +
  scale_x_continuous(labels = scales::percent)+
  labs(x = '', y = "", title = "Faixa Etária") +
  theme_bw() +
  theme(
    legend.position = 'none',
    plot.title = element_text(hjust = 0),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)))

# Sexo -------------

plot_sexo <-
  mfx %>% 
  filter(variavel == 'sexo') %>% 
  ggplot(aes(mfx, valor)) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  geom_linerange(aes(xmin = mfx - 1.96*std_err, xmax = mfx + 1.96*std_err,color=mfx),size=1.1,linetype='dotted') +
  geom_linerange(aes(xmin = mfx - 1.64*std_err, xmax = mfx + 1.64*std_err,color=mfx),size=1.1) +  geom_point(aes(fill=mfx),shape = 21, size = 4.5) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,) +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1) +
  scale_x_continuous(labels = scales::percent)+
  labs(x = '', y = "", title = "Sexo") +
  theme_bw() +
  theme(
    legend.position = 'none',
    plot.title = element_text(hjust = 0),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)))

# Cor -------------

plot_cor <-
  mfx %>% 
  filter(variavel == 'cor') %>% 
  ggplot(aes(mfx, reorder(valor,mfx))) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  geom_linerange(aes(xmin = mfx - 1.96*std_err, xmax = mfx + 1.96*std_err,color=mfx),size=1.1,linetype='dotted') +
  geom_linerange(aes(xmin = mfx - 1.64*std_err, xmax = mfx + 1.64*std_err,color=mfx),size=1.1) +  geom_point(aes(fill=mfx),shape = 21, size = 4.5) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,) +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1) +
  scale_x_continuous(labels = scales::percent)+
  labs(x = '', y = "", title = "Cor") +
  theme_bw() +
  theme(
    legend.position = 'none',
    plot.title = element_text(hjust = 0),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)))

# Estrato -------------

plot_estrato <-
  mfx %>% 
  filter(variavel == 'Estrato') %>% 
  ggplot(aes(mfx, reorder(valor,mfx))) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  geom_linerange(aes(xmin = mfx - 1.96*std_err, xmax = mfx + 1.96*std_err,color=mfx),size=1.1,linetype='dotted') +
  geom_linerange(aes(xmin = mfx - 1.64*std_err, xmax = mfx + 1.64*std_err,color=mfx),size=1.1) +  geom_point(aes(fill=mfx),shape = 21, size = 4.5) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1,) +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1) +
  scale_x_continuous(labels = scales::percent)+
  labs(x = '', y = "", title = "Estrato") +
  theme_bw() +
  theme(
    legend.position = 'none',
    plot.title = element_text(hjust = 0),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)))


p1 <- (plot_idade | plot_cor)/(plot_sexo|plot_estrato)

p1

ggsave('plot_mfx_1.png', path = '03_regress_data/img/')


# Plot 2: odds ratio renda e rm -------------


# renda ----------------

plot_renda <-
  mfx %>% 
  filter(variavel == 'decil_renda') %>% 
  mutate(valor = factor(valor,  levels = c("1",'2','3','4','5','6','7','8','9','10'))) %>% 
  ggplot(aes(mfx, valor)) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  geom_linerange(aes(xmin = mfx - 1.96*std_err, xmax = mfx + 1.96*std_err,color=mfx),size=1.1,linetype='dotted') +
  geom_linerange(aes(xmin = mfx - 1.64*std_err, xmax = mfx + 1.64*std_err,color=mfx),size=1.1) +
  geom_point(aes(fill=mfx),shape = 21, size = 4.5) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1) +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = '', y = "", title = "Decis de Renda") +
  theme_minimal() +
  theme(
    legend.position = 'none',
    plot.title = element_text(hjust = 0),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)))


# rm ----------------

plot_rm <-
  mfx %>% 
  filter(variavel == 'RM') %>% 
  ggplot(aes(mfx, reorder(valor,mfx))) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  geom_linerange(aes(xmin = mfx - 1.96*std_err, xmax = mfx + 1.96*std_err,color=mfx),size=1.1,linetype='dotted') +
  geom_linerange(aes(xmin = mfx - 1.64*std_err, xmax = mfx + 1.64*std_err,color=mfx),size=1.1) +
  geom_point(aes(fill=mfx),shape = 21, size = 4.5) +
  paletteer::scale_fill_paletteer_c(palette = 'scico::berlin',direction = -1) +
  paletteer::scale_color_paletteer_c(palette = 'scico::berlin',direction = -1) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = '', y = "", title = "Região Metropolitana") +
  theme_minimal() +
  theme(
    legend.position = 'none',
    plot.title = element_text(hjust = 0),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1)))

p2 <- plot_renda | plot_rm

p2

ggsave('plot_mfx_2.png', path = '03_regress_data/img/')
