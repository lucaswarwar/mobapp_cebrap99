UF <- fread('UF.csv')

p1<-
  RM %>%  
  ggplot(aes(RM, share, group = Modo)) +
  geom_line(
    data = RM %>% filter(Modo != 'Aplicativo')%>% 
      filter(RM != 'Brasil Urbano') %>% 
      filter(RM != 'Zona Rural'),
    aes(RM, share, group = Modo, color = Modo),
    linetype = 'dashed', size = 1) +
  geom_point(
    data = RM %>% filter(Modo != 'Aplicativo')%>% 
      filter(RM != 'Brasil Urbano') %>% 
      filter(RM != 'Zona Rural'),
    aes(RM, share, fill = Modo), shape = 21, size = 3)  +
  geom_line(
    data = RM %>% filter(Modo == 'Aplicativo')%>% 
      filter(RM != 'Brasil Urbano') %>% 
      filter(RM != 'Zona Rural'),
    aes(RM, share, group = Modo, color = Modo),
    size = 1.2) +
  geom_point(
    data = RM %>% filter(Modo == 'Aplicativo')%>% 
      filter(RM != 'Brasil Urbano') %>% 
      filter(RM != 'Zona Rural'),
    aes(RM, share, fill = Modo), shape = 21, size = 3.5)  +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = '', y = 'Porcentagem dos Usuários',
    subtitle = 'Distribuição dos usuários por RM') +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = rel(.8)))

p2<-
  RM %>% 
  #filter(Modo == 'Aplicativo') %>% 
  ggplot(aes(RM, frequencia, group = Modo)) +
  geom_line(aes(color = Modo), size = 1) +
  geom_point(aes(color = Modo), size = 2) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_flip() +
  theme_minimal() +
  labs(
    x = '', y = 'Nº de despesas por mês',
    subtitle  = 'Frequência de consumo por RM')+
  theme(legend.position = 'none', panel.grid.minor = element_blank(),
        axis.title = element_text(size = rel(.8))) 
p3<-
  RM %>% 
  #filter(Modo == 'Aplicativo') %>% 
  ggplot(aes(RM, ticket, group = Modo)) +
  geom_line(aes(color = Modo), size = 1) +
  geom_point(aes(color = Modo), size = 2) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  coord_flip() +
  theme_minimal() +
  labs(
    x = '', y = 'Custo médio (R$)',
    subtitle = 'Valor médio da despesa por RM')+
  theme(legend.position = 'none', 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = rel(.8))) 

p3<-
  UF %>% 
  #filter(Modo == 'Aplicativo') %>% 
  arrange(-gasto) %>% 
  ggplot(aes(reorder(UF, -ticket), ticket, group = Modo)) +
  geom_ribbon(
    aes(ymin = ticket - ticket_ci,
        ymax = ticket + ticket_ci, fill = Modo),
    alpha = .6) +
  geom_line(aes(color = Modo), size = 1) +
  geom_point(aes(color = Modo), size = 2) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(
    x = '', y = 'Ticket médio',
    subtitle = 'Custo médio da viagem por UF')+
  theme(legend.position = 'none', 
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = rel(.8))) 
p<-
  (p1+p2)/(p3+p4)
p
library(patchwork)
