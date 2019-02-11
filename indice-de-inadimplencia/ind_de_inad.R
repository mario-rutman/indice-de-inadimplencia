library(tidyverse)
library(lubridate)
library(DT)
library(readr)

# Para ficar no formato brasileiro.
Sys.setlocale("LC_ALL", "pt_BR.UTF-8")

# Para não aparecer notação científica.
options(scipen=999)

mes_ano_referencia <- rep(seq.Date(from=as.Date("2015-01-01"),to=as.Date("2018-12-01"),
                                   by="month"), each = 8)
tipo_de_tributo <- rep(rep(c('ICMS', 'FECP'), each = 4), 48)
natureza_do_tributo <- rep(rep(c('Operações Próprias', 'Substituição Tributária'), each = 2), 96)
pg_vista_ou_parcelado <- rep(rep(c('Pagamento à vista', 'Parcelado'), each = 1), 192)

set.seed(5)
debito_declarado <- as.vector(sample(c(0:9), size=384, replace=TRUE, 
                                     prob=c(0.05, 0.02, 0.15, 0.1, 0.1, 0.1, 0.1, 0.2, 0.08, 0.1)))

set.seed(2)
valor_pago <- as.vector(sample(c(0:9), size=384, replace=TRUE, 
                               prob=c(0.05, 0.02, 0.15, 0.1, 0.1, 0.1, 0.1, 0.2, 0.08, 0.1)))

# Criando o data frame.
inadimplencia <- data.frame(mes_ano_referencia, tipo_de_tributo, natureza_do_tributo,
                            pg_vista_ou_parcelado, debito_declarado, valor_pago) %>% 
  
  # Tirando as linhas onde o parcelamento indica valor pago maior ou igual débito declarado
  filter(!(pg_vista_ou_parcelado == "Parcelado" & valor_pago >= debito_declarado))

# Para exportar para excel
write_excel_csv2(inadimplencia, "inadimplencia.csv")

# Salvando e lendo RDS
write_rds(inadimplencia, "inadimplencia.rds")

inadimplencia <- read_rds("inadimplencia.rds")

# Agora começam as sumarizações para fazer os gráficos.

# GRÁFICO 01. Inadimplência de jan-2015 até dez-2018, mês a mês.
inadimplencia %>% 
  # agrupando em coluna com datas, por mes.
  group_by(mes_ano_referencia) %>%
  summarise(indice_inadimplencia = 1 - (sum(valor_pago)/sum(debito_declarado)),
            inadimplencia = sum(debito_declarado - valor_pago)) %>%
  ggplot(aes(x = mes_ano_referencia, y = inadimplencia)) +
  geom_line(size = 2, color = 'firebrick1', alpha = 0.8) +
  geom_point(size = 4, shape = 21, fill = 'white') +
  geom_text(aes(label = paste(round(100*indice_inadimplencia, 0),"%")), vjust = 2) +
  geom_text(aes(label = paste("R$", inadimplencia)), vjust = -1) +
  labs(title="Inadimplência de ICMS e FECP somadas de jan/2015 até dez/2018.",
       subtitle = "A inadimplência é medida pela diferença entre o total dos débitos declarados pelo contribuinte e os valores pagos,\ 
       o índice de inadimplência é a inadimplência dividida pelo total dos débitos declarados.",
       caption = "Fonte: dados simulados para protótipo",
       y = element_blank(), x = element_blank()) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 month") +
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 8, angle = 0),
        axis.text.y = element_blank(), # tirando o testo e os traços do eixo y
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(), # tirando linhas de grade y, isto é, horizontais.
        panel.grid.minor.y = element_blank(),
        panel.grid = element_line(colour = "gray75"),
        #trabalhando elementos do fundo e contorno do gráfico.
        panel.background = element_rect(fill = "cornsilk",
                                        colour = "black",
                                        size = 1.5, linetype = "solid"))

# GRÁFICO 02. Inadimplência de jan-2015 até dez-2018, mês a mês, separada por ICMS e FECP.
inadimplencia %>% 
  # agrupando em coluna com datas, por mes.
  group_by(mes_ano_referencia, tipo_de_tributo) %>%
  summarise(indice_inadimplencia = 1 - (sum(valor_pago)/sum(debito_declarado)),
            inadimplencia = sum(debito_declarado - valor_pago)) %>% 
  ggplot(aes(x = mes_ano_referencia, y = inadimplencia)) +
  geom_line(aes(color = tipo_de_tributo), size = 2, alpha = 0.8) +
  scale_colour_manual(values = c("darkorchid1", "tan1")) +
  geom_point(size = 4, shape = 21, fill = 'white') +
  geom_text(aes(label = paste(round(100*indice_inadimplencia, 0),"%")), vjust = 2) +
  geom_text(aes(label = paste("R$", inadimplencia)), vjust = -1) +
  labs(title="Inadimplência de ICMS e FECP em valores relativos e absolutos de jan/2015 até dez/2018.",
       subtitle = "A inadimplência é medida pela diferença entre o total dos débitos declarados pelo contribuinte e os valores pagos,\ 
       o índice de inadimplência é a inadimplência dividida pelo total dos débitos declarados.",
       caption = "Fonte: dados simulados para protótipo",
       y = element_blank(), x = element_blank()) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 month") +
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 8, angle = 0),
        legend.position = "top", # colocando a legenda no topo da página.
        legend.title = element_blank(), # retirando o título da legenda.
        axis.text.y = element_blank(), # tirando o testo e os traços do eixo y
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(), # tirando linhas de grade y, isto é, horizontais.
        panel.grid.minor.y = element_blank(),
        panel.grid = element_line(colour = "gray75"),
        #trabalhando elementos do fundo e contorno do gráfico.
        panel.background = element_rect(fill = "cornsilk",
                                        colour = "black",
                                        size = 1.5, linetype = "solid"))

# GRÁFICO 03. Inadimplência de jan-2015 até dez-2018, mês a mês, separada por Operações Próprias e Substituição Tributária.
inadimplencia %>% 
  group_by(mes_ano_referencia, natureza_do_tributo) %>%
  summarise(indice_inadimplencia = 1 - (sum(valor_pago)/sum(debito_declarado)),
            inadimplencia = sum(debito_declarado - valor_pago)) %>%
  ggplot(aes(x = mes_ano_referencia, y = inadimplencia)) +
  geom_line(aes(color = natureza_do_tributo), size = 2, alpha = 0.8) +
  scale_colour_manual(values = c("darkorchid1", "tan1")) +
  geom_point(size = 4, shape = 21, fill = 'white') +
  geom_text(aes(label = paste(round(100*indice_inadimplencia, 0),"%")), vjust = 2) +
  geom_text(aes(label = paste("R$", inadimplencia)), vjust = -1) +
  labs(title="Inadimplência relativa a Operações próprias e Substituição tributária em valores relativos e absolutos de jan/2015 até dez/2018.",
       subtitle = "A inadimplência é medida pela diferença entre o total dos débitos declarados pelo contribuinte e os valores pagos,\ 
       o índice de inadimplência é a inadimplência dividida pelo total dos débitos declarados.",
       caption = "Fonte: dados simulados para protótipo",
       y = element_blank(), x = element_blank()) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 month") +
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 8, angle = 0),
        legend.position = "top", # colocando a legenda no topo da página.
        legend.title = element_blank(), # retirando o título da legenda.
        axis.text.y = element_blank(), # tirando o testo e os traços do eixo y
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(), # tirando linhas de grade y, isto é, horizontais.
        panel.grid.minor.y = element_blank(),
        panel.grid = element_line(colour = "gray75"),
        #trabalhando elementos do fundo e contorno do gráfico.
        panel.background = element_rect(fill = "cornsilk",
                                        colour = "black",
                                        size = 1.5, linetype = "solid"))
# GRÁFICO 04. Inadimplência de jan-2015 até dez-2018, mês a mês, separada por pagamento à vista e parcelado.
inadimplencia %>% 
  group_by(mes_ano_referencia, pg_vista_ou_parcelado) %>%
  summarise(indice_inadimplencia = 1 - (sum(valor_pago)/sum(debito_declarado)),
            inadimplencia = sum(debito_declarado - valor_pago)) %>%
  ggplot(aes(x = mes_ano_referencia, y = inadimplencia)) +
  geom_line(aes(color = pg_vista_ou_parcelado), size = 2, alpha = 0.8) +
  scale_colour_manual(values = c("darkorchid1", "tan1")) +
  geom_point(size = 4, shape = 21, fill = 'white') +
  geom_text(aes(label = paste(round(100*indice_inadimplencia, 0),"%")), vjust = 2) +
  geom_text(aes(label = paste("R$", inadimplencia)), vjust = -1) +
  labs(title="Inadimplência relativa a pagamento à vista ou parcelamento em valores relativos e absolutos de jan/2015 até dez/2018.",
       subtitle = "A inadimplência é medida pela diferença entre o total dos débitos declarados pelo contribuinte e os valores pagos,\ 
       o índice de inadimplência é a inadimplência dividida pelo total dos débitos declarados.",
       caption = "Fonte: dados simulados para protótipo",
       y = element_blank(), x = element_blank()) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 month") +
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 8, angle = 0),
        legend.position = "top", # colocando a legenda no topo da página.
        legend.title = element_blank(), # retirando o título da legenda.
        axis.text.y = element_blank(), # tirando o testo e os traços do eixo y
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(), # tirando linhas de grade y, isto é, horizontais.
        panel.grid.minor.y = element_blank(),
        panel.grid = element_line(colour = "gray75"),
        #trabalhando elementos do fundo e contorno do gráfico.
        panel.background = element_rect(fill = "cornsilk",
                                        colour = "black",
                                        size = 1.5, linetype = "solid"))

# Agora fazer as tabelas para informações detalhadas.

# Tabela 01 - Esta tabela é para informações detalhadas por ano.
# Primeiro preparar o inadimplencia. Duplicar a coluna mes_de_referencia em ano e mês,
# Mas nesta tabela vou usar só o ano.
# Depois criar a coluna crédito a recupar e índice de inadimplência.
inadimplencia %>% separate(mes_ano_referencia, c("Ano", "Mês")) %>% # Separando a data em ano e mês.
  mutate(valor_a_recuperar = debito_declarado - valor_pago) %>% 
  group_by(Ano, tipo_de_tributo, natureza_do_tributo, pg_vista_ou_parcelado) %>% 
  summarise(debito_declarado = sum(debito_declarado), valor_pago = sum(valor_pago),
            valor_a_recuperar = sum(valor_a_recuperar),
            indice_inadimplencia = 1- (sum(valor_pago)/sum(debito_declarado))) %>% 
  datatable(rownames = FALSE, class = 'cell-border stripe',
            caption = htmltools::tags$caption('Inadimplência por ano.',
                                              style = 'caption-side: top; text-align: left; color:black;
                                              font-size:350% ;',''),
            colnames = c('Ano', 'Tributo','Natureza', 'Pg à vista X Parcelado', 'Valor Declarado', 'Valor Pago', 'Inadimplência', 'Índice de Inadimplência'),
            filter = 'top', options = list(pageLength = 5, autoWidth = TRUE)) %>% 
  formatCurrency(columns = c (5, 6, 7), currency = "R$ ", interval = 3, mark = ",", digits = 0) %>% 
  formatPercentage(columns = 8, digits = 0, interval = 3, mark = ",", 
                   dec.mark = getOption("OutDec")) %>% 
  formatStyle(columns = c(1:8), fontSize = '175%')



# Tabela 02 - Esta tabela é para informações detalhadas por mê e ano.
# Primeiro preparar o inadimplencia. Duplicar a coluna mes_de_referencia em ano e mê,
# Depois criar a coluna crédito a recupar e índice de inadimplência.
inadimplencia %>% separate(mes_ano_referencia, c("Ano", "Mês")) %>% # Separando a data em ano e mês.
  mutate(valor_a_recuperar = debito_declarado - valor_pago) %>% 
  group_by(Ano, Mês, tipo_de_tributo, natureza_do_tributo, pg_vista_ou_parcelado) %>% 
  summarise(debito_declarado = sum(debito_declarado), valor_pago = sum(valor_pago),
            valor_a_recuperar = sum(valor_a_recuperar),
            indice_inadimplencia = 1- (sum(valor_pago)/sum(debito_declarado))) %>% 
  datatable(rownames = FALSE, class = 'cell-border stripe',
            caption = htmltools::tags$caption('Inadimplência por ano e mês.',
                                              style = 'caption-side: top; text-align: left; color:black;
                                              font-size:350% ;',''),
            colnames = c('Ano', 'Mês (preencher com número)', 'Tributo','Natureza', 'Pg à vista X Parcelado', 'Valor Declarado', 'Valor Pago', 'Inadimplência', 'Índice de Inadimplência'),
            filter = 'top', options = list(pageLength = 5, autoWidth = TRUE)) %>% 
  formatCurrency(columns = c (6, 7, 8), currency = "R$ ", interval = 3, mark = ",", digits = 0) %>% 
  formatPercentage(columns = 9, digits = 0, interval = 3, mark = ",", 
                   dec.mark = getOption("OutDec")) %>% 
  formatStyle(columns = c(1:9), fontSize = '175%') %>% 
  formatDate(columns = 2, method =  "toLocaleDateString", 
             params = list('pt-BR', list(month = 'long')))
