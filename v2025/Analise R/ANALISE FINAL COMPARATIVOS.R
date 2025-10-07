# pacotes -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(scales)
library(extrafont)
library(gganimate)
library(ggbeeswarm)
library(plotly)

library(colorspace)
library(RColorBrewer)
library(viridis)

library(geobr)
library(cartogram)
library(sf)
library(geojsonsf)

library(janitor)


# estilo dos gráficos -----------------------------------------------------

loadfonts()

tema <- function() {
  theme_minimal() + # nolint
    theme( # nolint
      text = element_text(family = "Lora", colour = "grey20"), # nolint
      title = element_text(size = 10, color = "dimgrey", face = "plain"),
      plot.subtitle = element_text(color = "grey20", face = "plain", size = 10),
      axis.text = element_text(colour = "grey20", size = 8, family = "Source Sans Pro"), # nolint
      plot.caption = element_text(face = "italic"),
      panel.grid.major = element_blank(),  # nolint
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(size = 0.4), # nolint
      axis.ticks.length = unit(.2, "cm"), # nolint
      axis.title = element_text(size = 8, colour = "grey20"),
      legend.position = 'none', # nolint
      legend.text = element_text(size = 8, family = "Source Sans Pro"),
      legend.title = element_text(size = 9, family = "Source Sans Pro")
    )
}

tema_barra <- function() {
  tema() +
    theme( # nolint
      axis.ticks.y = element_blank() # nolint
    )
}

tema_mapa <- function() {
  tema() +
    theme(axis.line = element_blank(), # nolint
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          legend.text = element_text(size = 10), # nolint
          plot.background = element_blank(),
          panel.background = element_blank())
}


# dados iniciais ----------------------------------------------------------

Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/Pandoc")

setwd("/Users/gustavo.silva/OneDrive - Tesouro Nacional/Área de Trabalho/projetos/RAIO_X_EMPRESAS_DOS_ESTADOS/comparativo") # nolint

tab_uf <- read_excel("./dados/dados-originais/tab_ufs.xlsx") %>%
  select(Estado, Nome_estado, REGIAO)

dados_raw_2023 <- read_excel("./dados/dados-originais/quadro_estatais_2023_v3.xlsx", sheet = "lista definitiva") # nolint
dados_raw_2022 <- read_excel("./dados/dados-originais/quadro_estatais_2022.xlsx", sheet = "Todos") # nolint
dados_raw_2021 <- read_excel("./dados/dados-originais/quadro_estatais_2021.xlsx", sheet = "Todos") # nolint

tab_definicoes_setores <- read_excel("./dados/dados-originais/tab_setores.xlsx", sheet = "def") # nolint

dados_selecionados_raw_2023 <- dados_raw_2023 %>%
  select(
    Estado    = UF,
    emp       = `Estatal`,
    sit       = `Situação`,
    setor      = `Setor`,
    esp       = `Espécie`,
    dep       = `Dependência`,
    PL        = `Patrimônio Líquido`,
    lucros    = `Lucro / Prejuízo Líquido do Exercício`,
    gov_ca    = `Possui Conselho de Administração`,
    gov_cf    = `Possui Conselho Fiscal`,
    gov_aud   = `Possui Comitê de Autidoria`,
    maior_rem = `Remuneração bruta total paga no ano`, # nolint
    plr_rva   = `Foi Distribuído PLR ou RVA no exercício`,
    qde_empregados = `Número de Empregados (incluindo temporários e terceirizados)`, # nolint
    desp_investimento = `Investimento (por competência)`,
    desp_pessoal = `Despesa com Pessoal, incluindo temporários e terceirizados (por competência)`, # nolint
    Dividendos = `Dividendos e Juros sobre Capital Próprio pagos ao Tesouro Estadual / Municipal (pago)`, # nolint
    `Subvenção` = `Subvenções Recebidas do Tesouro Estadual / Municipal - Exercício`,
    `Subvenção (anterior)` = `Subvenções Recebidas do Tesouro Estadual / Municipal - Exercício anterior`,
    `Reforço de Capital` = `Reforço de Capital -Exercício`,
    `Reforço de Capital (anterior)` = `Reforço de Capital -Exercício anterior`,
    capital = `Capital Social a Integralizar -Exercício`,
    link      = `Link Carta Anual (copiar)`,
    indicio_dependencia = `Dependência`,
    var_capital = `Variação do Capital Social`,
    var_acoes = `Variação das Ações`,
  )

# Remove all special characters from the 'Name' column
dados_selecionados_raw_2023$emp <- str_replace_all(dados_selecionados_raw_2023$emp, "[^[:alnum:] ]", "") # nolint

dados_selecionados_raw_2022 <- dados_raw_2022 %>%
  select(
    Estado    = UF,
    emp       = `Estatal`,
    sit       = `Situação`,
    setor      = `Setor`,
    esp       = `Espécie`,
    dep       = `Dependência`,
    PL        = `Patrimônio Líquido`,
    lucros    = `Lucro / Prejuízo Líquido do Exercício`,
    gov_ca    = `Possui Conselho de Administração`,
    gov_cf    = `Possui Conselho Fiscal`,
    gov_aud   = `Possui Comitê de Auditoria`, #(sic)
    maior_rem = `Remuneração bruta total paga no ano (empregado que recebeu a maior remuneração)`, # nolint
    plr_rva   = `Foi Distribuído PLR ou RVA no exercício`,
    qde_empregados = `Número de Empregados (incluindo temporários e terceirizados)`, # nolint
    desp_investimento = `Investimento (por competência)`,
    desp_pessoal = `Despesa com Pessoal, incluindo temporários e terceirizados (por competência)`, # nolint
    Dividendos = `Dividendos e Juros sobre Capital Próprio pagos ao Tesouro Estadual / Municipal (pago)`, # nolint
    `Subvenção` = `Subvenções - Exercício`,
    `Subvenção (anterior)` = `Subvenções  - Exercício anterior`,
    `Reforço de Capital` = `Reforço de Capital - Exercício`,
    `Reforço de Capital (anterior)` = `Reforço de Capital - Exercício anterior`,
    capital = `Capital Social Integralizado - Exercício`,
    #var_capital = `Variação Capital Social Integralizado`, # nolint
    #var_acoes = `Crescimento ações`, # nolint
    link      = `Link Carta Anual`,
    indicio_dependencia = `Indícios de Dependência`
  )

# Remove all special characters from the 'Name' column
dados_selecionados_raw_2022$emp <- str_replace_all(dados_selecionados_raw_2022$emp, "[^[:alnum:] ]", "") # nolint

# Evolução da quantidade de empresas estatais por Estado ---------------------------------------------------------- # nolint

dados_selecionados_raw_2021 <- dados_raw_2021 %>%
  select(
    Estado    = UF,
    emp       = `Estatal`,
    sit       = `Situação`,
    setor      = `Setor`,
    esp       = `Espécie`,
    dep       = `Dependência`,
    PL        = `Patrimônio Líquido`,
    lucros    = `Lucro / Prejuízo Líquido do Exercício`,
    gov_ca    = `Possui Conselho de Administração`,
    gov_cf    = `Possui Conselho Fiscal`,
    gov_aud   = `Possui Comitê de Autidoria`, #(sic)
    maior_rem = `Remuneração bruta total paga no ano (empregado que recebeu a maior remuneração)`, # nolint
    plr_rva   = `Foi Distribuído PLR ou RVA no exercício`,
    qde_empregados = `Número de Empregados (incluindo temporários e terceirizados)`, # nolint
    desp_investimento = `Investimento (por competência)`,
    desp_pessoal = `Despesa com Pessoal, incluindo temporários e terceirizados (por competência)`, # nolint
    Dividendos = `Dividendos e Juros sobre Capital Próprio pagos ao Tesouro Estadual / Municipal (pago)`, # nolint
    `Subvenção` = `Subvenções - Exercício`,
    `Subvenção (anterior)` = `Subvenções - Exercício anterior`,
    `Reforço de Capital` = `Reforço de Capital - Exercício`,
    `Reforço de Capital (anterior)` = `Reforço de Capital - Exercício anterior`,
    capital = `Capital Social Integralizado - Exercício`,
    #var_capital = `Variação Capital Social Integralizado`, # nolint
    #var_acoes = `Crescimento ações`, # nolint
    link      = `Link Carta Anual`,
    indicio_dependencia = `Indícios de Dependência`
  )

  dados_selecionados_raw_2021$emp <- str_replace_all(dados_selecionados_raw_2021$emp, "[^[:alnum:] ]", "") # nolint

# limpeza -----------------------------------------------------------------

sim <- c("SIM", "Sim", "CONTROLE INTERNO", "Possui", "DEPENDENTE")
nao <- c("NÃO", "Não", "Não Possui", "Não possui", "NAO", "NÂO", "NÃO DEPENDENTE") # nolint

dados_selecionados2023 <- dados_selecionados_raw_2023 %>%
  left_join(tab_uf) %>%
  mutate(
    dep     = str_to_title(dep), # nolint
    dep     = ifelse(is.na(dep), "Não Informado", dep),
    gov     = gov_ca %in% sim & gov_cf %in% sim & gov_aud %in% sim,
    plr_rva = ifelse(plr_rva %in% sim, "Sim",
                     ifelse(plr_rva %in% nao, "Não", plr_rva))) %>%
  mutate_at(
    .vars = c("PL", "lucros", "desp_investimento", "desp_pessoal", "qde_empregados"), # nolint
    .funs = as.numeric) %>%
  mutate(result_NA = is.na(Dividendos) & is.na(`Subvenção`) & is.na(`Reforço de Capital`)) %>% # nolint
  mutate_at(.vars = vars("Dividendos", `Subvenção`, `Reforço de Capital`),
            .funs = ~ifelse(is.na(.), 0, .)) %>%
  mutate(`Resultado para o Estado Acionista` = ifelse(result_NA, NA, Dividendos - `Subvenção` - `Reforço de Capital`)) # nolint

dados_selecionados2022 <- dados_selecionados_raw_2022 %>%
  left_join(tab_uf) %>%
  mutate(
    dep     = str_to_title(dep), # nolint
    dep     = ifelse(is.na(dep), "Não Informado", dep),
    gov     = gov_ca %in% sim & gov_cf %in% sim & gov_aud %in% sim,
    plr_rva = ifelse(plr_rva %in% sim, "Sim",
                     ifelse(plr_rva %in% nao, "Não", plr_rva))) %>%
  mutate_at(
    .vars = c("PL", "lucros", "desp_investimento", "desp_pessoal", "qde_empregados"), # nolint
    .funs = as.numeric) %>%
  mutate(result_NA = is.na(Dividendos) & is.na(`Subvenção`) & is.na(`Reforço de Capital`)) %>% # nolint
  mutate_at(.vars = vars("Dividendos", `Subvenção`, `Reforço de Capital`),
            .funs = ~ifelse(is.na(.), 0, .)) %>%
  mutate(`Resultado para o Estado Acionista` = ifelse(result_NA, NA, Dividendos - `Subvenção` - `Reforço de Capital`)) # nolint

dados_selecionados2021 <- dados_selecionados_raw_2021 %>%
  left_join(tab_uf) %>%
  mutate(
    dep     = str_to_title(dep), # nolint
    dep     = ifelse(is.na(dep), "Não Informado", dep),
    gov     = gov_ca %in% sim & gov_cf %in% sim & gov_aud %in% sim,
    plr_rva = ifelse(plr_rva %in% sim, "Sim",
                     ifelse(plr_rva %in% nao, "Não", plr_rva))) %>%
  mutate_at(
    .vars = c("PL", "lucros", "desp_investimento", "desp_pessoal", "qde_empregados"), # nolint
    .funs = as.numeric) %>%
  mutate(result_NA = is.na(Dividendos) & is.na(`Subvenção`) & is.na(`Reforço de Capital`)) %>% # nolint
  mutate_at(.vars = vars("Dividendos", `Subvenção`, `Reforço de Capital`),
            .funs = ~ifelse(is.na(.), 0, .)) %>%
  mutate(`Resultado para o Estado Acionista` = ifelse(result_NA, NA, Dividendos - `Subvenção` - `Reforço de Capital`)) # nolint


# Dados combinados
dados_empresas <- bind_rows(
  dados_selecionados_raw_2021 %>% mutate(Ano = 2021),
  dados_selecionados_raw_2022 %>% mutate(Ano = 2022),
  dados_selecionados_raw_2023 %>% mutate(Ano = 2023)
)


########################################################################################################################################################

## Sumarizando os lucros por setor e ano
#sumario_lucro_setor_ano <- dados_empresas %>%
#  filter(!is.na(lucros)) %>%
#  group_by(setor, Ano) %>%
#  summarise(tot = sum(lucros, na.rm = TRUE), .groups = "drop") %>%
#  mutate(result_pos = ifelse(tot >= 0, "Positivo", "Negativo"))
#
## Calculando o total geral por setor e identificando a categoria para ordenação
#ordenacao_setores <- sumario_lucro_setor_ano %>%
#  group_by(setor) %>%
#  summarise(
#    tot_geral = sum(tot, na.rm = TRUE),
#    pos_neg = case_when(
#      all(tot >= 0) ~ "Positivo",  # Apenas valores positivos
#      all(tot < 0) ~ "Negativo",  # Apenas valores negativos
#      TRUE ~ "Misto"              # Valores mistos (positivos e negativos)
#    ),
#    .groups = "drop"
#  ) %>%
#  arrange(desc(tot_geral), pos_neg) %>% # Ordenação: maior total primeiro, depois por tipo
#  pull(setor)
#
## Ajustando a ordem dos fatores do eixo X
#sumario_lucro_setor_ano$setor <- factor(sumario_lucro_setor_ano$setor, levels = ordenacao_setores)
#
## Criando o gráfico comparativo
#graf_barra_lucro_setor_ano <- ggplot(sumario_lucro_setor_ano,
#                                     aes(x = setor, # Eixo X com a ordem ajustada
#                                         y = tot,
#                                         fill = as.factor(Ano))) +
#  geom_col(width = 1, position = position_dodge(width = 0.9)) + # Barras com mais espaço
#  geom_text(aes(label = format(round(tot / 1e6, 0), big.mark = ".", decimal.mark = ','), # nolint
#                group = Ano),
#            position = position_dodge(width = 0.9), # Ajusta o posicionamento para coincidir com o espaço das barras
#            vjust = -0.5,
#            family = "Source Sans Pro", size = 2.0) + # Tamanho menor para os rótulos
#  coord_flip() +
#  scale_fill_manual(values = c("2021" = "#DC143C", "2022" = "#008080", "2023" = "#3b7302"),
#                    name = "Ano") + # Legenda indicando as cores dos anos
#  scale_y_continuous(labels = function(x) {
#    paste(format(round(x / 1e6, 1), big.mark = ".", decimal.mark = ','), "mi")}, 
#    expand = expansion(mult = .1)) +
#  labs(x = NULL, y = "Lucros / Prejuízos (em milhões)", fill = "Ano") +
#  theme(legend.position = "top", # Legenda no topo
#        legend.title = element_text(size = 10, face = "bold"),
#        legend.text = element_text(size = 9)) + # Ajustes no estilo da legenda
#  tema_barra()
#
## Salvando o gráfico
#ggsave(plot = graf_barra_lucro_setor_ano, "./plots_final/bar_lucro_setor_ano_comparativo.png", h = 6, w = 8, device = "png")

#######################################################################################################################################################

#dados_dependente <- dados_empresas %>%
#  filter(dep == "DEPENDENTE")
#
## Quantidade de empresas por estado e ano (apenas "Dependente")
#emp_por_estado <- dados_dependente %>%
#  group_by(Estado, Ano) %>%
#  summarise(Quantidade = n()) %>%
#  ungroup()
#
## Gráfico ajustado
#ggplot(emp_por_estado, aes(x = reorder(Estado, -Quantidade), y = Quantidade, fill = as.factor(Ano))) + # nolint
#  geom_bar(stat = "identity", position = "dodge") +
#  labs(
#    title = "Comparação da Quantidade de Empresas Estatais por Estado",
#    x = "Estado",
#    y = "Quantidade de Empresas",
#    fill = "Ano"  # Legenda associada às cores
#  ) +
#  scale_fill_manual(values = c("2021" = "#DC143C", "2022" = "#008080", "2023" = "#3b7302")) +
#  theme_minimal() +
#  theme(
#    text = element_text(family = "Lora", colour = "grey20"),
#    axis.text.x = element_text(angle = 45, hjust = 1),
#    legend.position = "top",  # Posição da legenda
#    legend.title = element_text(size = 10, face = "bold"),
#    legend.text = element_text(size = 9)
#  )
#
## Salvar o gráfico
#ggsave("./plots_final/quantidade_empresas_estado_DEPENDENTE.jpeg", width = 10, height = 6)
#
##----------------------------------------------------
#
#
#dados_dependente_n <- dados_empresas %>%
#  filter(dep == "NÃO DEPENDENTE")
#
## Quantidade de empresas por estado e ano (apenas "Dependente")
#emp_por_estado <- dados_dependente_n %>%
#  group_by(Estado, Ano) %>%
#  summarise(Quantidade = n()) %>%
#  ungroup()
#
## Gráfico ajustado
#ggplot(emp_por_estado, aes(x = reorder(Estado, -Quantidade), y = Quantidade, fill = as.factor(Ano))) + # nolint
#  geom_bar(stat = "identity", position = "dodge") +
#  labs(
#    title = "Comparação da Quantidade de Empresas Estatais por Estado",
#    x = "Estado",
#    y = "Quantidade de Empresas",
#    fill = "Ano"  # Legenda associada às cores
#  ) +
#  scale_fill_manual(values = c("2021" = "#DC143C", "2022" = "#008080", "2023" = "#3b7302")) +
#  theme_minimal() +
#  theme(
#    text = element_text(family = "Lora", colour = "grey20"),
#    axis.text.x = element_text(angle = 45, hjust = 1),
#    legend.position = "top",  # Posição da legenda
#    legend.title = element_text(size = 10, face = "bold"),
#    legend.text = element_text(size = 9)
#  )
#
## Salvar o gráfico
#ggsave("./plots_final/quantidade_empresas_estado_NÃO DEPENDENTE.jpeg", width = 10, height = 6)

####################################################################################################################################################

# Evolução do Patrimônio Líquido Total por Ano ---------------------------------------------------------- # nolint

# Dados resumidos por ano
pl_por_ano <- dados_empresas %>%
  group_by(Ano) %>%
  summarise(Patrimonio_Liquido = sum(PL, na.rm = TRUE))

# Formatação dos valores resumidos
formatar_valor <- scales::label_number(scale_cut = scales::cut_short_scale(), 
                                       big.mark = ".", decimal.mark = ",")

# Gráfico ajustado com mais espaço no topo
ggplot(pl_por_ano, aes(x = Ano, y = Patrimonio_Liquido)) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", linetype = "dashed", size = 1) +
  geom_point(size = 3, color = "steelblue") +
  geom_text(aes(label = formatar_valor(Patrimonio_Liquido)), 
            vjust = -1, size = 6, family = "Source Sans Pro", color = "grey20") +
  scale_y_continuous(
    labels = NULL, breaks = NULL,
    expand = expansion(mult = c(0.05, 0.2))  # Expande o topo para evitar corte
  ) +
  scale_x_continuous(breaks = unique(pl_por_ano$Ano)) +  # Garante pontos no eixo X
  labs(
    title = "",
    x = "Ano", 
    y = NULL  # Remove o título do eixo Y
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lora", color = "grey20"),
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.ticks.y = element_blank(),  # Remove ticks do eixo Y
    panel.grid.major.y = element_blank(),  # Remove linhas horizontais principais
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)  # Ajusta a margem superior
  )

# Salvar o gráfico corrigido
ggsave("./plots_final/evolucao_patrimonio_liquido.jpeg", width = 10, height = 7, device = "jpeg")

####################################################################################################################################################


# Situação das Empresas por Ano ---------------------------------------------------------- # nolint

# Agrupamento por Ano e Situação
library(dplyr)
library(ggplot2)
library(stringr)

# Agrupando por Ano, Situação e Dependência
situacao_por_ano <- dados_empresas %>%
  group_by(Ano, sit, dep) %>%
  summarise(Quantidade = n(), .groups = "drop")

# Ajustando os nomes de situação
situacao_por_ano <- situacao_por_ano %>%
  mutate(
    sit = if_else(sit == "INATIVO", "EM LIQUIDAÇÃO", sit), # Substitui "INATIVO" por "EM LIQUIDAÇÃO"
    sit_dep = case_when(
      sit == "ATIVA"           & dep == "NÃO DEPENDENTE" ~ "ATIVA - Não dependente",
      sit == "ATIVA"           & dep == "DEPENDENTE"     ~ "ATIVA - dependente",
      sit == "EM LIQUIDAÇÃO"   & dep == "NÃO DEPENDENTE" ~ "EM LIQUIDAÇÃO - Não dependente",
      sit == "EM LIQUIDAÇÃO"   & dep == "DEPENDENTE"     ~ "EM LIQUIDAÇÃO - dependente",
      TRUE ~ paste(sit, dep, sep = " - ") # fallback para eventuais casos
    )
  )

# Cores personalizadas
cores_personalizadas <- c(
  "ATIVA - Não dependente"        = "#006400", 
  "ATIVA - dependente"            = "#e67e00", 
  "EM LIQUIDAÇÃO - Não dependente" = "#6bb56b",
  "EM LIQUIDAÇÃO - dependente"     = "#ffba65"
)

# Criando o gráfico
ggplot(situacao_por_ano, aes(x = as.factor(Ano), y = Quantidade, fill = sit_dep)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = Quantidade),
            position = position_stack(vjust = 0.5),
            size = 8, color = "white", fontface = "bold") +
  labs(
    x = "Ano",
    y = NULL,  # Remove título do eixo y
    fill = ""
  ) +
  scale_fill_manual(values = cores_personalizadas) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lora", colour = "grey20"),
    axis.text.x = element_text(size = 30, face = "bold"),
    axis.text.y = element_blank(),  # Remove os valores do eixo y
    legend.position = "bottom",       # Coloca a legenda à direita
    legend.title = element_text(size = 30, face = "bold"),
    legend.text = element_text(size = 29)
  ) +
  guides(fill = guide_legend(ncol = 1))  # Define a legenda em uma única coluna

# Salvar o gráfico
ggsave("./plots_final/situacao_empresas.jpeg", width = 15, height = 15)


#####################################################################################################################################################


# Evolução de Lucros/Prejuízos Totais por Ano ---------------------------------------------------------- # nolint

# Formatação dos valores resumidos
formatar_valor <- scales::label_number(scale_cut = scales::cut_short_scale(), 
                                       big.mark = ".", decimal.mark = ",")

# Dados de Lucros/Prejuízos Totais por Ano
lucros_por_ano <- dados_empresas %>%
  group_by(Ano) %>%
  summarise(Lucro_Total = sum(lucros, na.rm = TRUE))

# Separação dos dados em Lucros e Prejuízos
lucros_por_ano <- lucros_por_ano %>%
  mutate(Tipo = ifelse(Lucro_Total >= 0, "Lucro", "Prejuízo"))

# Gráfico ajustado
ggplot() +
  # Linha para Lucro (verde)
  geom_line(data = lucros_por_ano %>% filter(Tipo == "Lucro"),
            aes(x = Ano, y = Lucro_Total, color = "Lucro"), 
            size = 1.2) +
  geom_point(data = lucros_por_ano %>% filter(Tipo == "Lucro"),
             aes(x = Ano, y = Lucro_Total), 
             size = 3, color = "#008080") +
  # Linha para Prejuízo (laranja)
  geom_line(data = lucros_por_ano %>% filter(Tipo == "Prejuízo"),
            aes(x = Ano, y = Lucro_Total, color = "Prejuízo"), 
            size = 1.2) +
  geom_point(data = lucros_por_ano %>% filter(Tipo == "Prejuízo"),
             aes(x = Ano, y = Lucro_Total), 
             size = 3, color = "#DC143C") +
  # Valores em cada ponto
  geom_text(aes(x = Ano, y = Lucro_Total, label = scales::label_number(scale_cut = scales::cut_short_scale())(Lucro_Total),
                color = Tipo),
            data = lucros_por_ano, vjust = -1, size = 4, family = "Source Sans Pro") +
  # Escala de cores
  scale_color_manual(values = c("Lucro" = "#008080", "Prejuízo" = "#DC143C"), name = "Resultado") +
  # Configuração dos eixos
  scale_y_continuous(
    labels = NULL, breaks = NULL,
    expand = expansion(mult = c(0.05, 0.2))
  ) +
  scale_x_continuous(breaks = unique(lucros_por_ano$Ano)) +
  # Título e legenda
  labs(
    title = "Evolução dos Lucros",
    x = "Ano",
    y = NULL
  ) +
  # Tema ajustado
  theme_minimal() +
  theme(
    text = element_text(family = "Lora", color = "grey20"),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",  # Posiciona a legenda corretamente
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )

# Salvar o gráfico corrigido
ggsave("./plots_final/evolucao_lucros.jpeg", width = 10, height = 7, device = "jpeg")

#-------------------------------------------------------


library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# Exemplo de formatação de valores
formatar_valor <- label_number(
  scale_cut   = cut_short_scale(), 
  big.mark    = ".", 
  decimal.mark= ","
)

# 1) Somar separadamente os valores positivos (lucro) e negativos (prejuízo) por ano
lucros_por_ano <- dados_empresas %>%
  group_by(Ano) %>%
  summarise(
    Lucro_Total   = sum(lucros[lucros > 0], na.rm = TRUE),
    Prejuizo_Total = sum(lucros[lucros < 0], na.rm = TRUE),
    .groups = "drop"
  )

# 2) Transformar o data frame em formato longo (tidy)
lucros_por_ano_long <- lucros_por_ano %>%
  pivot_longer(
    cols = c("Lucro_Total", "Prejuizo_Total"),
    names_to = "Tipo",
    values_to = "Valor"
  ) %>%
  # Ajustar o nome das categorias
  mutate(
    Tipo = ifelse(Tipo == "Lucro_Total", "Lucro", "Prejuízo")
  )

# 3) Plotar as linhas com a linha no ponto zero
ggplot(lucros_por_ano_long, aes(x = Ano, y = Valor, color = Tipo)) +
  # Linha de referência no ponto zero
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", size = 0.8) +
  # Linhas
  geom_line(size = 1.2) +
  # Pontos
  geom_point(size = 3) +
  # Rótulos em cada ponto
  geom_text(
    aes(label = formatar_valor(Valor)),
    vjust = -1,
    size  = 7,
    family = "Source Sans Pro"
  ) +
  # Escala de cores manual
  scale_color_manual(
    values = c("Lucro" = "#008080", "Prejuízo" = "#DC143C"),
    name = "Resultado"
  ) +
  # Configuração do eixo Y
  scale_y_continuous(
    labels = formatar_valor,
    breaks = scales::pretty_breaks(n = 8),
    expand = expansion(mult = c(0.05, 0.2))
  ) +
  scale_x_continuous(breaks = unique(lucros_por_ano_long$Ano)) +
  # Títulos e legenda
  labs(
    title = "",
    x = "Ano",
    y = "Valores (em milhões)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lora", color = "grey20"),
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"), # Grelha principal
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 16)
  )

# (Opcional) Salvar o gráfico
ggsave("./plots_final/evolucao_lucros_prejuizos.jpeg", width = 10, height = 7, device = "jpeg")

###############################################################################################################################################


ggplot(pl_distribuicao, aes(x = "", y = pct, fill = plr_rva)) +
  geom_col(
    width = 1,
    color = NA,    # remove linha de borda
    size = 0,      # zera a espessura (importante!)
    linejoin = "round"
  ) +
  coord_polar("y", start = 0, clip = "off") +
  # Remove qualquer expansão extra no eixo
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ Ano, ncol = 3) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    size = 15,
    color = "white"
  ) +
  scale_fill_manual(
    values = c("Distribuído" = "#735D36", "Não Distribuído" = "#F4C773"),
    name = "PLR/RVA"
  ) +
  labs(
    title = "",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    legend.position = "bottom",  # Move a legenda para a parte inferior
    legend.text = element_text(size = 25),  # Aumenta o tamanho do texto da legenda
    legend.title = element_text(size = 26, face = "bold"),  # Aumenta o título da legenda
    strip.text = element_text(size = 30, face = "bold")
  )

ggsave("./plots_final/distribuicao_PL.jpeg", width = 15, height = 10)


##################################################################################################################################################

#library(dplyr)
#library(ggplot2)
#
## Passo 1) Criação do data frame agregado, se ainda não tiver
#emp_por_estado_dep <- dados_empresas %>%
#  group_by(Estado, Ano, dep) %>%
#  summarise(Quantidade = n(), .groups = "drop")
#
## Passo 2) Determinar o valor máximo de Quantidade
#max_y <- max(emp_por_estado_dep$Quantidade, na.rm = TRUE)
#
## Ajustar a ordem alfabética no eixo X
#emp_por_estado_dep$Estado <- factor(emp_por_estado_dep$Estado, levels = sort(unique(emp_por_estado_dep$Estado)))
#
## Passo 3) Gerar o gráfico
#ggplot(emp_por_estado_dep,
#       aes(x = Estado, # Agora em ordem alfabética
#           y = Quantidade,
#           fill = as.factor(Ano))) + 
#  geom_bar(stat = "identity", position = "dodge") +
#  facet_wrap(~ dep, ncol = 1) +  # Cada tipo de Dependência em uma linha
#  scale_y_continuous(limits = c(0, max_y)) +  # ajusta o limite superior do eixo Y
#  labs(
#    title = "Quantidade de Empresas por Estado, Ano e Dependência (Facet por dep)",
#    x = "Estado",
#    y = "Quantidade de Empresas",
#    fill = "Ano"
#  ) +
#  scale_fill_manual(values = c("2021" = "#DC143C", "2022" = "#008080", "2023" = "#3b7302")) +
#  theme_minimal() +
#  theme(
#    text = element_text(family = "Lora", colour = "grey20"),
#    axis.text.x = element_text(angle = 45, hjust = 1),
#    legend.position = "top"
#  )
#
## (Opcional) Salvar o gráfico
#ggsave("./plots_final/quantidade_empresas_estado_dep.jpeg", 
#       width = 10, height = 8, dpi = 300)
#
##------------------------------------------------
#
#library(dplyr)
#library(ggplot2)
#
## 1) Criar a nova coluna no data frame principal
#dados_empresas <- dados_empresas %>%
#  mutate(cat_lucro_prejuizo = ifelse(lucros > 0, "Lucro", "Prejuízo"))
#
## 2) Agrupar e resumir por Estado, Ano e nova coluna
#emp_por_estado_lucro <- dados_empresas %>%
#  group_by(Estado, Ano, cat_lucro_prejuizo) %>%
#  summarise(Quantidade = n(), .groups = "drop")
#
## 3) Ajustar a ordem alfabética no eixo X
#emp_por_estado_lucro$Estado <- factor(emp_por_estado_lucro$Estado, levels = sort(unique(emp_por_estado_lucro$Estado)))
#
## 4) Determinar o valor máximo de Quantidade
#max_y <- max(emp_por_estado_lucro$Quantidade, na.rm = TRUE)
#
## 5) Gerar o gráfico
#ggplot(emp_por_estado_lucro,
#       aes(x = Estado, # Agora em ordem alfabética
#           y = Quantidade,
#           fill = as.factor(Ano))) +
#  geom_bar(stat = "identity", position = "dodge") +
#  facet_wrap(~ cat_lucro_prejuizo, ncol = 1) +  # Facet agora é por Lucro/Prejuízo
#  scale_y_continuous(limits = c(0, max_y)) +   # Ajuste do eixo Y
#  labs(
#    title = "Quantidade de Empresas por Estado, Ano e Lucro/Prejuízo",
#    x = "Estado",
#    y = "Quantidade de Empresas",
#    fill = "Ano"
#  ) +
#  scale_fill_manual(values = c("2021" = "#DC143C", "2022" = "#008080", "2023" = "#3b7302")) +
#  theme_minimal() +
#  theme(
#    text = element_text(family = "Lora", colour = "grey20"),
#    axis.text.x = element_text(angle = 45, hjust = 1),
#    legend.position = "top"
#  )
#
## (opcional) salvar o gráfico
#ggsave("./plots_final/quantidade_empresas_estado_lucro_prejuizo.jpeg",
#       width = 10, height = 8, dpi = 300)

###################################################################################################################################

dados_dependente <- dados_empresas

emp_por_estado <- dados_dependente %>%
  group_by(Estado, Ano) %>%
  summarise(Quantidade = n()) %>%
  ungroup()

# Gráfico ajustado
ggplot(emp_por_estado, aes(x = reorder(Estado, -Quantidade), y = Quantidade, fill = as.factor(Ano))) + # nolint
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    #title = "Comparação da Quantidade de Empresas Estatais por Estado",
    x = "Estado",
    y = "Quantidade de Empresas",
    fill = "Ano"  # Legenda associada às cores
  ) +
  scale_fill_manual(values = c("2021" = "#f2ac29", "2022" = "#718c35", "2023" = "#009ADE")) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lora", colour = "grey20"),
    axis.text.x = element_text(angle = 45, hjust = 1, size=25),
    legend.position = "top",  # Posição da legenda
    legend.title = element_text(size = 30, face = "bold"),
    legend.text = element_text(size = 29)
  )

# Salvar o gráfico
ggsave("./plots_final/quantidade_empresas_estado.jpeg", width = 15, height = 10)

###################################################################################################################################

## Adicionar coluna "dep" no DataFrame emp_por_estado
#emp_por_estado_dep <- dados_dependente %>%
#  group_by(Estado, Ano, dep) %>%
#  summarise(Quantidade = n()) %>%
#  ungroup() %>%
#  mutate(
#    dep = factor(dep, levels = c("DEPENDENTE", "NÃO DEPENDENTE")),
#    categoria = paste(dep, Ano, sep = " - ") # Combinação de dependência e ano
#  )
#
## Vetor de cores ajustado para tons de vermelho e azul
#vetor_cores_ano_dep <- c(
#  "DEPENDENTE - 2021" = "#dc6079",
#  "DEPENDENTE - 2022" = "#dc3859",
#  "DEPENDENTE - 2023" = "#DC143C",
#  "NÃO DEPENDENTE - 2021" = "#99ccff",
#  "NÃO DEPENDENTE - 2022" = "#6699ff",
#  "NÃO DEPENDENTE - 2023" = "#003366"
#)
#
## Gráfico ajustado
#graf_emp_por_estado <- ggplot(emp_por_estado_dep, aes(
#  x = Estado,
#  y = Quantidade,
#  fill = categoria
#)) +
#  geom_col(width = 0.65, position = position_stack(reverse = TRUE)) +
#  geom_text(aes(label = Quantidade),
#            vjust = 0.4, position = position_stack(vjust = 0.5, reverse = TRUE),
#            family = "Source Sans Pro", size = 3, color = "#ffffff") +
#  labs(
#    title = "Distribuição de Empresas por Estado e Dependência",
#    x = "Estado",
#    y = "Quantidade de Empresas",
#    fill = "Ano e Dependência"
#  ) +
#  scale_fill_manual(values = vetor_cores_ano_dep) +
#  theme_minimal() +
#  theme(
#    text = element_text(family = "Lora", colour = "grey20"),
#    axis.text.x = element_text(angle = 45, hjust = 1),
#    legend.position = "top",
#    legend.title = element_text(size = 10, face = "bold"),
#    legend.text = element_text(size = 9)
#  )
#
## Salvar o gráfico
#ggsave(plot = graf_emp_por_estado, "./plots_final/quantidade_empresas_estado_dep.jpeg", width = 10, height = 6)


library(dplyr)
library(ggplot2)

# Filtrar apenas as linhas onde 'dep' é "DEPENDENTE"
dados_filtrados <- dados_empresas %>%
  filter(dep == "DEPENDENTE") %>%
  mutate(
    grupo = ifelse(
      gov_ca == "SIM" & gov_cf == "SIM" & gov_aud == "SIM",
      "Possui todas as\n3 governança",  # Quebra de linha na legenda
      "Não possui todas as\n3 governança"  # Quebra de linha na legenda
    )
  )

# Contar as ocorrências de cada grupo
dados_resumo <- dados_filtrados %>%
  group_by(grupo) %>%
  summarise(frequencia = n(), .groups = "drop") %>%
  mutate(pct = frequencia / sum(frequencia))

# Criar o gráfico de pizza
grafico_pizza <- ggplot(dados_resumo, aes(x = "", y = pct, fill = grupo)) +
  geom_col(
    width = 1,
    color = NA  # Remove as bordas das fatias
  ) +
  coord_polar(theta = "y", start = 0) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Possui todas as\n3 governança" = "#ffba65",
      "Não possui todas as\n3 governança" = "#e67e00"
    ),
    name = ""
  ) +
  labs(
    title = "",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10, lineheight = 1.2)  # Ajuste na quebra da legenda
  )

# Salvar o gráfico como PNG
ggsave(
  plot = grafico_pizza,
  filename = "./plots_final/pizza_dep_possui.png",
  width = 4,
  height = 3,
  dpi = 300,
  type = "cairo-png"
)


# ----------------------------------------------

library(dplyr)
library(ggplot2)

# Filtrar apenas as linhas onde 'dep' é "DEPENDENTE"
dados_filtradoss <- dados_empresas %>%
  filter(dep == "NÃO DEPENDENTE") %>%
  mutate(
    grupo = ifelse(
      gov_ca == "SIM" & gov_cf == "SIM" & gov_aud == "SIM",
      "Possui todas as\n3 governança",  # Quebra de linha na legenda
      "Não possui todas as\n3 governança"  # Quebra de linha na legenda
    )
  )

# Contar as ocorrências de cada grupo
dados_resumo <- dados_filtradoss %>%
  group_by(grupo) %>%
  summarise(frequencia = n(), .groups = "drop") %>%
  mutate(pct = frequencia / sum(frequencia))

# Criar o gráfico de pizza
grafico_pizza <- ggplot(dados_resumo, aes(x = "", y = pct, fill = grupo)) +
  geom_col(
    width = 1,
    color = NA  # Remove as bordas das fatias
  ) +
  coord_polar(theta = "y", start = 0) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Possui todas as\n3 governança" = "#6bb56b",
      "Não possui todas as\n3 governança" = "#006400"
    ),
    name = ""
  ) +
  labs(
    title = "",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10, lineheight = 1.2)  # Ajuste na quebra da legenda
  )

# Salvar o gráfico como PNG
ggsave(
  plot = grafico_pizza,
  filename = "./plots_final/pizza_ndep_possui.png",
  width = 4,
  height = 3,
  dpi = 300,
  type = "cairo-png"
)
