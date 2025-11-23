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

setwd("C:/Users/Lenovo/empresas-estados/v2025") # nolint

tab_uf <- read_excel("./dados/dados-originais/tab_ufs.xlsx") %>%
  select(Estado, Nome_estado, REGIAO)

dados_raw_2023 <- read_excel("./dados/dados-originais/quadro_estatais_2023_v3.xlsx", sheet = "lista definitiva") # nolint
dados_raw_2022 <- read_excel("./dados/dados-originais/quadro_estatais_2022.xlsx", sheet = "Todos") # nolint
dados_raw_2021 <- read_excel("./dados/dados-originais/quadro_estatais_2021.xlsx", sheet = "Todos") # nolint
dados_raw_2024 <- read_excel("./dados/dados-originais/quadro_estatais_2024_v4.xlsx", sheet = "Todos") # nolint


tab_definicoes_setores <- read_excel("./dados/dados-originais/tab_setores.xlsx", sheet = "def") # nolint

dados_selecionados_raw_2024 <- dados_raw_2024 %>%
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
    capital = `Capital Social a Integralizar - Exercício`,
    link      = `Link Carta Anual`,
    indicio_dependencia = `Dependência`,
    cnpj = `CNPJ`
  )

# Remove caracteres especiais
dados_selecionados_raw_2024$emp <- stringr::str_replace_all(dados_selecionados_raw_2024$emp, "[^[:alnum:] ]", "") # nolint


dados_selecionados_raw_2023 <- dados_raw_2023 %>%
  select(
    Estado    = UF,
    emp       = `Estatal`,
    sit       = `Situação`,
    setor     = `Setor`,
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
    var_acoes   = `Variação das Ações`,
    cnpj = `CNPJ`
  )

# Remove caracteres especiais
dados_selecionados_raw_2023$emp <- stringr::str_replace_all(dados_selecionados_raw_2023$emp, "[^[:alnum:] ]", "") # nolint

dados_selecionados_raw_2022 <- dados_raw_2022 %>%
  select(
    Estado    = UF,
    emp       = `Estatal`,
    sit       = `Situação`,
    setor     = `Setor`,
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
    indicio_dependencia = `Indícios de Dependência`,
    cnpj = `CNPJ`
  )

dados_selecionados_raw_2022$emp <- stringr::str_replace_all(dados_selecionados_raw_2022$emp, "[^[:alnum:] ]", "") # nolint

dados_selecionados_raw_2021 <- dados_raw_2021 %>%
  select(
    Estado    = UF,
    emp       = `Estatal`,
    sit       = `Situação`,
    setor     = `Setor`,
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
    link      = `Link Carta Anual`,
    indicio_dependencia = `Indícios de Dependência`,
    cnpj = `CNPJ`
  )

dados_selecionados_raw_2021$emp <- stringr::str_replace_all(dados_selecionados_raw_2021$emp, "[^[:alnum:] ]", "") # nolint


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
                     ifelse(plr_rva %in% nao, "Não", plr_rva))
  ) %>%
  mutate_at(
    .vars = c("PL", "lucros", "desp_investimento", "desp_pessoal", "qde_empregados"), # nolint
    .funs = as.numeric
  ) %>%
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
                     ifelse(plr_rva %in% nao, "Não", plr_rva))
  ) %>%
  mutate_at(
    .vars = c("PL", "lucros", "desp_investimento", "desp_pessoal", "qde_empregados"), # nolint
    .funs = as.numeric
  ) %>%
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
                     ifelse(plr_rva %in% nao, "Não", plr_rva))
  ) %>%
  mutate_at(
    .vars = c("PL", "lucros", "desp_investimento", "desp_pessoal", "qde_empregados"), # nolint
    .funs = as.numeric
  ) %>%
  mutate(result_NA = is.na(Dividendos) & is.na(`Subvenção`) & is.na(`Reforço de Capital`)) %>% # nolint
  mutate_at(.vars = vars("Dividendos", `Subvenção`, `Reforço de Capital`),
            .funs = ~ifelse(is.na(.), 0, .)) %>%
  mutate(`Resultado para o Estado Acionista` = ifelse(result_NA, NA, Dividendos - `Subvenção` - `Reforço de Capital`)) # nolint

# --- Fun auxiliar p/ parse numérico robusto (pt-BR) ---
num_pt <- function(x) readr::parse_number(
  as.character(x),
  locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
)

# --- Versão tratada de 2024 (espelha o que você fez p/ 2021–2023) ---
dados_selecionados2024 <- dados_selecionados_raw_2024 %>%
  left_join(tab_uf, by = "Estado") %>%
  mutate(
    dep     = stringr::str_to_title(dep),
    dep     = ifelse(is.na(dep), "Não Informado", dep),
    gov     = gov_ca %in% sim & gov_cf %in% sim & gov_aud %in% sim,
    plr_rva = ifelse(plr_rva %in% sim, "Sim",
                     ifelse(plr_rva %in% nao, "Não", plr_rva))
  ) %>%
  mutate(across(
    c(PL, lucros, desp_investimento, desp_pessoal, qde_empregados,
      Dividendos, `Subvenção`, `Subvenção (anterior)`,
      `Reforço de Capital`, `Reforço de Capital (anterior)`, capital),
    ~ num_pt(.)
  )) %>%
  mutate(
    result_NA = is.na(Dividendos) & is.na(`Subvenção`) & is.na(`Reforço de Capital`),
    `Resultado para o Estado Acionista` =
      ifelse(result_NA, NA, Dividendos - `Subvenção` - `Reforço de Capital`)
  )

# --- Agora combine SEM conflito de tipos ---
dados_empresas <- dplyr::bind_rows(
  dados_selecionados2021 %>% mutate(Ano = 2021),
  dados_selecionados2022 %>% mutate(Ano = 2022),
  dados_selecionados2023 %>% mutate(Ano = 2023),
  dados_selecionados2024 %>% mutate(Ano = 2024)
)

# ========================================================================
# GRÁFICOS
# ========================================================================

# Evolução do Patrimônio Líquido Total por Ano ----------------------------

pl_por_ano <- dados_empresas %>%
  group_by(Ano) %>%
  summarise(Patrimonio_Liquido = sum(PL, na.rm = TRUE), .groups = "drop")

formatar_valor <- scales::label_number(
  scale_cut = scales::cut_short_scale(),
  big.mark = ".", decimal.mark = ","
)

ggplot(pl_por_ano, aes(x = Ano, y = Patrimonio_Liquido)) +
  geom_line(linewidth = 1, color = "steelblue", linetype = "dashed") +  # <- trocado (era loess)
  geom_point(size = 3, color = "steelblue") +
  geom_text(aes(label = formatar_valor(Patrimonio_Liquido)),
            vjust = -1, size = 6, family = "Source Sans Pro", color = "grey20") +
  scale_y_continuous(labels = NULL, breaks = NULL,
                     expand = expansion(mult = c(0.05, 0.2))) +
  scale_x_continuous(breaks = unique(pl_por_ano$Ano)) +
  labs(title = "", x = "Ano", y = NULL) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lora", color = "grey20"),
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

ggsave("./plots_final/evolucao_patrimonio_liquido.jpeg", width = 10, height = 7, device = "jpeg")


# Situação das Empresas por Ano -------------------------------------------

library(stringr)

situacao_por_ano <- dados_empresas %>%
  group_by(Ano, sit, dep) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(
    sit = if_else(sit == "INATIVO", "EM LIQUIDAÇÃO", sit),
    sit_dep = case_when(
      sit == "ATIVA"         & dep == "NÃO DEPENDENTE" ~ "ATIVA - Não dependente",
      sit == "ATIVA"         & dep == "DEPENDENTE"     ~ "ATIVA - dependente",
      sit == "EM LIQUIDAÇÃO" & dep == "NÃO DEPENDENTE" ~ "EM LIQUIDAÇÃO - Não dependente",
      sit == "EM LIQUIDAÇÃO" & dep == "DEPENDENTE"     ~ "EM LIQUIDAÇÃO - dependente",
      TRUE ~ paste(sit, dep, sep = " - ")
    )
  )

cores_personalizadas <- c(
  "ATIVA - Não dependente"         = "#006400",
  "ATIVA - dependente"             = "#e67e00",
  "EM LIQUIDAÇÃO - Não dependente" = "#6bb56b",
  "EM LIQUIDAÇÃO - dependente"     = "#ffba65"
)

ggplot(situacao_por_ano, aes(x = as.factor(Ano), y = Quantidade, fill = sit_dep)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = Quantidade),
            position = position_stack(vjust = 0.5),
            size = 8, color = "white", fontface = "bold") +
  labs(x = "Ano", y = NULL, fill = "") +
  scale_fill_manual(values = cores_personalizadas) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lora", colour = "grey20"),
    axis.text.x = element_text(size = 30, face = "bold"),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 30, face = "bold"),
    legend.text = element_text(size = 29)
  ) +
  guides(fill = guide_legend(ncol = 1))

ggsave("./plots_final/situacao_empresas.jpeg", width = 15, height = 15)


# Evolução de Lucros/Prejuízos (uma série total) --------------------------

lucros_por_ano <- dados_empresas %>%
  group_by(Ano) %>%
  summarise(Lucro_Total = sum(lucros, na.rm = TRUE), .groups = "drop") %>%
  mutate(Tipo = ifelse(Lucro_Total >= 0, "Lucro", "Prejuízo"))

ggplot() +
  geom_line(data = lucros_por_ano %>% filter(Tipo == "Lucro"),
            aes(x = Ano, y = Lucro_Total, color = "Lucro"),
            linewidth = 1.2) +
  geom_point(data = lucros_por_ano %>% filter(Tipo == "Lucro"),
             aes(x = Ano, y = Lucro_Total), size = 3, color = "#008080") +
  geom_line(data = lucros_por_ano %>% filter(Tipo == "Prejuízo"),
            aes(x = Ano, y = Lucro_Total, color = "Prejuízo"),
            linewidth = 1.2) +
  geom_point(data = lucros_por_ano %>% filter(Tipo == "Prejuízo"),
             aes(x = Ano, y = Lucro_Total), size = 3, color = "#DC143C") +
  geom_text(aes(x = Ano, y = Lucro_Total,
                label = scales::label_number(scale_cut = scales::cut_short_scale())(Lucro_Total),
                color = Tipo),
            data = lucros_por_ano, vjust = -1, size = 4, family = "Source Sans Pro") +
  scale_color_manual(values = c("Lucro" = "#008080", "Prejuízo" = "#DC143C"), name = "Resultado") +
  scale_y_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0.05, 0.2))) +
  scale_x_continuous(breaks = unique(lucros_por_ano$Ano)) +
  labs(title = "Evolução dos Lucros", x = "Ano", y = NULL) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lora", color = "grey20"),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  )

ggsave("./plots_final/evolucao_lucros.jpeg", width = 10, height = 7, device = "jpeg")


# Evolução Lucros x Prejuízos (séries separadas) --------------------------

formatar_valor <- label_number(
  scale_cut   = cut_short_scale(),
  big.mark    = ".",
  decimal.mark= ","
)

lucros_por_ano2 <- dados_empresas %>%
  group_by(Ano) %>%
  summarise(
    Lucro_Total    = sum(lucros[lucros > 0], na.rm = TRUE),
    Prejuizo_Total = sum(lucros[lucros < 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c("Lucro_Total", "Prejuizo_Total"),
               names_to = "Tipo", values_to = "Valor") %>%
  mutate(Tipo = ifelse(Tipo == "Lucro_Total", "Lucro", "Prejuízo"))

ggplot(lucros_por_ano2, aes(x = Ano, y = Valor, color = Tipo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = formatar_valor(Valor)),
            vjust = -1, size  = 7, family = "Source Sans Pro") +
  scale_color_manual(values = c("Lucro" = "#008080", "Prejuízo" = "#DC143C"), name = "Resultado") +
  scale_y_continuous(labels = formatar_valor, breaks = scales::pretty_breaks(n = 8),
                     expand = expansion(mult = c(0.05, 0.2))) +
  scale_x_continuous(breaks = unique(lucros_por_ano2$Ano)) +
  labs(title = "", x = "Ano", y = "Valores (em milhões)") +
  theme_minimal() +
  theme(
    text = element_text(family = "Lora", color = "grey20"),
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 16)
  )

ggsave("./plots_final/evolucao_lucros_prejuizos.jpeg", width = 10, height = 7, device = "jpeg")


# Distribuição do PL por Ano (criar pl_distribuicao e plotar) --------------

pl_distribuicao <- dados_empresas %>%
  mutate(plr_rva = case_when(
    plr_rva %in% c("SIM","Sim","CONTROLE INTERNO","Possui") ~ "Distribuído",
    plr_rva %in% c("NÃO","Não","NAO","Não Possui","Não possui") ~ "Não Distribuído",
    TRUE ~ "Não Distribuído"
  )) %>%
  group_by(Ano, plr_rva) %>%
  summarise(Total_PL = sum(PL, na.rm = TRUE), .groups = "drop") %>%
  group_by(Ano) %>%
  mutate(pct = if (sum(Total_PL, na.rm = TRUE) > 0) Total_PL / sum(Total_PL) else 0) %>%
  ungroup()

ggplot(pl_distribuicao, aes(x = "", y = pct, fill = plr_rva)) +
  geom_col(width = 1, color = NA, linewidth = 0, linejoin = "round") +
  coord_polar("y", start = 0, clip = "off") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~ Ano, ncol = 3) +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 15, color = "white") +
  scale_fill_manual(values = c("Distribuído" = "#735D36", "Não Distribuído" = "#F4C773"),
                    name = "PLR/RVA") +
  labs(title = "", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    legend.position = "bottom",
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 26, face = "bold"),
    strip.text = element_text(size = 30, face = "bold")
  )

ggsave("./plots_final/distribuicao_PL.jpeg", width = 15, height = 10)


# Quantidade de empresas por estado, ano ----------------------------------

dados_dependente <- dados_empresas

emp_por_estado <- dados_dependente %>%
  group_by(Estado, Ano) %>%
  summarise(Quantidade = n(), .groups = "drop")

ggplot(emp_por_estado, aes(x = reorder(Estado, -Quantidade), y = Quantidade, fill = as.factor(Ano))) + # nolint
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Estado", y = "Quantidade de Empresas", fill = "Ano") +
  scale_fill_manual(values = c("2021" = "#f2ac29", "2022" = "#718c35", "2023" = "#009ADE")) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lora", colour = "grey20"),
    axis.text.x = element_text(angle = 45, hjust = 1, size=25),
    legend.position = "top",
    legend.title = element_text(size = 30, face = "bold"),
    legend.text = element_text(size = 29)
  )

ggsave("./plots_final/quantidade_empresas_estado.jpeg", width = 15, height = 10)

# Pizza (DEPENDENTE) governança -------------------------------------------

dados_filtrados <- dados_empresas %>%
  filter(stringr::str_to_upper(dep) == "DEPENDENTE") %>%   # << aqui
  mutate(
    grupo = ifelse(
      (gov_ca %in% sim) & (gov_cf %in% sim) & (gov_aud %in% sim),  # << e aqui
      "Possui todas as\n3 governança",
      "Não possui todas as\n3 governança"
    )
  )

dados_resumo <- dados_filtrados %>%
  count(grupo, name = "frequencia") %>%
  mutate(pct = if (sum(frequencia) > 0) frequencia / sum(frequencia) else 0)

grafico_pizza <- ggplot(dados_resumo, aes(x = "", y = pct, fill = grupo)) +
  geom_col(width = 1, color = NA) +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4) +
  scale_fill_manual(values = c(
    "Possui todas as\n3 governança" = "#ffba65",
    "Não possui todas as\n3 governança" = "#e67e00"
  ), name = "") +
  labs(title = "", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10, lineheight = 1.2)
  )

dir.create("./plots_final", showWarnings = FALSE, recursive = TRUE)
ggsave(plot = grafico_pizza, filename = "./plots_final/pizza_dep_possui.png",
       width = 4, height = 3, dpi = 300, device = "png")  # ragg ignora 'type'


# Pizza (NÃO DEPENDENTE) governança ---------------------------------------

dados_filtradoss <- dados_empresas %>%
  filter(stringr::str_to_upper(dep) == "NÃO DEPENDENTE") %>%   # << aqui
  mutate(
    grupo = ifelse(
      (gov_ca %in% sim) & (gov_cf %in% sim) & (gov_aud %in% sim),  # << e aqui
      "Possui todas as\n3 governança",
      "Não possui todas as\n3 governança"
    )
  )

dados_resumo <- dados_filtradoss %>%
  count(grupo, name = "frequencia") %>%
  mutate(pct = if (sum(frequencia) > 0) frequencia / sum(frequencia) else 0)

grafico_pizza2 <- ggplot(dados_resumo, aes(x = "", y = pct, fill = grupo)) +
  geom_col(width = 1, color = NA) +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4) +
  scale_fill_manual(values = c(
    "Possui todas as\n3 governança" = "#6bb56b",
    "Não possui todas as\n3 governança" = "#006400"
  ), name = "") +
  labs(title = "", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10, lineheight = 1.2)
  )

dir.create("./plots_final", showWarnings = FALSE, recursive = TRUE)
ggsave(plot = grafico_pizza2, filename = "./plots_final/pizza_ndep_possui.png",
       width = 4, height = 3, dpi = 300, device = "png")  # ragg ignora 'type'




# ------------------------------------------------------------
# CNPJs — Incluídos x Excluídos (Plotly compacto, pronto p/ embutir)
# ------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(plotly)
  library(htmlwidgets)
  library(htmltools)
})

# --- Helpers -------------------------------------------------
cnpj_clean <- function(x) str_replace_all(as.character(x), "\\D", "")
format_cnpj <- function(x){
  x <- str_pad(x, 14, pad = "0")
  paste0(substr(x,1,2), ".", substr(x,3,5), ".", substr(x,6,8),
         "/", substr(x,9,12), "-", substr(x,13,14))
}
get_cnpj_col <- function(df){
  nm <- intersect(names(df), c("cnpj","CNPJ","Cnpj","CNPJ_FMT","CNPJ_fmt"))
  if (length(nm) == 0) stop("Não encontrei coluna 'cnpj' em um dos data.frames.")
  nm[1]
}

# --- Dados de entrada (precisam existir no ambiente) --------
stopifnot(exists("dados_selecionados_raw_2023"),
          exists("dados_selecionados_raw_2022"))

c23 <- get_cnpj_col(dados_selecionados_raw_2023)
c22 <- get_cnpj_col(dados_selecionados_raw_2022)

k23 <- dados_selecionados_raw_2023 %>%
  transmute(CNPJ = cnpj_clean(.data[[c23]])) %>%
  filter(!is.na(CNPJ), CNPJ != "", nchar(CNPJ) >= 14) %>%
  distinct(CNPJ)

k22 <- dados_selecionados_raw_2022 %>%
  transmute(CNPJ = cnpj_clean(.data[[c22]])) %>%
  filter(!is.na(CNPJ), CNPJ != "", nchar(CNPJ) >= 14) %>%
  distinct(CNPJ)

inc <- anti_join(k23, k22, by = "CNPJ") %>%
  mutate(CNPJ_fmt = format_cnpj(CNPJ)) %>% arrange(CNPJ_fmt)
exc <- anti_join(k22, k23, by = "CNPJ") %>%
  mutate(CNPJ_fmt = format_cnpj(CNPJ)) %>% arrange(CNPJ_fmt)

if (nrow(inc) == 0) inc <- tibble(CNPJ_fmt = "(nenhum)")
if (nrow(exc) == 0) exc <- tibble(CNPJ_fmt = "(nenhum)")

# --- Posições (duas colunas MUITO próximas) -----------------
inc$y <- rev(seq_len(nrow(inc)))
exc$y <- rev(seq_len(nrow(exc)))
y_max <- max(nrow(inc), nrow(exc))
x_left  <- 0.48
x_right <- 0.52

# --- Plotly (sem ggplotly, sem width/height em layout) ------
p <- plot_ly(height = 420)

p <- add_trace(
  p, type = "scatter", mode = "text",
  x = rep(x_left,  nrow(inc)), y = inc$y,
  text = inc$CNPJ_fmt, textposition = "middle right",
  textfont = list(size = 12),
  hovertemplate = "<b>Incluído</b><br>CNPJ: %{text}<extra></extra>",
  name = sprintf("Incluídos (%d)", nrow(inc)), showlegend = TRUE
)

p <- add_trace(
  p, type = "scatter", mode = "text",
  x = rep(x_right, nrow(exc)), y = exc$y,
  text = exc$CNPJ_fmt, textposition = "middle left",
  textfont = list(size = 12),
  hovertemplate = "<b>Excluído</b><br>CNPJ: %{text}<extra></extra>",
  name = sprintf("Excluídos (%d)", nrow(exc)), showlegend = TRUE
)

p <- layout(
  p,
  title = list(
    text = sprintf("CNPJs — Incluídos (%d) × Excluídos (%d)", nrow(inc), nrow(exc)),
    x = 0.5, font = list(size = 14)
  ),
  xaxis = list(visible = FALSE, range = c(0.40, 0.60), fixedrange = TRUE),
  yaxis = list(visible = FALSE, range = c(0.5, y_max + 0.5), fixedrange = TRUE),
  shapes = list(list(
    type = "line", x0 = 0.50, x1 = 0.50, y0 = 0, y1 = y_max + 1,
    line = list(width = 1, color = "rgba(0,0,0,0.15)")
  )),
  margin = list(l = 6, r = 6, t = 34, b = 6),
  legend = list(orientation = "h", x = 0, y = -0.06, font = list(size = 11))
)

# --- Salvar HTML (sem selfcontained p/ não exigir pandoc) ----
dir.create("plots_final", showWarnings = FALSE, recursive = TRUE)
out_html <- file.path("plots_final", "cnpjs_incluidos_excluidos.html")
saveWidget(p, out_html, selfcontained = FALSE,
           libdir = file.path("plots_final","libs"),
           background = "transparent", title = NULL)

message("OK! Arquivo gerado em: ", normalizePath(out_html))

# -------------------------------------------------------------------------
# "Tabela" ggplot: CNPJs Incluídos (2023) x Excluídos (2022)
# -------------------------------------------------------------------------

cnpj_clean <- function(x) stringr::str_replace_all(as.character(x), "\\D", "")
format_cnpj <- function(x) {
  x <- stringr::str_pad(x, 14, pad = "0")
  paste0(substr(x,1,2), ".", substr(x,3,5), ".", substr(x,6,8), "/", substr(x,9,12), "-", substr(x,13,14))
}

k23 <- dados_selecionados_raw_2024 %>%
  transmute(CNPJ = cnpj_clean(cnpj)) %>%
  filter(!is.na(CNPJ), CNPJ != "", stringr::str_length(CNPJ) >= 14) %>%
  distinct(CNPJ, .keep_all = TRUE)

k22 <- dados_selecionados_raw_2023 %>%
  transmute(CNPJ = cnpj_clean(cnpj)) %>%
  filter(!is.na(CNPJ), CNPJ != "", stringr::str_length(CNPJ) >= 14) %>%
  distinct(CNPJ, .keep_all = TRUE)

inc <- anti_join(k23, k22, by = "CNPJ") %>%
  mutate(Status = "Incluídos (novos em 2024)") %>%
  mutate(CNPJ_fmt = format_cnpj(CNPJ)) %>%
  arrange(CNPJ_fmt) %>%
  mutate(linha = row_number())

exc <- anti_join(k22, k23, by = "CNPJ") %>%
  mutate(Status = "Excluídos (não estão em 2024)") %>%
  mutate(CNPJ_fmt = format_cnpj(CNPJ)) %>%
  arrange(CNPJ_fmt) %>%
  mutate(linha = row_number())

if (nrow(inc) == 0) inc <- tibble::tibble(CNPJ_fmt = "(nenhum)", Status = "Incluídos (novos em 2024)", linha = 1)
if (nrow(exc) == 0) exc <- tibble::tibble(CNPJ_fmt = "(nenhum)", Status = "Excluídos (não estão em 2024)", linha = 1)

tab <- bind_rows(inc, exc) %>%
  mutate(Status = factor(Status, levels = c("Incluídos (novos em 2024)",
                                            "Excluídos (não estão em 2024)")))

p_tab <- ggplot(tab, aes(x = Status, y = linha)) +
  geom_tile(aes(fill = Status), width = 0.96, height = 0.96, alpha = 0.08, show.legend = FALSE) +
  geom_text(aes(label = CNPJ_fmt), family = "Source Sans Pro", size = 5, color = "grey20") +
  scale_y_reverse(expand = expansion(mult = c(0.01, 0.03))) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = c(
    "Incluídos (novos em 2024)"     = "#009ADE",
    "Excluídos (não estão em 2024)" = "#DC143C"
  )) +
  labs(title = "", x = NULL, y = NULL) +
  tema() +
  theme(
    axis.text.x = element_text(size = 18, face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks  = element_blank(),
    panel.grid  = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

p_tab

.alt <- max(max(inc$linha), max(exc$linha))
.alt <- max(6, .alt * 0.30)
ggsave("./plots_final/tabela_cnpjs_incluidos_excluidos.png",
       p_tab, width = 12, height = .alt, dpi = 300)