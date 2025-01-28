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

setwd("/Users/gustavo.silva/OneDrive - Tesouro Nacional/Área de Trabalho/projetos/RAIO_X_EMPRESAS_DOS_ESTADOS/2023") # nolint

tab_uf <- read_excel("./dados/dados-originais/tab_ufs.xlsx") %>%
  select(Estado, Nome_estado, REGIAO)

dados_raw <- read_excel("./dados/dados-originais/quadro_estatais_v3.xlsx", sheet = "lista definitiva") # nolint

tab_definicoes_setores <- read_excel("./dados/dados-originais/tab_setores.xlsx", sheet = "def") # nolint

dados_selecionados_raw <- dados_raw %>%
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
dados_selecionados_raw$emp <- str_replace_all(dados_selecionados_raw$emp, "[^[:alnum:] ]", "") # nolint


# limpeza -----------------------------------------------------------------

sim <- c("SIM", "Sim", "CONTROLE INTERNO", "Possui", "DEPENDENTE")
nao <- c("NÃO", "Não", "Não Possui", "Não possui", "NAO", "NÂO", "NÃO DEPENDENTE") # nolint

dados_selecionados <- dados_selecionados_raw %>%
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


# mapa small multiples ----------------------------------------------------

mapa <- readRDS("./dados/dados-intermediarios/mapa.rds")
mapa <- st_simplify(mapa, dTolerance = .0001)


# exporta dados para JS ---------------------------------------------------

# Geração de cores para os setores
tab_definicoes_setores$cores <- viridis::plasma(nrow(tab_definicoes_setores), direction = 1) # nolint

# Exportação da lista de setores
write.csv(tab_definicoes_setores, file = "./dados/lista-setores.csv", fileEncoding = "UTF-8") # nolint

# Mapa
dados_qde_setor_estado <- dados_selecionados %>%
  count(setor, Estado)

# Primeiro termo do setor
primeiro_termo_setor <- str_split(
  unique(dados_selecionados$setor), # nolint
  pattern = " ",
  simplify = TRUE)[, 1] %>%
  str_replace_all(pattern = "[^a-zA-Z ]", replacement = "") # para ficar igual ao JS # nolint

# Todos os setores por estado
todos_setores_estados <- full_join(
  data.frame(setor = unique(dados_selecionados$setor),
             cod_setor = primeiro_termo_setor),
  data.frame(Estado = unique(tab_uf$Estado)),
  by = character()
)

# Transformação dos dados do mapa
dados_setor_estados_mapa <- todos_setores_estados %>%
  left_join(dados_qde_setor_estado) %>%
  mutate(tem_empresa = ifelse(is.na(n), 0, ifelse(n > 0, 1, 0))) %>%
  group_by(Estado, cod_setor) %>% # Agrupa para lidar com duplicatas
  summarise(tem_empresa = max(tem_empresa), .groups = "drop") %>% # Resolve duplicatas
  pivot_wider(names_from = cod_setor, values_from = tem_empresa, values_fill = 0) # Substitui spread

# Adiciona os dados de quantidade ao mapa
mapa_qde_export <- mapa %>%
  rename(Estado = abbrev_state) %>%
  left_join(dados_setor_estados_mapa)

# Exportação do mapa em formato GeoJSON
write_file(
  geojsonsf::sf_geojson(mapa_qde_export), #, digits = 5),  # nolint
  "./dados/mapa-setores.geojson"
)

###################################################################################################################

# plot mapa small multiples -----------------------------------------------

setores <- data.frame(
  setor = unique(dados_selecionados$setor)
)

mapa_qde <- mapa %>%
  rename(Estado = "abbrev_state") %>%
  inner_join(dados_qde_setor_estado) %>%
  rename(qde = "n") %>%
  arrange(setor) %>%
  mutate(setor = str_wrap(setor, width = 20))

graf_mapa_comp <- ggplot(mapa_qde) +
  geom_sf(data = mapa, fill = "#EFEFEF", color = "ghostwhite") +
  geom_sf(aes(group = Estado, fill = ifelse(qde > 0, setor, NA)), color = "ghostwhite") + # nolint
  scale_fill_viridis_d(direction = 1,
                       option = "plasma", na.value = "#EFEFEF") +
  tema() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Source Sans Pro"),
        legend.position = "none",
        legend.text = element_text(size = 10),
        plot.background = element_blank(),
        panel.background = element_blank())

graf_mapa_facet <- graf_mapa_comp + facet_wrap(~setor)#, labeller = setor_labeller) # nolint
ggsave(plot = graf_mapa_facet, "./plots_final/segmentos_facet2.png", width = 9, height = 8, dpi = 300) # windows: acrescentar: , type = "cairo-png" # nolint

###################################################################################################################

# barchart - quantidades --------------------------------------------------

qde_empresas_seg <- dados_selecionados %>%
  group_by(setor, dep) %>%
  summarise(qde = n()) %>%
  ungroup() %>%
  group_by(setor) %>%
  mutate(qde_tot = sum(qde),
         dep = factor(dep, levels = c("Dependente", "Não Dependente", "Não Informado"))) %>% # nolint
  filter(!is.na(setor))

vetor_anos_2020_2021 <-
  c("2020" = "#50A315",
    "2021" = "#009ADE")

vetor_cores_dep <- c("Dependente" = "#f2ac29",
                     "Não Dependente" = "#718c35",
                     "Não Informado" = "#5c4b51")

graf_qde_emp <-
  ggplot(qde_empresas_seg, aes(x = reorder(setor, qde_tot), y = qde, fill = dep)) + # nolint
  geom_col(width = 0.65, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = qde, y = qde),
            vjust = 0.4, position = position_stack(vjust = 0.5, reverse = TRUE),
            family = "Source Sans Pro", size = 3, color = "#ebf2f2") +
  geom_text(aes(label = qde_tot), y = -1,
            vjust = 0.4, check_overlap = TRUE,
            family = "Source Sans Pro", size = 3.5, color = "grey") +
  coord_flip() +
  scale_fill_manual(values = vetor_cores_dep) +
  #scale_fill_viridis_d() +
  #scale_color_viridis_d() +
  labs(x = NULL, y = NULL,
       title = NULL, #"Quantidade de empresas por segmento",
       fill = NULL) +
  tema_barra() + theme(axis.text = element_text(size = 8))

ggsave(plot = graf_qde_emp, "./plots_final/qde_seg.png", h = 4.5, w = 5.5)#, type = "cairo-png") # nolint

#-------------------------------------

qde_empresas_seg <- dados_selecionados %>%
  group_by(setor) %>%
  summarise(qde_tot = n()) %>%
  ungroup() %>%
  filter(!is.na(setor))

graf_qde_emp <- 
  ggplot(qde_empresas_seg, aes(x = reorder(setor, qde_tot), y = qde_tot)) +
  geom_col(width = 0.65, fill = "#5c4b51") +  # Define uma única cor para as barras
  geom_text(aes(label = qde_tot), 
            vjust = 0.4, hjust = 1.1,
            family = "Source Sans Pro", size = 3.5, color = "grey") +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = NULL) +
  tema_barra() + 
  theme(axis.text = element_text(size = 8))

ggsave(plot = graf_qde_emp, "./plots_final/qde_seg_total.png", h = 4.5, w = 5.5)

###################################################################################################################

qde_empresas_est <- dados_selecionados %>%
  group_by(Nome_estado, dep) %>%
  summarise(qde = n()) %>%
  ungroup() %>%
  group_by(Nome_estado) %>%
  mutate(qde_tot = sum(qde),
         dep = factor(dep, levels = c("Dependente", "Não Dependente", "Não Informado"))) %>% # nolint
  filter(!is.na(dep))

graf_qde_emp_est <-
  ggplot(qde_empresas_est, aes(x = reorder(Nome_estado, qde_tot), y = qde, fill = dep)) + # nolint
  geom_col(width = 0.65, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = qde, y = qde),
            vjust = 0.4, position = position_stack(vjust = 0.5, reverse = TRUE),
            family = "Source Sans Pro", size = 3, color = "#ebf2f2") +
  geom_text(aes(label = qde_tot), y = -.6,
            vjust = 0.4, check_overlap = TRUE,
            family = "Source Sans Pro", size = 3.5, color = "grey") +
  coord_flip() +
  scale_fill_manual(values = vetor_cores_dep) +
  labs(x = NULL, y = NULL,
       title = NULL, #"Quantidade de empresas por Estado",
       fill = NULL) +
  tema_barra() + theme(axis.text = element_text(size = 9))

ggsave(plot = graf_qde_emp_est, "./plots_final/qde_est.png", h = 6.5, w = 5)

#----------------------------

qde_empresas_est <- dados_selecionados %>%
  group_by(Nome_estado) %>%
  summarise(qde_tot = n()) %>%
  ungroup() %>%
  filter(!is.na(Nome_estado))

graf_qde_emp_est <- 
  ggplot(qde_empresas_est, aes(x = reorder(Nome_estado, qde_tot), y = qde_tot)) +
  geom_col(width = 0.65, fill = "#5c4b51") +  # Cor fixa para as barras
  geom_text(aes(label = qde_tot),
            vjust = 0.4, hjust = 1.1,
            family = "Source Sans Pro", size = 3.5, color = "grey") +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = NULL) +
  tema_barra() + 
  theme(axis.text = element_text(size = 9))

ggsave(plot = graf_qde_emp_est, "./plots_final/qde_est_total.png", h = 6.5, w = 5)

##############################################################################################################

# ROE - beeswarm ----------------------------------------------------------

summary(dados_selecionados$PL)
length(which(dados_selecionados$PL==0)) # nolint
length(which(dados_selecionados$PL<0)) # nolint
length(which(is.na(dados_selecionados$PL)))
length(which(is.na(dados_selecionados$lucros)))

## importante
qde_emp_fora_roe <- length(which(
  is.na(dados_selecionados$lucros) |  # nolint
    dados_selecionados$lucros ==0 | # nolint
    dados_selecionados$PL<=0 |  # nolint
    is.na(dados_selecionados$PL)))


fab <-
  dados_selecionados %>%
  filter(is.na(lucros)|PL<=0|is.na(PL)) # nolint

fab%>% # nolint
  arrange(emp) %>%
  write.csv2("empresas_excluidas_rentabilidade.csv")

top_setores <- dados_qde_setor_estado %>%
  group_by(setor) %>%
  summarise(qde = sum(n)) %>%
  arrange(desc(qde)) %>%
  filter(qde >= 10 & setor != "OUTRO")

principais_setores <- top_setores$setor

dados_roe <- dados_selecionados %>%
  filter(PL > 0, lucros!=0) %>% # nolint
  mutate(ROE = lucros / PL,
         PL_formatado = format(PL, big.mark = ".", decimal.mark = ',', scientific = FALSE)) %>% # nolint
  filter(!is.na(ROE)) %>%
  mutate(Empresa = paste0(emp, ' (', Estado, ')\n', # nolint
                          'Dependência: ', dep, '\n', # nolint
                          'Possui todas as estruturas de Governança? ', ifelse(gov, "Sim", "Não"), '\n', # nolint
                          'Setor: ', setor, '\n', # nolint
                          'PL: R$ ', PL_formatado, '\n', # nolint
                          'Lucros / Prejuízos no ano: R$ ',  # nolint
                          format(lucros, big.mark = '.', decimal.mark = ","), '\n', # nolint
                          'ROE: ', percent(round(ROE,4))), # nolint
         cat_ROE = cut(ROE,
                       breaks = c(-Inf, -0.5, 0, 0.5, Inf),
                       labels = c("bem_neg", "neg", "pos", "bem_pos")),
         setores_principais = ifelse(setor %in% principais_setores, setor, "Demais"), # nolint
         sinal_ROE = ifelse(ROE>0, "Positivo", "Negativo")) # nolint

summary(dados_roe$ROE)[c("Min.", "Max.")]

seq(summary(dados_roe$ROE)[c("Min.")],
    summary(dados_roe$ROE)[c("Max.")],
    by = 0.5)


define_breaks <- function(limits) {
  seq(round(limits[1],0), round(limits[2],0), by = 0.5) # nolint
}

cor_anotacoes <- "#3b7302"

cores_escala <- c("bem_neg" = "#912849",
                  "neg"     = "#ff7270",
                  "pos"     = "#91c1cc",
                  "bem_pos" = "#375e8b")

sumario_roe <- dados_roe %>%
  group_by(cat_ROE, dep) %>%
  summarise(qde = n()) %>%
  group_by(dep) %>%
  mutate(pct_qde = percent(qde/sum(qde))) %>% # nolint
  ungroup() %>%
  mutate(y = case_when(cat_ROE == "bem_neg" ~ -0.75,
                       cat_ROE == "neg" ~ -0.25,
                       cat_ROE == "pos" ~  0.25,
                       cat_ROE == "bem_pos" ~  0.75))

sumario_roe_sinal <- dados_roe %>%
  group_by(sinal_ROE, dep) %>%
  summarise(qde = n()) %>%
  group_by(dep) %>%
  mutate(pct_qde = percent(qde/sum(qde))) %>% # nolint
  ungroup() %>%
  mutate(y = ifelse(sinal_ROE == "Positivo", 0.5, -0.5))

# empresas fora do limte
dados_roe %>% filter(ROE > 2 | ROE < -2) %>% select(emp, Estado, dep, ROE)

roe2 <- ggplot(dados_roe %>% filter(!is.na(ROE), ROE != 0, PL > 0, ROE >= -1, ROE <= 1.5, !is.na(lucros), lucros != 0), aes(y = ROE, color = sinal_ROE, x = dep,  # nolint
                                               label = Empresa)) + # nolint
  geom_quasirandom() + #beeswarm() + #aes(size = PL),
  scale_color_manual(values = c("Negativo" = "#DC143C",
                                "Positivo" = "#008080")) +
  annotate("rect", xmin = 0, xmax = 1.5, ymin = 0, ymax = 2, alpha = 0.2, fill = "antiquewhite") + # nolint
  annotate("rect", xmin = 1.5, xmax = 2.7, ymin = 0, ymax = 2, alpha = 0.2, fill = "antiquewhite") + # nolint
  geom_text(data = sumario_roe_sinal,
            aes(y = ifelse(dep == "Dependente", y, NA),
                label = paste0(pct_qde, ' das \nDependentes'), # nolint
                color = sinal_ROE),
            x = 0.8, # 0.8 para estático
            hjust = "right", vjust = "center", family = "Source Sans Pro",
            size = 3.5) +
  geom_text(data = sumario_roe_sinal,
            aes(y = ifelse(dep == "Não Dependente", y, NA),
                label = paste0(pct_qde, ' das não\nDependentes'), # nolint
                color = sinal_ROE),
            x = 2.2, # 2.4 para estático
            hjust = "left", vjust = "center", family = "Source Sans Pro",
            size = 3.5) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_y_continuous(labels = percent,
                     breaks = define_breaks,
                     limits = c(-2,2)) + #, # nolint
  tema()

ggsave(plot = roe2, "./plots_final/roe2.png", h = 6.5, w = 6.5)

#para texto do gráfico
roe_acima_200pct <- length(which(dados_roe$ROE > 2))
roe_abaixo_200pct_neg <- length(which(dados_roe$ROE < -2))

qde_emp_fora_roe <- length(which(
  is.na(dados_selecionados$lucros) |  # nolint
    dados_selecionados$lucros ==0 | # nolint
    dados_selecionados$PL<=0 |  # nolint
    is.na(dados_selecionados$PL)))

##############################################################################################################

# ROE - dotplot -----------------------------------------------------------

emp_distorcao <- dados_roe %>%
  filter(setor == "OUTROS" & abs(ROE) >= 4) %>%
  select(emp, ROE) %>%
  select(emp) %>%
  unlist()

dados_qde_setor_dep <- dados_roe %>%
  filter(dep != "Não Informado") %>%
  count(setor, dep)

sum(dados_qde_setor_dep$n)

dados_roe_agreg <- dados_roe %>%
  filter(dep != "Não Informado", abs(ROE) < 50) %>%
  #filter(!(emp %in% emp_distorcao)) %>% #(1)
  group_by(setor, dep) %>%
  summarise(media_ROE = mean(ROE),
            soma_lucro = sum(lucros),
            soma_PL    = sum(PL),
            ROE_medio = sum(lucros)/sum(PL)) %>% # nolint
  ungroup() %>%
  filter(ROE_medio > 0) %>% # Removendo valores menores ou iguais a 0
  select(setor, dep, ROE_medio) %>%
  spread(dep, ROE_medio) %>%
  mutate(maior = ifelse(Dependente > `Não Dependente`, "Dependente", "Não Dependente")) %>% # nolint
  rowwise() %>%
  mutate(maximo = max(Dependente, `Não Dependente`, na.rm = T), # nolint
         fora_escala = maximo > 1 | min(Dependente, `Não Dependente`, na.rm = T) < -1 ) %>% # nolint
  gather(Dependente, `Não Dependente`, key = dep, value = ROE_medio) %>%
  arrange(desc(maximo)) %>%
  left_join(dados_qde_setor_dep) %>%
  mutate(setor = ifelse(fora_escala, paste0(setor, "*"), setor))


roe_dotplot <- ggplot(dados_roe_agreg, aes(y = reorder(setor, maximo),
                                           color = dep, x = ifelse(ROE_medio < -.75, -.75, ROE_medio), group = setor)) + # nolint
  geom_path(color = "lightgrey", size = 1.3, aes(linetype = ifelse(fora_escala, "solid", "dotted"))) + # nolint
  geom_point(aes(size = n)) +
  geom_text(aes(label = ifelse(dep == maior | is.na(maior),
                               percent(ROE_medio, accuracy = 1), NA),
                color = dep), fontface = "bold", size = 3,
            family = "Source Sans Pro",
            nudge_x = 0.16) +
  geom_text(aes(label = ifelse(dep == maior, NA, percent(ROE_medio, accuracy = 1)),  # nolint
                color = dep),  size = 3,
            family = "Source Sans Pro",
            nudge_x = -0.14) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(labels = percent, expand = expansion(mult = .1)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_color_manual(values = vetor_cores_dep) +
  scale_fill_manual(values = vetor_cores_dep) +
  tema_barra()

ggsave(plot = roe_dotplot, "./plots_final/roe_dotplot.png", h = 6, w = 5.8)

dados_roe %>%
  filter(dep == "Não Informado" | abs(ROE) >= 50) %>%
  select(emp, ROE)

dados_roe %>%
  filter(emp == "COMPANHIA DE DESENVOLVIMENTO AGRICOLA DE SAO PAULO - CODASP - EM LIQUIDACAO") # nolint

###############################################################################################################
# ROE - plotly ------------------------------------------------------------

# Filtrar os dados para os limites de 5 bilhões no eixo x e 1 bilhão no eixo y
dados_roe_filtrado <- dados_roe #%>%
  #filter(PL <= 5e9, lucros <= 1e9)

# Gráfico ajustado com os novos limites
# Gráfico ajustado com intervalos de 5 em 5 no eixo X e Y
roe_plotly <- plot_ly() %>%
  add_markers(
    data = dados_roe_filtrado, # Dados já filtrados
    y = ~lucros,
    x = ~PL,
    text = ~Empresa,
    color = ~dep,
    hoverinfo = "text",
    alpha = 0.75,
    marker = list(size = 7),
    colors = vetor_cores_dep
  ) %>%
  layout(
    yaxis = list(
      title = "Lucros / Prejuízos (R$)",
      #dtick = 1e9, # Intervalo de 5 bilhão no eixo Y
      range = c(-2e9, 5e9) # Limites de -6 a 1 bilhão
    ),
    xaxis = list(
      title = "Patrimônio Líquido (R$)",
      #dtick = 5e9, # Intervalo de 5 bilhão no eixo X
      range = c(0, 24e9) # Limites de 0 a 24 bilhões
    ),
    font = list(family = "Source Sans Pro"),
    hoverlabel = list(font = list(family = "Source Sans Pro")),
    legend = list(orientation = 'h', x = 0, y = 1.3), # Legenda horizontal no topo
    width = 600,  # Largura do gráfico
    height = 400  # Altura do gráfico
  ) %>%
  config(displayModeBar = FALSE)

# Salvar o gráfico como arquivo HTML
htmlwidgets::saveWidget(partial_bundle(roe_plotly), file = "./plots_final/roe.html")


################################################################################################################

# Lucro / Prejuízo --------------------------------------------------------

dados_lucro_preju <- dados_selecionados %>%
  filter(!is.na(lucros)) %>%
  mutate(
    ROE = lucros / PL, # nolint
    PL_formatado = format(PL, big.mark = ".", decimal.mark = ',', scientific = FALSE)) %>% # nolint
  mutate(Empresa = paste0(emp, ' (', Estado, ')\n', # nolint
                          'Dependência: ', dep, '\n', # nolint
                          'Setor: ', setor, '\n', # nolint
                          'PL: R$ ', PL_formatado, '\n', # nolint
                          'Lucros / Prejuízos no ano: R$ ', format(lucros, big.mark = '.', decimal.mark = ","), '\n', # nolint
                          'ROE: ', ifelse(is.na(ROE), 'Não disponível', percent(round(ROE,4)))), # nolint
         setores_principais = ifelse(setor %in% principais_setores, setor, "Demais")) # nolint

qde_NAs_lucro <- length(which(is.na(dados_selecionados$lucros) == TRUE)) # nolint

length(which(dados_lucro_preju$lucros<=-50e6 | dados_lucro_preju$lucros>=50e6)) # nolint
summary(dados_lucro_preju$lucros)
length(which(dados_lucro_preju$`Resultado para o Estado Acionista`<=0)) # nolint

ggplot(dados_lucro_preju %>% filter(dep != "Não Informado"), aes(y = lucros, color = lucros>0, x = dep, label = Empresa)) + # nolint
  geom_quasirandom() +
  scale_y_continuous(limits = c(-2.5e8, 50e6),
                     labels = function(x){format(round(x/1e6, 1), big.mark = ".", # nolint
                                                 decimal.mark = ',')}) +  # nolint
  labs(#title = "Distribuição do ROE das empresas do estados",  # nolint
    x = NULL, y = NULL) + #, # nolint
  #subtitle = "Mais de 60% das dependentes têm ROE negativo, mais de 60% das não dependentes têm ROE positivo") + # nolint
  tema()

# grafico barras

sumario_lucro <- dados_selecionados %>%
  mutate(result_pos = ifelse(lucros >= 0, "Positivo", "Negativo")) %>%
  group_by(dep, result_pos) %>%
  summarise(qde = n()) %>%
  ungroup() %>%
  group_by(dep) %>%
  mutate(tot_por_dep = sum(qde),
         percent_dep = percent(qde / tot_por_dep))

sumario_lucro_total <- sumario_lucro %>%
  group_by(result_pos) %>%
  summarise(dep = "Total",
            qde = sum(qde),
            tot_por_dep = sum(qde)) %>%
  ungroup() %>%
  group_by(dep) %>%
  mutate(tot_por_dep = sum(qde),
         percent_dep = percent(qde / tot_por_dep)) %>%
  ungroup() %>%
  bind_rows(sumario_lucro)

graf_barra_lucro <- ggplot(sumario_lucro_total, aes(x = dep, y = qde, fill = result_pos)) + # nolint
  geom_col(position = "fill", width = 0.65) +
  geom_text(aes(label = paste0(qde, "\n(", percent_dep,")")), position = position_fill(vjust = 0.5), # nolint
            family = "Source Sans Pro", size = 3.2, color = "ghostwhite") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("Negativo" = "#DC143C",
                               "Positivo" = "#008080"),
                    na.value = "darkgray") +
  labs(x = NULL, y = NULL) +
  tema_barra()

ggsave(plot = graf_barra_lucro, "./plots_final/bar_lucro.png", h = 6, w = 4, device = "png") # nolint

#-----------------------------------------

sumario_lucro_setor <- dados_selecionados %>%
  filter(!is.na(lucros)) %>%
  group_by(setor) %>%
  summarise(tot = sum(lucros)) %>%
  mutate(result_pos = ifelse(tot >= 0, "Positivo", "Negativo"))

sumario_lucro_setor %>% janitor::adorn_totals("row")

graf_barra_lucro_setor <-
  ggplot(sumario_lucro_setor,
         aes(y = tot, color = result_pos, fill = result_pos,
             x = reorder(setor, tot))) +
  geom_col(width = 0.6) +
  geom_text(aes(label = format(round(tot/1e6, 0), big.mark = ".", # nolint
                               decimal.mark = ','), # nolint
                y = ifelse(tot>= 0, tot*1.03 - 1e4, tot - 50e7), # nolint
                hjust = ifelse(tot>= 0, "left", "right")),  # nolint
            vjust = 0.5,
            family = "Source Sans Pro", size = 3.5) +
  coord_flip() +
  scale_color_manual(values = c("Negativo" = "#DC143C",
                                "Positivo" = "#008080"),
                     na.value = "darkgray") +
  scale_fill_manual(values = c("Negativo" = "#DC143C",
                               "Positivo" = "#008080"),
                    na.value = "darkgray") +
  scale_y_continuous(labels = function(x) {
    paste(format(round(x/1e6, 1), big.mark = ".", decimal.mark = ','), "mi")}, expand = expansion(mult = .15)) + # nolint
  labs(x = NULL, y = NULL) +
  tema_barra()

ggsave(plot = graf_barra_lucro_setor, "./plots_final/bar_lucro_setor.png", h = 6, w = 7, device = "png") # nolint

###############################################################################################################

# mapa resultado----------------------------------------------------------------

colunas_interesse <- c("Dividendos",
                       "Subvenção",
                       "Reforço de Capital",
                       "Resultado para o Estado Acionista")

mapa_res <- dados_selecionados %>%
  group_by(Estado) %>%
  summarise_at(vars(colunas_interesse),
               .funs = ~-sum(as.numeric(.), na.rm = TRUE)) %>%
  mutate(Dividendos = -Dividendos) %>%
  gather(colunas_interesse, key = "variavel", value = "valor") %>%
  left_join(mapa, by = c("Estado" = "abbrev_state"))

mapa_res_simp <- mapa_res %>%
  filter(variavel == "Resultado para o Estado Acionista") %>%
  mutate(
    Resultado_cat = cut( # nolint
      -valor, # nolint
      breaks = c(-Inf, -500e6, -200e6, 0, 200e6, Inf),
      labels = c("Prejuízo maior que R$ 500 mi", "Prejuízo entre R$ 200 e 500 mi", "Prejuízo até R$ 200 mi", "Lucro até R$ 200 mi", "Lucro acima de R$ 200 mi")),  # nolint
    Resultado_cat = ifelse(valor == 0, "Sem informação", as.character(Resultado_cat))) # nolint

diverge_hcl(n = 7, rev = T) %>% dput() # nolint

cores_mapa <- c("#8E063B", "#BB7784", "#D6BCC0", "#E2E2E2", "#BEC1D4", "#7D87B9") # nolint

names(cores_mapa) <- c("Prejuízo maior que R$ 500 mi", "Prejuízo entre R$ 200 e 500 mi", "Prejuízo até R$ 200 mi", "Sem informação", "Lucro até R$ 200 mi", "Lucro acima de R$ 200 mi") # nolint

mapa_res_graf <- ggplot(mapa_res_simp) +
  geom_sf(aes(fill = Resultado_cat, geometry = geom), color = NA) +
  scale_fill_manual(values = cores_mapa) +
labs(title = NULL, fill = NULL, x = NULL, y = NULL) + # nolint
  tema_mapa() +
  theme(legend.position = c(0.2, 0.2),
        legend.text = element_text(size = 8))

ggsave(plot = mapa_res_graf, "./plots_final/mapa_result.png", h = 5, w = 5, device = "png") # nolint

tab_resultado <- dados_selecionados %>% group_by(Estado) %>% summarise(soma = sum(`Resultado para o Estado Acionista`, na.rm =TRUE)) %>% arrange(desc(soma)) # nolint

##################################################################################################################

# Resultado -  decomposição -----------------------------------------------

dados_selecionados %>% filter(result_NA) %>% nrow()

sumario_result <- dados_selecionados %>%
  select(dep, colunas_interesse) %>%
  group_by(dep) %>%
  summarise_all(~sum(as.numeric(.), na.rm = T)) %>% # nolint
  mutate(`Resultado para o Estado Acionista` = Dividendos - `Subvenção` - `Reforço de Capital`) # nolint


result_total_para_incorporar <- sumario_result %>%
  select(dep, `Resultado para o Estado Acionista`) %>%
  gather(`Resultado para o Estado Acionista`, key = componentes, value = y_end) %>% # nolint
  mutate(y_0 = 0)

result_waterfall <- sumario_result %>%
  select(-`Resultado para o Estado Acionista`) %>%
  mutate(Dividendos = -Dividendos) %>%
  filter(dep != "Não Informado") %>%
  gather(-dep, key = componentes, value = valor) %>%
  arrange(dep) %>%
  group_by(dep) %>%
  mutate(y_end = cumsum(valor),
         y_0 = lag(y_end,1)) %>% # nolint
  ungroup() %>%
  select(-valor) %>%
  mutate_at(.vars = vars(starts_with("y")), .funs = ~-.) %>%
  bind_rows(result_total_para_incorporar) %>%
  mutate(componentes = factor(componentes, levels = colunas_interesse)) %>%
  replace_na(list(y_0 = 0, y_end = 0)) %>%
  mutate(pto_medio = (y_0 + y_end)/2) # nolint


waterfall <- ggplot(result_waterfall %>% filter(dep != "Não Informado"),
                    aes(x = componentes, xend = componentes, color = componentes)) +  # nolint
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey",
             size = 0.5) +
  geom_segment(aes(y = ifelse(componentes == "Resultado para o Estado Acionista", NA, y_0), yend = y_end),  # nolint
               arrow = arrow(angle = 90, ends = "both", length = unit(.05, "inches")), # nolint
               size = .5) +
  geom_segment(aes(y = ifelse(componentes != "Resultado para o Estado Acionista", NA, y_0), yend = y_end),  # nolint
               size = 14) +
  geom_text(aes(y = ifelse(componentes == "Dividendos", y_end + .4e9,
                           y_end - .3e9),
                label = format(round((y_end-y_0)/1e6,0), big.mark = ".", # nolint
                               decimal.mark = ",")), family = "Source Sans Pro", size = 3.5, hjust = "center", vjust = "center") + # nolint
  scale_color_manual(values = c("Dividendos" = "#008080", "Subvenção" = "#DC143C", "Reforço de Capital" = "#DC143C", "Resultado para o Estado Acionista" = "#DC143C")) + # nolint
  scale_fill_manual(values = c("Dividendos" = "#008080", "Subvenção" = "#DC143C", "Reforço de Capital" = "#DC143C", "Resultado para o Estado Acionista" = "#DC143C")) + # nolint
  scale_y_continuous(labels = function(x){format(round(x/1e6, 1), big.mark = ".", decimal.mark = ',')}) + # nolint
  scale_x_discrete(labels = c("Dividendos", "Subvenção", "Reforço de Capital" = "Reforço\nde Capital", "Resultado para o Estado Acionista"="Resultado\n para o \nEstado Acionista")) + # nolint
  labs(x = NULL, y = NULL) +
  tema() + theme(panel.background = element_rect(fill = "ghostwhite",
                                                 color = NA),
                 strip.text = element_text(family = "Source Sans Pro")) +
  facet_wrap(~dep)

ggsave(plot = waterfall, "./plots_final/waterfall.png", h = 6, w = 6)

################################################################################################################

# Governança --------------------------------------------------------------

tab_linhas <- data.frame(setor = unique(dados_selecionados$setor), x0 = 0, x1 = 1) %>% # nolint
  gather(x0, x1, key = pos, value = x) %>%
  select(-pos) %>%
  arrange(setor, x)

dados_gov_setor <- dados_selecionados %>%
  filter(dep != "Não Informado") %>%
  mutate(gov = ifelse(is.na(gov), FALSE, gov)) %>%
  count(setor, gov, dep) %>%
  spread(gov, n, fill = 0) %>%
  mutate(total = `FALSE` + `TRUE`,
         pct_gov = `TRUE`/total) %>% # nolint
  select(setor, dep, pct_gov) %>%
  spread(dep, pct_gov) %>%
  group_by(setor) %>%
  mutate(
    maximo = max(Dependente, `Não Dependente`, na.rm = T), # nolint
    maior  = ifelse(`Dependente` > `Não Dependente`, "Dependente", "Não Dependente")) %>% # nolint
  gather(`Não Dependente`, `Dependente`, key = dep, value = pct_gov) %>%
  left_join(tab_linhas) %>%
  left_join(dados_qde_setor_dep)



gov_dotplot <- ggplot(dados_gov_setor, aes(y = reorder(setor, maximo),
                                           color = dep, x = pct_gov, group = setor)) + # nolint
  geom_path(aes(x = x), color = "lightgrey", size = 1.3, alpha = .5,
            arrow = arrow(angle = 90, ends = "both", type = "closed", length = unit(3.5, "points"))) + # nolint
  geom_point(size = 2.5) + #aes(size = n)) +
  geom_text(aes(label = ifelse(dep == maior | is.na(maior),
                               percent(pct_gov, accuracy = 1), NA),
                color = dep), fontface = "bold", size = 3.5,
            family = "Source Sans Pro",
            nudge_x = 0.1) +
  geom_text(aes(label = ifelse(dep == maior, NA, percent(pct_gov, accuracy = 1)),  # nolint
                color = dep),  size = 3.5,
            family = "Source Sans Pro",
            nudge_x = -0.07) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(labels = percent, breaks = seq(0, 1, .2)) +
  expand_limits(x = 1.15) +
  scale_color_manual(values = vetor_cores_dep) +
  scale_fill_manual(values = vetor_cores_dep) +
  tema_barra()

ggsave(plot = gov_dotplot, "./plots_final/gov_dotplot.png", h = 6, w = 5.9)

##############################################################################################################

# governança estados ------------------------------------------------------

tab_linhas_est <- data.frame(Nome_estado = unique(dados_selecionados$Nome_estado), x0 = 0, x1 = 1) %>% # nolint
  gather(x0, x1, key = pos, value = x) %>% 
  select(-pos) %>% 
  arrange(Nome_estado, x)

dados_qde_est_dep <- dados_roe %>% 
  filter(dep != "Não Informado") %>% 
  count(Nome_estado, dep)

dados_gov_estado <- dados_selecionados %>% 
  filter(dep != "Não Informado") %>% 
  mutate(gov = ifelse(is.na(gov), FALSE, gov)) %>% 
  count(Nome_estado, gov, dep) %>% 
  spread(gov, n, fill = 0) %>% 
  mutate(total = `FALSE` + `TRUE`, 
         pct_gov = `TRUE` / total) %>% # nolint
  select(Nome_estado, dep, pct_gov) %>% 
  spread(dep, pct_gov) %>% 
  group_by(Nome_estado) %>% 
  mutate(
    maximo = max(Dependente, `Não Dependente`, na.rm = T), # nolint
    maior  = ifelse(`Dependente` > `Não Dependente`, "Dependente", "Não Dependente")) %>% # nolint
  gather(`Não Dependente`, `Dependente`, key = dep, value = pct_gov) %>% 
  filter(pct_gov > 0) %>% # Remove valores menores ou iguais a zero
  left_join(tab_linhas_est) %>% 
  left_join(dados_qde_est_dep)

gov_est_dotplot <- ggplot(dados_gov_estado, aes(y = reorder(Nome_estado, maximo),  # nolint
                                                color = dep, x = pct_gov, group = Nome_estado)) + # nolint
  geom_path(aes(x = x), color = "lightgrey", size = 1.3, alpha = .5,
            arrow = arrow(angle = 90, ends = "both", type = "closed", length = unit(3.5, "points"))) + # nolint
  geom_point(size = 2) + #aes(size = n)) +
  geom_text(aes(label = ifelse(dep == maior | is.na(maior),
                               percent(pct_gov, accuracy = 1), NA),
                color = dep), fontface = "bold", size = 3,
            family = "Source Sans Pro",
            nudge_x = 0.085) +
  geom_text(aes(label = ifelse(dep == maior, NA, percent(pct_gov, accuracy = 1)), # nolint
                color = dep),  size = 3,
            family = "Source Sans Pro",
            nudge_x = -0.07) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(labels = percent, breaks = seq(0, 1, .2)) +
  expand_limits(x = 1.1) +
  scale_color_manual(values = vetor_cores_dep) +
  scale_fill_manual(values = vetor_cores_dep) +
  tema_barra()

ggsave(plot = gov_est_dotplot, "./plots_final/gov_est_dotplot.png", h = 6, w = 4.5)

################################################################################################################
# Total de registros antes do filtro
roe_gov_total <- nrow(dados_roe)

# Filtro aplicado e criação do gráfico
roe_gov_filtrado <- dados_roe %>%
  filter(!is.na(ROE), ROE != 0, PL > 0, ROE >= -1, ROE <= 1.5, !is.na(lucros), lucros != 0)

# Total de registros após o filtro
total_filtrado_roe_gov <- nrow(roe_gov_filtrado)

# Calcula e imprime o total de registros removidos
registros_removidos_roe_gov <- roe_gov_total - total_filtrado_roe_gov
print(paste("Total de registros removidos ROE_GOV:", registros_removidos_roe_gov))
# governança roe ----------------------------------------------------------

sumario_roe_gov <- dados_roe %>%
  group_by(cat_ROE, gov) %>%
  summarise(qde = n()) %>%
  group_by(gov) %>%
  mutate(pct_qde = percent(qde/sum(qde))) %>% # nolint
  ungroup() %>%
  mutate(y = case_when(cat_ROE == "bem_neg" ~ -0.75,
                       cat_ROE == "neg" ~ -0.25,
                       cat_ROE == "pos" ~  0.25,
                       cat_ROE == "bem_pos" ~  0.75))

sumario_roe_gov_sinal <- dados_roe %>%
  group_by(sinal_ROE, gov) %>%
  summarise(qde = n()) %>%
  group_by(gov) %>%
  mutate(pct_qde = percent(qde/sum(qde))) %>% # nolint
  ungroup() %>%
  mutate(y = ifelse(sinal_ROE == "Positivo", 0.5, -0.5))

# empresas fora do limte
dados_roe %>% filter(ROE > 2 | ROE < -2) %>% select(emp, Estado, gov, ROE) # nolint

roe_gov <- ggplot(dados_roe %>% filter(!is.na(ROE), ROE != 0, PL > 0, ROE >= -1, ROE <= 1.5, !is.na(lucros), lucros != 0), aes(y = ROE, color = sinal_ROE, x = gov, label = Empresa)) + # nolint
  geom_quasirandom() + #beeswarm() + #aes(size = PL),
  scale_color_manual(values = c("Negativo" = "#DC143C",
                                "Positivo" = "#008080")) +
  annotate("rect", xmin = 0, xmax = 1.5, ymin = 0, ymax = 2, alpha = 0.2, fill = "antiquewhite") + # nolint
  annotate("rect", xmin = 1.5, xmax = 2.7, ymin = 0, ymax = 2, alpha = 0.2, fill = "antiquewhite") + # nolint
  geom_text(data = sumario_roe_gov_sinal,  # nolint
            aes(y = ifelse(!gov, y, NA), # nolint
                label = paste0(pct_qde, ' das que NÃO \n possuem estrutura \nde governança'), # nolint
                color = sinal_ROE), # nolint
            x = 0.9, # 0.8 para estático
            hjust = "right", vjust = "center", family = "Source Sans Pro",  # nolint
            size = 3.5) +
  geom_text(data = sumario_roe_gov_sinal,  # nolint
            aes(y = ifelse(gov, y, NA),
                label = paste0(pct_qde, ' das que \npossuem estrutura \nde governança'), # nolint
                color = sinal_ROE), # nolint
            x = 2.1, # 2.4 para estático
            hjust = "left", vjust = "center", family = "Source Sans Pro",  # nolint
            size = 3.5) +
  labs(title = NULL, x = NULL, y = NULL) + # nolint
  scale_y_continuous(labels = percent,
                     breaks = define_breaks,
                     limits = c(-2, 2)) + # nolint
  scale_x_discrete(labels = c("Empresas que NÃO possuem \n estrutura de Governança completa", "Empresas que possuem \nestrutura de Governança completa")) + # nolint
  tema()

# Salvar o gráfico
ggsave(plot = roe_gov, "./plots_final/roe_gov.png", h = 6.5, w = 6)

####################################################################################################################

# PLR ---------------------------------------------------------------------

dados_plr_plot <- dados_selecionados %>%
  filter(dep == "Não Dependente",
         plr_rva %in% c("Sim", "Não")) %>%
  count(setor, plr_rva) %>%
  spread(plr_rva, n, fill = 0) %>%
  mutate(total = `Não` + `Sim`,
         pct_sim = scales::label_percent(accuracy = 1)(`Sim` / total), # nolint
         pct_nao = scales::label_percent(accuracy = 1)(`Não` / total)
  ) %>%
  gather(`Não`, `Sim`, key = plr, value = n)

plr <- ggplot(dados_plr_plot,
              aes(y = reorder(setor, total), x = n)) +
  geom_col(aes(fill = plr), width = .7) +
  geom_text(aes(label = ifelse(n == 0, "",
                               ifelse(plr == 'Não',  # nolint
                                      paste0(n, " (", pct_nao, ")"),  # nolint
                                      paste0(n, " (", pct_sim, ")") # \n?
                               ) # nolint
  ),  # nolint
  x = ifelse(plr == 'Não', total, n), # nolint
  color = plr), hjust=0, # nolint
  nudge_x = .1, # nolint
  family = "Source Sans Pro", size = 3.5) +
  scale_fill_manual(values = c("Sim" = "#735D36", "Não" = "#F4C773")) +
  scale_color_manual(values = c("Sim" = "#735D36", "Não" = "#DEA25D")) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_x_continuous(expand = expansion(add = c(0, 5))) +
  labs(y = NULL, x = NULL) +
  tema() +
  theme(axis.ticks.y = element_blank())

plr

ggsave(plot = plr, "./plots_final/plr.png", h = 6, w = 6)

dados_selecionados %>%
  filter(is.na(plr_rva)) %>% nrow() # nolint

################################################################################################################

# Pacotes necessários
library(dplyr)
library(plotly)
library(htmlwidgets)

# Dados de entrada: Cria uma coluna simplificada para `dep`
dados_viz <- dados_selecionados %>%
  mutate(
    dep_simplif = ifelse(dep == "Dependente", "Dependente", "Não Dependente")
  )

# Ordena `dep_simplif` e cria o eixo X numérico dentro de cada setor
dados_viz <- dados_viz %>%
  mutate(
    dep_simplif = factor(dep_simplif, levels = c("Dependente", "Não Dependente")),
    setor = factor(setor, levels = sort(unique(setor), decreasing = TRUE)) # Ordem inversa alfabética do setor
  ) %>%
  group_by(setor) %>%
  arrange(dep_simplif, .by_group = TRUE) %>%
  mutate(x = row_number()) %>%
  ungroup()

# DataFrame auxiliar para categorias da legenda
legenda_auxiliar <- tibble(
  x = NA,
  setor = NA,
  dep_simplif = c("Dependente", "Não Dependente")
)

# Une os dados reais com o auxiliar para manter a legenda
dados_viz_legenda <- bind_rows(dados_viz, legenda_auxiliar)

# Vetor de cores
vetor_cores <- c("Dependente" = "#f2ac29", "Não Dependente" = "#718c35")

# Lista de estados com a opção "Todos"
estados <- c("Todos", unique(dados_selecionados$Nome_estado))

# Gráfico interativo
grafico_empresas_setor <- plot_ly(
  data = dados_viz_legenda,
  x = ~x,
  y = ~setor,
  color = ~dep_simplif,
  colors = vetor_cores,
  hoverinfo = "text",
  text = ~ifelse(
    is.na(emp), "",
    paste0(
      "<b>", emp, "</b><br>",
      "Estado: ", Nome_estado, "<br>",
      "Situação: ", sit
    )
  ),
  marker = list(size = 10),
  type = "scatter",
  mode = "markers"
) %>%
  layout(
    title = list(
      text = "Ficha básica das empresas por setor",
      x = 0
    ),
    margin = list(l = 150, r = 30, t = 50, b = 30),
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    yaxis = list(
      title = "",
      autorange = "reversed"
    ),
    legend = list(
      title = list(text = ""),
      orientation = "h",
      x = 0, y = 1.1
    )
  ) %>%
  config(
    displayModeBar = FALSE
  )

# Processa o gráfico com plotly_build para extrair dados e layout
grafico_processado <- plotly_build(grafico_empresas_setor)
grafico_data <- grafico_processado$x$data
grafico_layout <- grafico_processado$x$layout

# Geração do HTML com filtro
html_file <- "./plots_final/grafico_empresas_setor_com_filtro.html"

html_content <- paste0(
  "<!DOCTYPE html>",
  "<html>",
  "<head>",
  "<script src='https://cdn.plot.ly/plotly-latest.min.js'></script>",
  "</head>",
  "<body>",
  "<h1>Ficha Básica das Empresas por Setor</h1>",
  "<label for='state-filter'>Selecione o Estado:</label>",
  "<select id='state-filter'>",
  "<option value='Todos'>Todos</option>",
  paste0("<option value='", estados[-1], "'>", estados[-1], "</option>", collapse = ""),
  "</select>",
  "<div id='grafico'></div>",
  "<script>",
  "  var data = ", jsonlite::toJSON(grafico_data, auto_unbox = TRUE), ";",
  "  var layout = ", jsonlite::toJSON(grafico_layout, auto_unbox = TRUE), ";",
  "  Plotly.newPlot('grafico', data, layout);",
  "  document.getElementById('state-filter').addEventListener('change', function() {",
  "    var selectedState = this.value;",
  "    data.forEach(trace => {",
  "      if (trace.text) {",
  "        trace.marker.opacity = trace.text.map(text => {",
  "          return (selectedState === 'Todos' || text.includes('Estado: ' + selectedState)) ? 1 : 0;",
  "        });",
  "      }",
  "    });",
  "    Plotly.react('grafico', data, layout);",
  "  });",
  "</script>",
  "</body>",
  "</html>"
)

# Salvar o HTML
writeLines(html_content, html_file)

print(paste("Gráfico com filtro criado em:", html_file))


#####################################################################################################################

# Definição das cores específicas para cada setor
cores_setores <- c(
  "ABASTECIMENTO DE ALIMENTOS E OUTROS INSUMOS" = "#0D0887",
  "COMUNICAÇÃO" = "#310597",
  "DESENVOLVIMENTO REGIONAL" = "#4C02A1",
  "ENERGIA" = "#6600A7",
  "FINANCEIRO" = "#7E03A8",
  "GÁS E DERIVADOS" = "#9511A1",
  "GESTÃO DE ATIVOS" = "#A92395",
  "HABITAÇÃO E URBANIZAÇÃO" = "#BC3488",
  "INFORMÁTICA E TECNOLOGIA DA INFORMAÇÃO" = "#CC4678",
  "MINERAÇÃO" = "#DA596A",
  "OUTROS" = "#E56B5D",
  "PESQUISA E ASSISTÊNCIA TÉCNICA AGROPECUÁRIA" = "#F07F4F",
  "PORTOS E HIDROVIAS" = "#F89441",
  "SANEAMENTO" = "#FDAB33",
  "SAÚDE" = "#FDC328",
  "TRANSPORTE" = "#F9DD25",
  "TURISMO" = "#F0F921"
)

# Criação da pasta, se não existir
if (!dir.exists("./plots_final/quebra")) {
  dir.create("./plots_final/quebra", recursive = TRUE)
}

# Loop para criar e salvar um gráfico para cada setor
for (setor_atual in setores$setor) {
  # Filtra os dados para o setor atual
  mapa_qde_setor <- mapa_qde %>%
    filter(setor == setor_atual)
  
  # Cria o gráfico para o setor atual
  graf_mapa_individual <- ggplot(mapa_qde_setor) +
    geom_sf(data = mapa, fill = "#EFEFEF", color = "ghostwhite") +  # Base do mapa
    geom_sf(aes(fill = ifelse(qde > 0, setor, NA)), color = "ghostwhite") + # Preenchimento por setor
    scale_fill_manual(values = cores_setores, na.value = "#EFEFEF") + # Aplica cores definidas
    theme_void() + # Remove elementos desnecessários do gráfico
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.position = "none",
      plot.title = element_text(size = 12, hjust = 0.5)
    ) +
    ggtitle(paste("Setor:", setor_atual)) # Adiciona título ao gráfico
  
  # Caminho para salvar o gráfico
  caminho_arquivo <- paste0("./plots_final/quebra/", setor_atual, ".png")
  
  # Salva o gráfico
  ggsave(
    filename = caminho_arquivo,
    plot = graf_mapa_individual,
    width = 9,
    height = 8,
    dpi = 300
  )
}

print("Gráficos gerados e salvos em ./plots_final/quebra.")


#---------------------------------------------------------

library(htmltools)
library(base64enc)

# Define o caminho da pasta com as imagens
caminho_absoluto <- "C:/Users/gustavo.silva/OneDrive - Tesouro Nacional/Área de Trabalho/projetos/RAIO_X_EMPRESAS_DOS_ESTADOS/2023/plots_final/quebra"

# Lista as imagens na pasta
imagens <- list.files(caminho_absoluto, pattern = "\\.png$", full.names = TRUE)

# Gera os nomes dos setores a partir dos nomes dos arquivos
setores <- gsub("\\.png$", "", basename(imagens))

# Função para converter imagens em Base64
imagem_para_base64 <- function(caminho_imagem) {
  encoded <- base64encode(caminho_imagem)
  paste0("data:image/png;base64,", encoded)
}

# Converte as imagens para Base64
imagens_base64 <- lapply(imagens, imagem_para_base64)

# Cria o filtro e a galeria em HTML
html <- tags$html(
  tags$head(
    tags$title("Galeria de Mapas"),
    tags$style(HTML("
      body { font-family: Arial, sans-serif; margin: 20px; }
      .image-container { display: none; text-align: center; margin-top: 20px; }
      .image-container.active { display: block; }
      .thumbnail-container { display: flex; flex-wrap: wrap; justify-content: center; gap: 10px; margin-top: 30px; }
      .thumbnail { width: 50px; cursor: pointer; }
      .image-container img { width: 30%; }
      img { border: 1px solid #ddd; border-radius: 4px; }
      img:hover { border: 1px solid #777; }
    "))
  ),
  tags$body(
    tags$label("Selecione o setor:"),
    tags$select(
      id = "filter",
      tags$option(value = setores[1], setores[1], selected = TRUE),
      lapply(seq_along(setores)[-1], function(i) {
        tags$option(value = setores[i], setores[i])
      })
    ),
    lapply(seq_along(imagens_base64), function(i) {
      tags$div(
        class = if (i == 1) "image-container active" else "image-container",
        id = setores[i],
        tags$img(src = imagens_base64[[i]])
      )
    }),
    tags$div(
      class = "thumbnail-container",
      lapply(seq_along(imagens_base64), function(i) {
        tags$img(
          src = imagens_base64[[i]],
          class = "thumbnail",
          onclick = paste0("document.getElementById('filter').value='", setores[i], "'; document.getElementById('filter').dispatchEvent(new Event('change'));")
        )
      })
    ),
    tags$script(HTML("
      const filterDropdown = document.getElementById('filter');
      const containers = document.querySelectorAll('.image-container');
      const initialSelection = filterDropdown.value;
      containers.forEach(container => container.classList.remove('active'));
      if (initialSelection) {
        document.getElementById(initialSelection).classList.add('active');
      }
      filterDropdown.addEventListener('change', function() {
        containers.forEach(container => container.classList.remove('active'));
        const selected = this.value;
        if (selected) {
          document.getElementById(selected).classList.add('active');
        }
      });
    "))
  )
)

# Salva o arquivo HTML
html_file <- file.path(caminho_absoluto, "../galeria.html")
save_html(html, html_file)

print(paste("Galeria HTML criada em:", html_file))
