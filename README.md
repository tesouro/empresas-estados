# Estatais por Estado — Série Histórica

Projeto para análise e visualização de dados de empresas estatais por estado, atualizado anualmente. Cada versão anual vive em uma pasta `vYYYY` com scripts R, dados, gráficos e páginas HTML.

## Visão Geral

- Mantemos uma linha do tempo anual (v2019, v2020, …, v2025).
- A cada ano, copia-se a pasta do ano anterior e cria-se a nova pasta do ano (ex.: copiar `v2024` e criar `v2025`).
- Em seguida, executam-se os scripts de `Análise R` e de `Análise Comparativo R` para gerar todos os arquivos (dados intermediários, CSVs, gráficos e HTMLs).
- No `Análise R`, é necessário substituir/atualizar o caminho do arquivo `.xlsx` que contém os dados daquele ano antes de rodar a análise.

## Estrutura do Projeto

- Pastas anuais: `v2019/` … `v2025/` — cada uma contém `analise.R`, arquivos de dados e saídas (plots e HTMLs).
- [vis/](vis) — scripts e bibliotecas JS usados nas visualizações (ex.: `vis-mapa.js`, `vis-cards.js`).
- [dados/](dados) — bases e arquivos geográficos compartilhados em algumas versões (CSV, GeoJSON, TopoJSON, etc.).
- [other/](other) — materiais auxiliares (scripts, notas, protótipos).
- Raiz — páginas e arquivos utilitários (ex.: [index.html](index.html), [estilo.css](estilo.css)).

## Fluxo Anual (Passo a Passo)

1. Copiar a pasta do ano anterior para a nova (`vYYYY` → `vYYYY+1`).
2. Abrir os scripts de `Análise R` na pasta do novo ano (ex.: `v2025/analise.R` ou dentro de `v2025/Analise R/`).
3. Substituir o caminho do arquivo `.xlsx` de dados no `Análise R` para apontar para a planilha do ano corrente.
4. Executar o `Análise R` para gerar dados intermediários, CSVs, gráficos e páginas HTML.
5. Executar o `Análise Comparativo R` para produzir gráficos e tabelas comparativas entre anos.
6. Validar as saídas (CSV, gráficos em `plots/`, páginas em `index.html` e `vis/`).
7. Versionar as mudanças (commit) e, opcionalmente, criar uma tag com o ano.

> Observação: Os nomes e locais exatos dos scripts podem variar entre anos (ex.: pastas como `Analise R/`). Adapte os caminhos conforme a estrutura do ano em questão.

## Pré-requisitos

- R (recomendado R >= 4.x) e, opcionalmente, RStudio.
- Pacotes R utilizados pelos scripts (instale conforme as dependências indicadas nos scripts). Exemplos comuns: `readxl`, `dplyr`, `tidyr`, `ggplot2`, `sf`.

Exemplo de instalação de pacotes no R:

```r
install.packages(c("readxl","dplyr","tidyr","ggplot2","sf"))
```

## Instalação Completa de Pacotes (v2025)

Com base nos scripts definitivos de 2025, instale os seguintes pacotes em um único passo:

```r
install.packages(c(
	"tidyverse", 
	"ggplot2",
	"dplyr",
	"tidyr",
	"stringr",
	"tibble",
	"readr",
	"readxl",
	"scales",
	"extrafont",
	"gganimate",
	"ggbeeswarm",
	"plotly",
	"colorspace",
	"RColorBrewer",
	"viridis",
	"geobr",
	"cartogram",
	"sf",
	"geojsonsf",
	"janitor",
	"htmlwidgets",
	"htmltools",
	"base64enc",
	"jsonlite",
	"rmapshaper",
	"geojsonio"
))
```

Pacotes usados no `v2025/Analise R/Analise_R_2025_Definitiva.R` (lista exaustiva):

- ggplot2, dplyr, tidyr, readr, stringr, tibble (via tidyverse)
- readxl, scales, extrafont, gganimate, ggbeeswarm, plotly
- colorspace, RColorBrewer, viridis
- geobr, cartogram, sf, geojsonsf
- janitor, htmlwidgets, htmltools, base64enc
- jsonlite, rmapshaper, geojsonio

### Dependências de sistema (Debian/Ubuntu)

Em Debian/Ubuntu, instale os pacotes de desenvolvimento necessários (inclui dependências para `sf`, `geojsonsf`, `geojsonio` e compilação de pacotes R):

```bash
sudo apt-get update
sudo apt-get install -y \
	build-essential pkg-config \
	libprotobuf-dev protobuf-compiler \
	libjq-dev libv8-dev \
	libudunits2-dev libgdal-dev libgeos-dev libproj-dev \
	libsqlite3-dev libcurl4-openssl-dev libssl-dev libxml2-dev
```

Opcionalmente, instale também `gdal-bin` para utilitários GDAL:

```bash
sudo apt-get install -y gdal-bin
```

Se estiver em Fedora/RHEL:

```bash
sudo dnf install -y \
	gdal gdal-devel \
	geos geos-devel \
	proj proj-devel \
	udunits2 udunits2-devel
```

### Fontes (extrafont)

Para gráficos com fontes personalizadas:

```r
library(extrafont)
extrafont::font_import(prompt = FALSE)
extrafont::loadfonts(device = "pdf")
```

> Observação: em Linux, garanta que as fontes desejadas estejam instaladas no sistema.

## Como Executar

Via terminal (Linux):

```bash
# entrar na pasta do ano (genérico)
cd v2025

# executar a análise principal (genérico)
Rscript analise.R

# executar a análise comparativa (genérico; ajuste conforme o ano)
Rscript "Analise R/analise_comparativo.R"
```

Via RStudio:

- Abra o arquivo de projeto RStudio do ano correspondente (ex.: `v2025/estatais-estados.Rproj`).
- Execute os scripts de análise pelo RStudio (Run Source), após ajustar o caminho do `.xlsx` no `Análise R`.

### Exemplo: v2025 (scripts definitivos)

Arquivos de referência:

- [v2025/Analise R/Analise_R_2025_Definitiva.R](v2025/Analise%20R/Analise_R_2025_Definitiva.R)
- [v2025/Analise R/Analise_Comparativo_R_2025_Definitiva.R](v2025/Analise%20R/Analise_Comparativo_R_2025_Definitiva.R)

Execução via terminal:

```bash
cd v2025/Analise\ R

# Análise R (gera bases e gráficos do ano)
Rscript Analise_R_2025_Definitiva.R

# Análise Comparativo R (gera comparativos entre anos)
Rscript Analise_Comparativo_R_2025_Definitiva.R
```

## Atualização do Caminho da Planilha (.xlsx)

- No `Análise R`, localize a leitura da planilha (ex.: função `readxl::read_excel()` ou similar) e substitua o caminho para a planilha do ano atual.
- Use caminhos relativos à pasta do ano (ex.: `v2025/dados/dados.xlsx`) ou absolutos, garantindo que a estrutura de colunas permaneça compatível.

Exemplo (ajuste conforme a sua planilha de 2025):

```r
library(readxl)
dados <- read_excel("CAMINHO/para/sua_planilha_2025.xlsx")
```

## Saídas Esperadas

- Dados intermediários (ex.: `.rds`) e CSVs atualizados na pasta do ano (ex.: `vYYYY/dados/`).
- Gráficos e arquivos HTML em `vYYYY/plots/` e páginas como `vYYYY/index.html`.
- Visualizações interativas que utilizam [vis/vis-mapa.js](vis/vis-mapa.js) e [vis/vis-cards.js](vis/vis-cards.js).

## Visualização

- Abra o `index.html` da pasta do ano diretamente no navegador, ou sirva localmente:

```bash
python3 -m http.server 8000
# depois acesse: http://localhost:8000/v2025/index.html
```

## Organização e Manutenção

- Mantenha consistência entre anos: estrutura de pastas e nomenclaturas.
- Documente mudanças relevantes (novas colunas, transformações) diretamente nos scripts ou em notas dentro de `other/`.
- Para dados geográficos, verifique as dependências de arquivos em [dados/](dados) e mantenha versões compatíveis.

---

 
Versão 2022 da história das Empresas dos Estados. Tudo o que foi usado para construí-la está neste repositório, desde os dados originais e o script `R` de análise, até os arquivos da página web.

Está procurando os dados? Eles estão [aqui](./dados/dados.csv).

