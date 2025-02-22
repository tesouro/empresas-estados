# https://github.com/tylermorganwall/rayshader

# https://www.curso-r.com/blog/2019-02-10-sf-miojo/

# https://www.tylermw.com/3d-ggplots-with-rayshader/

library(rayshader)
library(ggplot2)
library(readxl)
library(tidyverse)
library(brazilmaps)
library(sf)
library(viridis)
library(extrafont)
library(gganimate)

loadfonts()

tema <- function(){
  theme_minimal() +
    theme(
      text = element_text(family = "Lora", colour = "grey20"),
      title = element_text(size = 10, color = "dimgrey", face = "plain"), 
      plot.subtitle = element_text(color = "grey20", face = "plain", size = 10),
      axis.text = element_text(colour = "grey20", size = 8),
      plot.caption = element_text(face = "italic"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(size = 0.5),
      axis.ticks.length = unit(.25, "cm"),
      axis.title = element_text(size = 8, colour = "grey20"),
      legend.position = 'none',
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9))
}

setwd("~/GitHub/estatais-estados")

dados_empresas_raw <- read_excel("./dados/Estatais.xlsx") %>%
  mutate(PL = as.numeric(PL),
         lucros = as.numeric(`Lucros / Preju�zos`)) %>%
  rename(dep = `Depend�ncia`,
         seg = Segmento,
         emp = Empresa) %>%
  mutate(seg = case_when(
    seg == "Inform�tica" ~ "INFORM�TICA",
    seg == "ASSIS, T�CNICA" ~ "ASSIST�NCIA T�CNICA",
    TRUE ~ seg))

tab_uf <- read_excel("./dados/tab_ufs.xlsx")

dados_empresas <- dados_empresas_raw %>% left_join(tab_uf, by = c("Estado" = "UF"))

# mapas -------------------------------------------------------------------

mapa <- get_brmap("State") 

mapa_dados <- mapa %>% 
  inner_join(dados_empresas, by = c("State" = "CODUF"))

mapa_qde <- mapa_dados %>%
  group_by(seg, State) %>%
  summarise(qde = n())

# graf_mapa <- ggplot(mapa_qde %>% filter(seg == "SANEAMENTO")) + 
#   geom_sf(aes(fill = qde > 0), color = "coral") + 
#   scale_fill_manual(values = c("TRUE" = "lightcoral", "FALSE" = NA)) +
#   # scale_fill_viridis_d(direction = 1,
#   #                    option = "magma")+#,
#   #                    #breaks = c(1e3, 100e3, 10000e3),
#   #                    #trans = "log", #para usar uma escala de log
#   #                    #labels = function(x){format(x/1e6, decimal.mark = ",", big.mark = ".")}) + 
#   #labs(fill = "Popula��o \n(milh�es)") +
#   tema() + 
#   theme(axis.line = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         text = element_text(family = "Source Sans Pro"),
#         legend.position = "none",
#         legend.text = element_text(size = 10),
#         plot.background = element_blank(),
#         panel.background = element_blank())

dados_qde <- dados_empresas %>%
  group_by(Estado, seg) %>%
  summarise(qde = n()) %>%
  filter(!is.na(seg))

segmentos <- data.frame("seg" = unique(dados_empresas$seg))

combinacao_est_seg <- merge(segmentos, tab_uf, by = NULL) %>%
  rename(Estado = UF,
         State = CODUF) %>%
  left_join(dados_qde) %>%
  left_join(mapa) %>%
  filter(!is.na(seg)) %>%
  arrange(seg)

graf_mapa_comp <- ggplot(combinacao_est_seg, aes(group = State))+# %>% filter(seg == "OUTRO")) + 
  geom_sf(aes(fill = ifelse(qde > 0, seg, NA), geometry = geometry), color = "ghostwhite") + 
  # scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = NA)) +
  scale_fill_viridis_d(direction = 1,
                     option = "plasma", na.value = "#EFEFEF")+#,
                     #breaks = c(1e3, 100e3, 10000e3),
                     #trans = "log", #para usar uma escala de log
                     #labels = function(x){format(x/1e6, decimal.mark = ",", big.mark = ".")}) +
  #labs(fill = "Popula��o \n(milh�es)") +
  tema() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Source Sans Pro"),
        legend.position = "none",
        legend.text = element_text(size = 10),
        plot.background = element_blank(),
        panel.background = element_blank())

# graf_mapa_comp + geom_text(aes(label = State), x = -15, y = -47, size = 10)

graf_mapa_gif <- graf_mapa_comp + transition_states(states = seg,
                                    transition_length = 1,
                                    state_length = 3) +
  labs(title = "Estados que possuem empresas do setor de {closest_state}") +
  theme(title = element_text(size = 13, face = "plain"))

animate(graf_mapa_gif, nframes = nrow(segmentos)*20, fps = 8, type = "cairo")

anim_save("mapa.gif", animation = last_animation())

# graf_mapa_comp2 <- ggplot(combinacao_est_seg)+ #%>% filter(seg == "SANEAMENTO")) + 
#   geom_sf(aes(fill = seg, geometry = geometry)) + 
#   scale_fill_viridis() +
#   # scale_fill_viridis_d(direction = 1,
#   #                    option = "magma")+#,
#   #                    #breaks = c(1e3, 100e3, 10000e3),
#   #                    #trans = "log", #para usar uma escala de log
#   #                    #labels = function(x){format(x/1e6, decimal.mark = ",", big.mark = ".")}) + 
#   #labs(fill = "Popula��o \n(milh�es)") +
#   tema() + 
#   theme(axis.line = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         text = element_text(family = "Source Sans Pro"),
#         legend.position = "none",
#         legend.text = element_text(size = 10),
#         plot.background = element_blank(),
#         panel.background = element_blank())
# 
# graf_mapa_comp2 + transition_states(states = seg,
#                                 transition_length = 1,
#                                 state_length = 3) +
#   labs(title = "Estados que possuem empresas do setor de {closest_state}") +
#   theme(title = element_text(size = 12))
# 
# # quanto mais frames, mais se v� a movimenta��o dos estados
# 
# animate(gif_segmentos, nframes = nrow(segmentos)*30, fps = 10, type = "cairo")

graf_empXsetor <- ggplot(combinacao_est_seg%>%select(-geometry)) +
  geom_tile(aes(x = seg, y = reorder(nome, desc(nome)), fill = qde), color = "white") +
  scale_fill_viridis(direction = -1, na.value="ghostwhite", breaks = 1:max(combinacao_est_seg$qde, na.rm = T)) +
  labs(x = NULL, y = NULL, title = "Quantidade de empresas estatais por estado e setor", fill = "Quantidade") +
  tema() + theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(vjust = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "Source Sans Pro"),
        legend.title = element_text(family = "Source Sans Pro"))

plot_gg(graf_empXsetor,multicore=TRUE,width=10,height=10,scale=350)

qde_empresas_seg <- dados_empresas %>% 
  group_by(seg) %>%
  summarise(qde = n()) %>%
  filter(!is.na(seg))

graf_qde_emp <- 
  ggplot(qde_empresas_seg, aes(x = reorder(seg, qde), y = qde)) +
  geom_col(width = 0.75, fill = "steelblue") +
  geom_text(aes(label = qde, y = qde + 1), 
            vjust = 0.3, family = "Open Sans Condensed", size = 4, color = "steelblue") +
  coord_flip() +
  labs(x = NULL, y = NULL, title = "Quantidade de empresas por segmento") +
  tema()

mapa_qde <- mapa_dados %>%
  group_by(State) %>%
  summarise(qde = n())

graf_mapa_qde <- ggplot(mapa_qde)+ #%>% filter(seg == "SANEAMENTO")) + 
  geom_sf(aes(fill = qde, geometry = geometry), color = NA) + 
  scale_fill_viridis(direction = -1,
                     option = "magma",
                     na.value="ghostwhite"
                     )+
  labs(title = "Quantidade de empresas estatais em cada estado",
       fill = "Quantidade") +
  tema() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Source Sans Pro"),
        legend.position = "bottom",
        plot.background = element_blank(),
        panel.background = element_blank())

plot_gg(graf_mapa_qde,multicore=TRUE,width=12,height=12,scale=250)


mapa1_saneamento <- mapa_dados %>%
  filter(seg)

emp <- dados_empresas %>% group_by(Estado, `Depend�ncia`, Segmento) %>% summarise(qde = n())


ggplot(emp, aes(y = qde, x = Segmento, fill = `Depend�ncia`)) +
  geom_col(width = 0.8) +
  coord_flip() +
  theme_minimal()

library(ggbeeswarm)

ggplot(dados_empresas, aes(y = PL, x = `Depend�ncia`, color = (PL > 0), size = PL,
                           alpha = 0.5)) +
         #geom_jitter()+
  geom_beeswarm()+
        #geom_quasirandom(varwidth = TRUE, alpha = 0.5, size = 2) +
  scale_y_log10() +
         theme_minimal()



ggplot(dados_empresas, aes(y = ))

ggplot(dados_empresas, aes(x = Segmento))


    
mapa_dados <- mapa %>% 
  inner_join(dados_ibge, c("City" = "CodMun")) %>% 
  rename(pop = `POP EST`,
         catPop = `CLASSE POP`)

# como a �rea inteira do munic�pio � projetada, o mais l�gico talvez fosse usar a densidade m�dia no munic�pio, e n�o a popula��o.

mapa <- ggplot(mapa_dados) + 
  geom_sf(aes(fill = pop), color = NA) + 
  scale_fill_viridis(direction = -1,
                     option = "magma",
                     #breaks = c(1e3, 100e3, 10000e3),
                     #trans = "log", #para usar uma escala de log
                     labels = function(x){format(x/1e6, decimal.mark = ",", big.mark = ".")}) + 
  labs(fill = "Popula��o \n(milh�es)") +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Source Sans Pro"),
        legend.position = "none",
        legend.text = element_text(size = 10),
        plot.background = element_blank(),
        panel.background = element_blank())

#render_depth(focallength=100,focus=0.72)

plot_gg(mapa,multicore=TRUE,width=12,height=12,scale=350)
render_camera(fov = 70, zoom = 0.5, theta = 20, phi = 35)
render_camera(fov = 75, zoom = 0.45, theta = 0, phi = 40)
render_camera(fov = 45, zoom = 0.35, theta = 0, phi = 30)
render_camera(fov = 45, zoom = 0.35, theta = 10, phi = 50)
render_camera(fov = 90, zoom = 0.15, theta = 10, phi = 20)

render_camera(fov = 45, zoom = 0.25, theta = 0, phi = 30)
render_camera(fov = 15, zoom = 0.25, theta = 0, phi = 30)
render_camera(fov = 45, zoom = 0.45, theta = 10, phi = 40)
render_camera(fov = 45, zoom = 0.35, theta = 10, phi = 40)
# phi: azimuth
# theta: rota��o
# d� para passar vetores de zoom, fov, theta e phi para fazer a c�mera passear.
render_snapshot("brasil3d.png")
render_movie("teste.mp4")

#
#render_camera()



# gg = ggplot(diamonds, aes(x, depth)) +
#   stat_density_2d(aes(fill = stat(nlevel)), 
#                   geom = "polygon",
#                   n = 100,bins = 10,contour = TRUE) +
#   facet_wrap(clarity~.) +
#   scale_fill_viridis_c(option = "A")
# plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250)

# ver:
# https://github.com/kraaijenbrink/warmingstripes-3d/blob/master/animate.r

