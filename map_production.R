
library(dplyr)
library(readxl)
library(sf)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(cowplot)
library(ggrepel)

setwd("D:\\rodolfo\\Area de trabalho\\espacialização")

setores_2022 <- st_read("Malha_Sencitaria_2022.shp")
RD <- st_read('Rios_Duplos.shp')
rios <- st_read('D:\\rodolfo\\Area de trabalho\\GEOFT_BHO_REF_RIO\\GEOFT_BHO_REF_RIO.shp')
df <- read_xlsx("Processamento de Info/VSA e GP/IVS.xlsx", sheet = 3, col_names = TRUE)|> mutate(CD_GEOCODI = as.character(df$CD_GEOCODI)) |> rename('CD_SETOR' = 'CD_GEOCODI')

ivs <- left_join(setores_2022, df, by = "CD_SETOR") 

########################### mapa do grau de perigo ###################################
########################### hazard level map ###################################
GP <- ggplot()+
  geom_sf(data = ivs, aes(fill = categoria_gp), col = 'grey50')+
  geom_sf(data = RD, col = "transparent", fill = "cyan")+
  theme_light()+
  labs(x = "Longitude", y = "Latitude")+
  theme(plot.title = element_text(hjust = 0.10, vjust = -12, size = 18, face = "bold"), 
        legend.position =c(0.14,0.27), 
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        axis.title.x = element_text(face = "bold", size = 14), 
        axis.title.y = element_text(face = "bold", size = 14), 
        axis.text.y = element_text(angle = 90, size = 10), 
        axis.text.x = element_text(size = 10), 
        legend.background = element_blank())+
  #scale_fill_viridis_c(option = "cividis", direction = -1, na.value = "white")+
  #scale_fill_brewer(direction = -1,palette = "Blues", breaks = c("Baixa", "Moderada","Alta","NA"))+
  scale_fill_manual(name = "Perigo de Inundação",
                  breaks = c("Baixa", "Moderada", "Alta", NA),
                  values = c("lightblue","steelblue1","darkblue"), 
                  na.value = "gray30", 
                  labels = c("Baixa", "Moderada", "Alta", "Sem informação"), 
                  na.translate = T)+
  annotation_north_arrow(style = north_arrow_nautical(),
                         location = "tr", 
                         height = unit(3, "cm"), 
                         width = unit(3,"cm") )+
  annotation_scale(height = unit(0.3,"cm"), 
                   style = "tick", 
                   text_cex = 1)+
xlim(-49.3,-49)+
ylim(-5.45,-5.20)

############### mapas de vulnerabilidade social ############################
############### social vulnerability maps ############################
ivs_2010 <- ggplot()+
  geom_sf(data = ivs, aes(fill = categoria_IVS_2010), col = 'grey50')+
  geom_sf(data = RD, col = "transparent", fill = "cyan")+
  theme_light()+
  labs(title = "(A)", x = "Longitude", y = "Latitude")+
  theme(plot.title = element_text(hjust = 0.10, vjust = -12, size = 17, face = "bold"), 
        legend.position =c(0.16,0.27), 
        legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 8),
        axis.title.x = element_text(face = "bold", size = 11), 
        axis.title.y = element_text(face = "bold", size = 11), 
        axis.text.y = element_text(angle = 90, size = 8),
        axis.text.x = element_text(size = 8),
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_blank())+
  #scale_fill_viridis_c(option = "cividis", direction = -1, na.value = "white")+
  #scale_fill_brewer(palette = "Blues", breaks = c("low", "medium","high","no information"))+
  scale_fill_manual(name = paste("Vulnerabilidade", "\n","Social"),
                  values = c("#FFE5B4","orange","red"), 
                  breaks = c("Baixa","Moderada","Alta", NA), 
                  labels = c("Baixa","Moderada","Alta","Sem informação"),
                  na.value = "gray30",
                  na.translate = T)+
  annotation_north_arrow(style = north_arrow_nautical(),location = "tr", height = unit(1, "cm"), width = unit(1,"cm") )+
  annotation_scale(height = unit(0.3,"cm"), style = "tick", text_cex = 1)+xlim(-49.3,-49)+ylim(-5.45,-5.20)

plot(ivs_2010)

ivs_2022 <- ggplot()+
  geom_sf(data = ivs, aes(fill = categoria_IVS_2022), col = 'grey50')+
  geom_sf(data = RD, col = "transparent", fill = "cyan")+
  theme_light()+
  labs(title = "(B)", x = "Longitude", y = "Latitude")+
  theme(plot.title = element_text(hjust = 0.10, vjust = -12, size = 17, face = "bold"), 
        legend.position =c(0.16,0.27), 
        legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 8),
        axis.title.x = element_text(face = "bold", size = 11), 
        axis.title.y = element_text(face = "bold", size = 11), 
        axis.text.y = element_text(angle = 90, size = 8),
        axis.text.x = element_text(size = 8),
        legend.key.size = unit(0.4,"cm"),
        legend.background = element_blank())+
  #scale_fill_viridis_c(option = "cividis", direction = -1, na.value = "white")+
  #scale_fill_brewer(palette = "Blues", breaks = c("low", "medium","high","no information"))+
  scale_fill_manual(name = paste("Vulnerabilidade","\n", "Social"),
                  values = c("#FFE5B4","orange","red"), 
                  breaks = c("Baixa","Moderada","Alta", NA), 
                  labels = c("Baixa","Moderada","Alta","Sem informação"),
                  na.value = "gray30",
                  na.translate = T)+
  annotation_north_arrow(style = north_arrow_nautical(),location = "tr", height = unit(1, "cm"), width = unit(1,"cm") )+
  annotation_scale(height = unit(0.3,"cm"), style = "tick", text_cex = 1)+xlim(-49.3,-49)+ylim(-5.45,-5.20)

plot(ivs_2022)


mud_social <- ggplot()+
  geom_sf(data = ivs, aes(fill = Mudanca_social), col = 'grey50')+
  geom_sf(data = RD, col = "transparent", fill = "cyan")+
  theme_light()+
  labs(title = "(C)", x = "Longitude", y = "Latitude")+
  theme(plot.title = element_text(hjust = 0.10, vjust = -12, size = 17, face = "bold"), 
        legend.position =c(0.16,0.27), 
        legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 8),
        axis.title.x = element_text(face = "bold", size = 11), 
        axis.title.y = element_text(face = "bold", size = 11), 
        axis.text.y = element_text(angle = 90, size = 8),
        axis.text.x = element_text(size = 8),
        legend.key.size = unit(0.4,"cm"),
        legend.background = element_blank())+
  #scale_fill_viridis_c(option = "cividis", direction = -1, na.value = "white")+
  #scale_fill_brewer(palette = "Blues", breaks = c("low", "medium","high","no information"))+
  scale_fill_manual(name = "Mudança social",
                  values = c("#FFE5B4","orange","red"), 
                  breaks = c("Redução","Estabilidade","Aumento", NA), 
                  labels = c("Redução","Estabilidade","Aumento","Sem informação"),
                  na.value = "gray30",
                  na.translate = T)+
  annotation_north_arrow(style = north_arrow_nautical(),location = "tr", height = unit(1, "cm"), width = unit(1,"cm") )+
  annotation_scale(height = unit(0.3,"cm"), style = "tick", text_cex = 1)+xlim(-49.3,-49)+ylim(-5.45,-5.20)

plot(mud_social)


linha_cima <- plot_grid(ivs_2010, ivs_2022, nrow = 1, ncol = 2)
linha_baixo <- plot_grid(NULL, mud_social, NULL, nrow = 1, rel_widths = c(0.5, 1, 0.5))
mapa_final <- plot_grid(linha_cima, linha_baixo, nrow = 2)
print(mapa_final)

######################    Exposição  ##################################################
######################   potential population exposure ##################################################


exp_2010 <- ggplot()+
  geom_sf(data = ivs, aes(fill = categoria_Pexp_2010), col = 'grey50')+
  geom_sf(data = RD, col = "transparent", fill = "cyan")+
  theme_light()+
  labs(title = "(A)", x = "Longitude", y = "Latitude")+
  theme(plot.title = element_text(hjust = 0.10, vjust = -12, size = 17, face = "bold"), 
        legend.position =c(0.14,0.27), 
        legend.title = element_text(size = 7, face = "bold"),
        legend.text = element_text(size = 7),
        axis.title.x = element_text(face = "bold", size = 11), 
        axis.title.y = element_text(face = "bold", size = 11), 
        axis.text.y = element_text(angle = 90, size = 8),
        axis.text.x = element_text(size = 8),
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_blank())+
  #scale_fill_viridis_c(option = "cividis", direction = -1, na.value = "white")+
  #scale_fill_brewer(palette = "Blues", breaks = c("low", "medium","high","no information"))+
  scale_fill_manual(name = paste("População", "\n","potencialmente","\n","exposta"),
                  values = c("#FECC5C","#FD8D3C","#F03B20", "#BD0026"), 
                  breaks = c("Baixa","Moderada","Alta", "Sem Exposição", NA), 
                  labels = c("Baixa","Moderada","Alta","Sem exposição","Sem informação"),
                  na.value = "gray30",
                  na.translate = T)+
  annotation_north_arrow(style = north_arrow_nautical(),location = "tr", height = unit(1, "cm"), width = unit(1,"cm") )+
  annotation_scale(height = unit(0.3,"cm"), style = "tick", text_cex = 1)+xlim(-49.3,-49)+ylim(-5.45,-5.20)

plot(exp_2010)

exp_2022 <- ggplot()+
  geom_sf(data = ivs, aes(fill = categoria_Pexp_2022), col = 'grey50')+
  geom_sf(data = RD, col = "transparent", fill = "cyan")+
  theme_light()+
  labs(title = "(B)", x = "Longitude", y = "Latitude")+
  theme(plot.title = element_text(hjust = 0.10, vjust = -12, size = 17, face = "bold"), 
        legend.position =c(0.14,0.27), 
        legend.title = element_text(size = 7, face = "bold"),
        legend.text = element_text(size = 7),
        axis.title.x = element_text(face = "bold", size = 11), 
        axis.title.y = element_text(face = "bold", size = 11), 
        axis.text.y = element_text(angle = 90, size = 8),
        axis.text.x = element_text(size = 8),
        legend.key.size = unit(0.4,"cm"),
        legend.background = element_blank())+
  #scale_fill_viridis_c(option = "cividis", direction = -1, na.value = "white")+
  #scale_fill_brewer(palette = "Blues", breaks = c("low", "medium","high","no information"))+
  scale_fill_manual(name = paste("População", "\n","potencialmente","\n","exposta"),
                  values = c("#FECC5C","#FD8D3C","#F03B20", "#BD0026"), 
                  breaks = c("Baixa","Moderada","Alta","Sem Exposição", NA), 
                  labels = c("Baixa","Moderada","Alta","Sem exposição","Sem informação"),
                  na.value = "gray30",
                  na.translate = T)+
  annotation_north_arrow(style = north_arrow_nautical(),location = "tr", height = unit(1, "cm"), width = unit(1,"cm") )+
  annotation_scale(height = unit(0.3,"cm"), style = "tick", text_cex = 1)+xlim(-49.3,-49)+ylim(-5.45,-5.20)


mud_exp <- ggplot()+
  geom_sf(data = ivs, aes(fill = Mudanca_Pexp), col = 'grey50')+
  geom_sf(data = RD, col = "transparent", fill = "cyan")+
  theme_light()+
  labs(title = "(C)", x = "Longitude", y = "Latitude")+
  theme(plot.title = element_text(hjust = 0.10, vjust = -12, size = 17, face = "bold"), 
        legend.position =c(0.14,0.27), 
        legend.title = element_text(size = 7, face = "bold"),
        legend.text = element_text(size = 7),
        axis.title.x = element_text(face = "bold", size = 11), 
        axis.title.y = element_text(face = "bold", size = 11), 
        axis.text.y = element_text(angle = 90, size = 8),
        axis.text.x = element_text(size = 8),
        legend.key.size = unit(0.4,"cm"),
        legend.background = element_blank())+
  #scale_fill_viridis_c(option = "cividis", direction = -1, na.value = "white")+
  #scale_fill_brewer(palette = "Blues", breaks = c("low", "medium","high","no information"))+
  scale_fill_manual(name = paste("Mudança na", "\n","exposição"),
                  values = c("#FECC5C","#FD8D3C","#F03B20", "#BD0026"), 
                  breaks = c("Redução","Estabilidade","Aumento","Sem Exposição", NA), 
                  labels = c("Redução","Estabilidade","Aumento","Sem exposição","Sem informação"),
                  na.value = "gray30",
                  na.translate = T)+
  annotation_north_arrow(style = north_arrow_nautical(),location = "tr", height = unit(1, "cm"), width = unit(1,"cm") )+
  annotation_scale(height = unit(0.3,"cm"), style = "tick", text_cex = 1)+xlim(-49.3,-49)+ylim(-5.45,-5.20)


linha_cima <- plot_grid(exp_2010, exp_2022, nrow = 1, ncol = 2)
linha_baixo <- plot_grid(NULL, mud_exp, NULL, nrow = 1, rel_widths = c(0.5, 1, 0.5))
mapa_final <- plot_grid(linha_cima, linha_baixo, nrow = 2)
print(mapa_final)

ggsave(filename = 'Processamento de Info/VSA e GP/Pexp_categorica.jpg', plot = last_plot(), width = 10, height = 8, dpi = 300)


###################### mapa de vulnerabilidade socioambiental #########################
###################### socio-environmental vulnerability map #########################


vsa_2010 <- ggplot()+
  geom_sf(data = ivs, aes(fill = categoria_VSA_2010), col = 'grey50')+
  geom_sf(data = RD, col = "transparent", fill = "cyan")+
  theme_light()+
  labs(title = "(A)", x = "Longitude", y = "Latitude")+
  theme(plot.title = element_text(hjust = 0.10, vjust = -12, size = 17, face = "bold"), 
        legend.position =c(0.14,0.27), 
        legend.title = element_text(size = 7, face = "bold"),
        legend.text = element_text(size = 7),
        axis.title.x = element_text(face = "bold", size = 11), 
        axis.title.y = element_text(face = "bold", size = 11), 
        axis.text.y = element_text(angle = 90, size = 8),
        axis.text.x = element_text(size = 8),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_blank())+
  #scale_fill_viridis_c(option = "cividis", direction = -1, na.value = "white")+
  #scale_fill_brewer(palette = "Blues", breaks = c("low", "medium","high","no information"))+
  scale_fill_manual(name = paste("Vulnerabilidade", "\n","Socioambiental"),
                  values = c("darkgreen","lightgreen","yellow","orange","red","blue"), 
                  breaks = c("Muito Baixa", "Baixa","Moderada","Alta", "Muito Alta","Sem Exposição", NA), 
                  labels = c("Muito Baixa", "Baixa","Moderada","Alta", "Muito Alta","Sem exposição","Sem informação"),
                  na.value = "gray30",
                  na.translate = T)+
  annotation_north_arrow(style = north_arrow_nautical(),location = "tr", height = unit(1, "cm"), width = unit(1,"cm") )+
  annotation_scale(height = unit(0.3,"cm"), style = "tick", text_cex = 1)+xlim(-49.3,-49)+ylim(-5.45,-5.20)


vsa_2022 <- ggplot()+
  geom_sf(data = ivs, aes(fill = categoria_VSA_2022), col = 'grey50')+
  geom_sf(data = RD, col = "transparent", fill = "cyan")+
  theme_light()+
  labs(title = "(B)", x = "Longitude", y = "Latitude")+
  theme(plot.title = element_text(hjust = 0.10, vjust = -12, size = 17, face = "bold"), 
        legend.position =c(0.14,0.27), 
        legend.title = element_text(size = 7, face = "bold"),
        legend.text = element_text(size = 7),
        axis.title.x = element_text(face = "bold", size = 11), 
        axis.title.y = element_text(face = "bold", size = 11), 
        axis.text.y = element_text(angle = 90, size = 8),
        axis.text.x = element_text(size = 8),
        legend.key.size = unit(0.3,"cm"),
        legend.background = element_blank())+
  #scale_fill_viridis_c(option = "cividis", direction = -1, na.value = "white")+
  #scale_fill_brewer(palette = "Blues", breaks = c("low", "medium","high","no information"))+
  scale_fill_manual(name = paste("Vulnerabilidade","\n", "Socioambiental"),
                  values = c("darkgreen","lightgreen","yellow","orange","red","blue"), 
                  breaks = c("Muito Baixa", "Baixa","Moderada","Alta", "Muito Alta","Sem Exposição", NA), 
                  labels = c("Muito Baixa", "Baixa","Moderada","Alta", "Muito Alta","Sem exposição","Sem informação"),
                  na.value = "gray30",
                  na.translate = T)+
  annotation_north_arrow(style = north_arrow_nautical(),location = "tr", height = unit(1, "cm"), width = unit(1,"cm") )+
  annotation_scale(height = unit(0.3,"cm"), style = "tick", text_cex = 1)+xlim(-49.3,-49)+ylim(-5.45,-5.20)

plot(ivs_2022)


mud_sevi <- ggplot()+
  geom_sf(data = ivs, aes(fill = Mudanca_vsa), col = 'grey50')+
  geom_sf(data = RD, col = "transparent", fill = "cyan")+
  theme_light()+
  labs(title = "(C)", x = "Longitude", y = "Latitude")+
  theme(plot.title = element_text(hjust = 0.10, vjust = -12, size = 17, face = "bold"), 
        legend.position =c(0.14,0.27), 
        legend.title = element_text(size = 7, face = "bold"),
        legend.text = element_text(size = 7),
        axis.title.x = element_text(face = "bold", size = 11), 
        axis.title.y = element_text(face = "bold", size = 11), 
        axis.text.y = element_text(angle = 90, size = 8),
        axis.text.x = element_text(size = 8),
        legend.key.size = unit(0.3,"cm"),
        legend.background = element_blank())+
  #scale_fill_viridis_c(option = "cividis", direction = -1, na.value = "white")+
  #scale_fill_brewer(palette = "Blues", breaks = c("low", "medium","high","no information"))+
  scale_fill_manual(name = paste("Mudança","\n", "socioambiental"),
                  values = c("green","yellow","red", "blue"), 
                  breaks = c("Redução","Estabilidade","Aumento","Sem Exposição",NA), 
                  labels = c("Redução","Estabilidade","Aumento","Sem informação","Sem exposição"),
                  na.value = "gray30",
                  na.translate = T)+
  annotation_north_arrow(style = north_arrow_nautical(),location = "tr", height = unit(1, "cm"), width = unit(1,"cm") )+
  annotation_scale(height = unit(0.3,"cm"), style = "tick", text_cex = 1)+xlim(-49.3,-49)+ylim(-5.45,-5.20)


linha_cima <- plot_grid(vsa_2010, vsa_2022, nrow = 1, ncol = 2)
linha_baixo <- plot_grid(NULL, mud_sevi, NULL, nrow = 1, rel_widths = c(0.5, 1, 0.5))
mapa_final <- plot_grid(linha_cima, linha_baixo, nrow = 2)
print(mapa_final)

ggsave(filename = 'Processamento de Info/VSA e GP/VSA_categorica.jpg', plot = last_plot(), width = 10, height = 8, dpi = 300)
