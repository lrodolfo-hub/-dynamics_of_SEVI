# Chamada de pacotes necessarios
# Call for necessary packages

library(sf)
library(dplyr)
library(readxl)
library(ggplot2)

# definição do diretório
# directory definition

setwd("D:\\rodolfo\\Area de trabalho\\espacialização")

# leitura dos setores censitarios
# reading of census tracts

setores_2010 <- st_read("malha_censitaria_2010.shp")[,-1]
setores_2022 <- st_read("Malha_Sencitaria_2022.shp")
rio <- st_read("Rios_Duplos.shp")
plot(setores_2010$geometry)
plot(setores_2022$geometry)

# verificação dos tipos de dados
# data type verification

glimpse(setores_2010)
glimpse(setores_2022)

# leitura dos indicadores demograficos/socioeconomicos
# interpretation of demographic/socioeconomic indicators

df <- read_xlsx('Processamento de Info/attribute_2010.xlsx')|> mutate('CD_GEOCODI' = as.character(CD_GEOCODI))

glimpse(df)

# unir parametros de 2010
# combine parameters from 2010

lista <- c('CD_GEOCODI','P','Nm','Mm','Dep','Ana','DRGA','DRGE','DECA','DCL','DLL','PCB','PCPP','RRPC','RNM')
setores_2010_attr <- right_join(x = setores_2010, y = df, by = 'CD_GEOCODI') |> select(all_of(lista))

glimpse(setores_2010_attr)

# Corrigir as geometrias de 2010 e 2022
# Correct the geometries for 2010 and 2022

setores_2010_attr <- st_make_valid(setores_2010_attr)
setores_2022      <- st_make_valid(setores_2022)

#aplicação da interpolaçao dasimetrica
# application of dasymetric interpolation

resultado <- st_interpolate_aw(x = setores_2010_attr[c('P','Nm','Mm','Dep','Ana','DRGA','DRGE','DECA','DCL','DLL','PCB','PCPP','RRPC','RNM')], 
                                to = setores_2022, weights = 'NUMPOINTS', extensive = TRUE)|> mutate('ID_GEOCODI' = setores_2022$CD_SETOR)

# Calcular a soma das variáveis originais de 2010 (removendo a geometria)
# Calculate the sum of the original variables from 2010 (removing the geometry)

soma_2010 <- setores_2010_attr |> 
  st_drop_geometry() |> 
  summarise(across(c(P, Nm, Mm, Dep, Ana, DRGA, DRGE, DECA, DCL, DLL, PCB, PCPP, RRPC, RNM), sum, na.rm = TRUE))

# Calcular a soma das variáveis interpoladas no resultado
# Calculate the sum of the interpolated variables in the result

soma_resultado <- resultado |> 
  st_drop_geometry() |> 
  summarise(across(c(P, Nm, Mm, Dep, Ana, DRGA, DRGE, DECA, DCL, DLL, PCB, PCPP, RRPC, RNM), sum, na.rm = TRUE))

# Juntar os dois resultados para comparar lado a lado
# Combine the two results to compare them side by side

comparacao <- bind_rows(Origem_2010 = soma_2010, Interpolado_2022 = soma_resultado, .id = "Etapa")

# Visualizar a tabela de comparação
# View the comparison table

print(comparacao)

# Trazer o total estimado em 2022 de volta para a geometria de 2010 por proporção de área
# Adjust the 2022 estimated total to the 2010 geometry based on area proportion
# Isso serve para sabermos quanto o R conseguiu alocar de fato dentro de cada setor original
# This helps us determine how much R was actually able to allocate within each original sector

voltou_para_2010 <- st_interpolate_aw(resultado["RNM"], setores_2010_attr, extensive = TRUE) |> rename(RNM_estimado = RNM)

# Junção espacial usando st_join com a condição de igualdade geométrica (st_equals)
# Spatial join using st_join with a geometric equality condition (st_equals)

setores_analise_erro <- setores_2010_attr |> 
  st_join(voltou_para_2010, join = st_equals) |> 
  mutate(
    # Se o setor sumiu no meio do caminho (NA), vira 0 para calcular a perda real
    # If the segment disappeared along the way (NA), it becomes 0 for the purpose of calculating the actual loss.
    RNM_estimado = ifelse(is.na(RNM_estimado), 0, RNM_estimado),
    Diferenca = RNM_estimado - RNM
  )

# Descobrir qual ID de setor foi o culpado pelo sumiço dos dados
# Identify which sector ID was responsible for the data loss

setor_excluido <- setores_analise_erro %>% filter(RNM_estimado == 0)
print(paste("Quantidade de setores descartados:", nrow(setor_excluido)))

# Extrai os limites automáticos de setores_2010
# Extracts the automatic limits for setores_2010

limites <- st_bbox(setores_2010)

mapa_erro <- ggplot(data = setores_analise_erro) +
  geom_sf(aes(fill = Diferenca), color = NA) +
  geom_sf(data = setores_2010, fill = NA, color = 'black', linewidth = 0.2)+
  geom_sf(data = rio, fill = 'lightskyblue', color = NA)+
  coord_sf(xlim = c(limites["xmin"], limites["xmax"]), 
           ylim = c(limites["ymin"], limites["ymax"]), 
           expand = FALSE)+
  scale_fill_gradient2(
    low = "#bd0026",    # Vermelho escuro: Onde houve perda de registros (Dark red: Where records have been lost)
    mid = "#ffffcc",    # Amarelo claro: Próximo de zero (conservação correta) (Light yellow: Close to zero (proper storage))
    high = "#006837",   # Verde escuro: Ganho residual (Dark green: Residual gain)
    midpoint = 0,
    name = paste("Saldo do Rendimento","\n","nominal médio")) +
  theme_minimal() +
  labs( title = "Mapa de Calor de Resíduos da Interpolação Dasimétrica [RNM]")+
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0, margin = margin(b = 10)),
        legend.title = element_text(face = "bold", size = 10))

print(mapa_erro)

ggsave(filename = 'Interpolação_RNM.jpeg', path = "C:\\Users\\lrodo\\OneDrive\\Desktop\\mapas", plot = last_plot(), dpi = 300)

# salvar os resultados da interpolação
# save the interpolation results

install.packages("writexl")
library(writexl)
write_xlsx(x= st_drop_geometry(resultado), path = './Processamento de Info/inter_dasi_2010_2.xlsx', col_names = TRUE)

