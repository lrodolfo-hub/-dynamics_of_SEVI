
#habilitar bibliotecas
# enable libraries

library(readxl)
library(dplyr)
library(psych)
library(FactoMineR)
library(factoextra)
library(devtools)

#definir diretorio
# set directory

setwd('D:\\rodolfo\\Area de trabalho\\espacialização\\Processamento de Info')

# LEitura das informações
# Reading the information

df_2010_har <- read_xlsx('inter_dasi_2010.xlsx', sheet = 2, col_names = TRUE)
df_2022 <- read_xls('attribute_2022 - Copy.xls', sheet = 3, col_names = TRUE)

# Unificação das informações
# Unification of information

df_unif <- rbind(df_2010_har,df_2022)

# analise de normalidade dos parametros
# normality analysis of the parameters

func <- function(x){shapiro.test(x)$p.value}
resultados <- sapply(df_unif, func )
print(resultados)

# Aplicação de transformação aos dados aproximando a uma distribuição normal segundo o criterio Box-Cox (https://web-apps.thecoatlessprofessor.com/data/box-cox-explorer.html)
# Applying a transformation to the data to approximate a normal distribution, according to the Box-Cox criterion

transfor_raiz <- df_unif |> select(c("Ana","DRGA","DRGE","DECA","DCL","DLL","PHE")) |> sapply(function(x) sqrt(x)) |> as.data.frame()
transfor_power <- df_unif |> select("Dep") |> sapply(function(x) x^1.48) |> as.data.frame()

# junção dos dados depois de transformados
# joining of the data after transformation

df_unif_transfo <- cbind(transfor_raiz,transfor_power)

# Analise de normalidade dos parametros transformados
# Normality analysis of the transformed parameters

resultados <- sapply(df_unif_transfo, func )
print(resultados)

# analise de adequabilidade a PCA
# suitability analysis for PCA

corr <- cor(df_unif_transfo)
KMO(corr)
cortest.bartlett(corr, n = nrow(df_unif_transfo))

# aplicação da PCA
# application of PCA

res.pca <- PCA(X = df_unif_transfo, scale.unit = TRUE, ncp = 8)

# Exibe um resumo com a proporção da variância explicada
# Displays a summary with the proportion of explained variance

res.pca$eig

# Exibe as cargas (loadings) dos componentes principais
# Displays the loadings of the principal components

coordenadas <- res.pca$var$coord
std <- sqrt(res.pca$eig[,1])
cargas <-  sweep(coordenadas, 2, std, FUN = "/")
print(cargas[,1])

# Plota o gráfico de sedimentação (scree plot)
# Plot the scree plot

fviz_screeplot(res.pca,bchoice = 'variance', addlabel = T,xlab = 'principal component', ggtheme = theme_factoextra())+
    theme(axis.title = element_text(size = 16))

# Exibe os escores para cada observação
# Displays the scores for each observation

res.var = get_pca_var(res.pca)
res.var$cor[,1]
res.var$coord[,1]

# BiPlot das colunas
# Biplot of the columns

fviz_pca_var(X =res.pca, col.var = 'contrib', gradient.cols =c("red","orange","black"),
             repel = T, xlab='PC1 (46,98%)', ylab ='PC2 (16,26%)',ggtheme = theme_light(), 
             legend.title = "Contribuição (%)", title = "Analise de componentes principais")+
             theme(plot.title = element_text(hjust=0.5), legend.title = element_text(size = 12))

# salvar scores
# save scores

library(writexl)
scores <- get_pca_ind(res.pca)
vs <- as.data.frame(scores$coord)
write_xlsx(x = vs, path = "PCA/IVS.xlsx")
