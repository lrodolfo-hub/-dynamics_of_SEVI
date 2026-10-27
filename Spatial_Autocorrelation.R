# ============================================================
# ANALISE DE AUTOCORRELACAO ESPACIAL (SPATIAL AUTOCORRELATION ANALYSIS)
# MORAN GLOBAL E MORAN LOCAL (LISA) (GLOBAL MORAN’S I AND LOCAL MORAN’S I (LISA))
# ============================================================

# INSTALAR E CARREGAR PACOTES ------------------------------
# INSTALL AND LOAD PACKAGES------------------------------
install.packages("spdep")
library(sf)
library(spdep)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)


# IMPORTAR A BASE ESPACIAL ---------------------------------
# IMPORT THE SPACE BASE ---------------------------------
# To Shapefile:

arquivo <- "D:/rodolfo/Area de trabalho/espacialização/Malha_Sencitaria_2022.shp"

# To GeoPackage, use st_read() with the patch .gpkg.
ms <- st_read(arquivo, quiet = FALSE)

# Verificar estrutura da base (Check the base structure)
print(dados)
print(names(dados))
print(st_crs(dados))

# Leitura da base de dados
# Reading the database
df <- read_xlsx("C:\\Users\\lrodo\\OneDrive\\Desktop\\IVS.xlsx", sheet = 3,col_names = T) |> 
         rename(CD_SETOR = CD_GEOCODI)|> 
         mutate(CD_SETOR = as.character(CD_SETOR)) |>
         na.omit(Pexp_2010)

# remoção dos setores sem exposição (para análise do cenário de impacto) (removal of sectors with no exposure (for impact scenario analysis))
ms_impacto <- filter(df, Pexp_2010 != 0) 

# aplicaçao de join (join application)
ms_join <- right_join(ms, df,by = "CD_SETOR")

# PREPARAR OS DADOS ----------------------------------------
# PREPARE THE DATA ----------------------------------------

# Corrigir geometrias inválidas (Fix invalid geometries)
ms_join <- st_make_valid(ms_join)

# Verificar duplicidade de identificadores (Check for duplicate identifiers)
# unique(dados$CD_SETOR)

# Verificar se a variável possui variabilidade (Check whether the variable has variability)
x <- ms_join[["VSA_2022"]]

if (length(x) < 3 || sd(x) == 0) {
  stop("Dados insuficientes ou variavel sem variabilidade.")
}

cat("Numero de unidades:", length(x), "\n")
cat("Media:", mean(x), "\n")
cat("Desvio-padrao:", sd(x), "\n")


# CONSTRUIR A MATRIZ DE VIZINHANCA -------------------------
# CONSTRUCT THE NEIGHBORHOOD MATRIX -------------------------

# Vizinhança Queen: compartilha borda ou vértice (Queen's neighborhood: shares an edge or a vertex)
vizinhos <- poly2nb(
  ms_join,
  queen = TRUE,
  row.names = seq_len(nrow(ms_join))
)

# Verificar unidades sem vizinhos (Check for units with no neighbors)
sem_vizinhos <- which(card(vizinhos) == 0)

cat("Unidades sem vizinhos:", length(sem_vizinhos), "\n")

# Matriz de pesos espaciais padronizada por linha (Row-standardized spatial weights matrix)
pesos <- nb2listw(
  vizinhos,
  style = "W",
  zero.policy = TRUE
)


# 5. MORAN GLOBAL ---------------------------------------------

# Teste analítico (Analytical test)
moran_global <- moran.test(
  x,
  listw = pesos,
  zero.policy = TRUE,
  alternative = "two.sided"
)

print(moran_global)

# Teste de significância por permutação (Permutation significance test)
set.seed(123)

moran_permutacao <- moran.mc(
  x,
  listw = pesos,
  nsim = 999,
  zero.policy = TRUE,
  alternative = "two.sided"
)

print(moran_permutacao)

# Extrair estatística e p-valor (Extract statistic and p-value)
I_global <- as.numeric(moran_global$estimate[1])
p_global <- moran_global$p.value

cat("Moran Global:", I_global, "\n")
cat("P-valor analitico:", p_global, "\n")