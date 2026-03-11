# clear the workspace
rm(list = ls()) # clear the workspace

#Lendo pacotes necessários
library(tidyverse)
library(readxl)
library(writexl)
library(reshape2)
library(forcats)
library(sf)
library(flextable)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(broom)
library(spdep)
library(spData)
#install.packages("spData")
#install.packages("stargazer")

# Get and set working directory
setwd("C:/Users/makaw/Meus_Documentos/CEBRAP/Marcel/Dados/TABNET/TABNET_SP")

### Carregar bases

### geral
tb_sp_geral <- read_excel(  
  paste0("tb_sp_geral.xlsx")) %>%  
  data.frame()
colnames(tb_sp_geral)[1] <- "DA"
colnames(tb_sp_geral) <- gsub("^X", "", colnames(tb_sp_geral))
tb_sp_geral <- melt(tb_sp_geral, id.vars = c("DA")) %>%
  rename(Ano = variable, Casos = value)

### aids
tb_sp_aids <- read_excel(  
  paste0("tb_sp_aids.xlsx")) %>%  
  data.frame()
colnames(tb_sp_aids)[1] <- "DA"
colnames(tb_sp_aids) <- gsub("^X", "", colnames(tb_sp_aids))
tb_sp_aids <- melt(tb_sp_aids, id.vars = c("DA")) %>%
  rename(Ano = variable, aids = value)

### db
tb_sp_db <- read_excel(  
  paste0("tb_sp_db.xlsx")) %>%  
  data.frame()
colnames(tb_sp_db)[1] <- "DA"
colnames(tb_sp_db) <- gsub("^X", "", colnames(tb_sp_db))
tb_sp_db <- melt(tb_sp_db, id.vars = c("DA")) %>%
  rename(Ano = variable, db = value)

### drogas
tb_sp_drogas <- read_excel(  
  paste0("tb_sp_drogas.xlsx")) %>%  
  data.frame()
colnames(tb_sp_drogas)[1] <- "DA"
colnames(tb_sp_drogas) <- gsub("^X", "", colnames(tb_sp_drogas))
tb_sp_drogas <- melt(tb_sp_drogas, id.vars = c("DA")) %>%
  rename(Ano = variable, drogas = value)

### fx20_59
tb_sp_fx20_59 <- read_excel(  
  paste0("tb_sp_fx20_59.xlsx")) %>%  
  data.frame()
colnames(tb_sp_fx20_59)[1] <- "DA"
colnames(tb_sp_fx20_59) <- gsub("^X", "", colnames(tb_sp_fx20_59))
tb_sp_fx20_59 <- melt(tb_sp_fx20_59, id.vars = c("DA")) %>%
  rename(Ano = variable, fx20_59 = value)

### fx60+
tb_sp_fx60 <- read_excel(  
  paste0("tb_sp_fx60+.xlsx")) %>%  
  data.frame()
colnames(tb_sp_fx60)[1] <- "DA"
colnames(tb_sp_fx60) <- gsub("^X", "", colnames(tb_sp_fx60))
tb_sp_fx60 <- melt(tb_sp_fx60, id.vars = c("DA")) %>%
  rename(Ano = variable, fx60 = value)

### hiv
tb_sp_hiv <- read_excel(  
  paste0("tb_sp_hiv.xlsx")) %>%  
  data.frame()
colnames(tb_sp_hiv)[1] <- "DA"
colnames(tb_sp_hiv) <- gsub("^X", "", colnames(tb_sp_hiv))
tb_sp_hiv <- melt(tb_sp_hiv, id.vars = c("DA")) %>%
  rename(Ano = variable, hiv = value)

### pll
tb_sp_pll <- read_excel(  
  paste0("tb_sp_pll.xlsx")) %>%  
  data.frame()
colnames(tb_sp_pll)[1] <- "DA"
colnames(tb_sp_pll) <- gsub("^X", "", colnames(tb_sp_pll))
tb_sp_pll <- melt(tb_sp_pll, id.vars = c("DA")) %>%
  rename(Ano = variable, pll = value)

### sx_masc
tb_sp_sx_masc <- read_excel(  
  paste0("tb_sp_sx_masc.xlsx")) %>%  
  data.frame()
colnames(tb_sp_sx_masc)[1] <- "DA"
colnames(tb_sp_sx_masc) <- gsub("^X", "", colnames(tb_sp_sx_masc))
tb_sp_sx_masc <- melt(tb_sp_sx_masc, id.vars = c("DA")) %>%
  rename(Ano = variable, sx_masc = value)

### db
tb_sp_trat_superv <- read_excel(  
  paste0("tb_sp_trat_superv.xlsx")) %>%  
  data.frame()
colnames(tb_sp_trat_superv)[1] <- "DA"
colnames(tb_sp_trat_superv) <- gsub("^X", "", colnames(tb_sp_trat_superv))
tb_sp_trat_superv <- melt(tb_sp_trat_superv, id.vars = c("DA")) %>%
  rename(Ano = variable, trat_superv = value) %>% mutate(trat_superv = as.numeric(trat_superv))

### pop
tb_sp_pop <- read_excel(  
  paste0("pop_sp_da.xlsx")) %>%  
  data.frame() %>% mutate(Ano = as.factor(Ano)) %>%
  mutate(DA = case_when(
    DA == "SOCORRO" ~ "CAPELA DO SOCORRO",
    DA == "MOÓCA" ~ "MOOCA",
    TRUE ~ DA))

### GeoSES
geoses_da <- read_excel(  
  paste0("GeoSES_MSP_Distritos.xlsx")) %>% select (DA, GeoSES) %>%
  mutate(DA = case_when(
    DA == "SOCORRO" ~ "CAPELA DO SOCORRO",
    TRUE ~ DA))


####################################
###### base geral + calculos
####################################
tb_sp_geral <- tb_sp_geral %>% left_join(tb_sp_pop) %>%
  mutate(Inc = (Casos/PopTotal)*100000) %>% 
  left_join(tb_sp_aids) %>% mutate(prop_aids = (aids/Casos)*100) %>%
  left_join(tb_sp_db) %>% mutate(prop_db = (db/Casos)*100) %>%
  left_join(tb_sp_drogas) %>% mutate(prop_drogas = (drogas/Casos)*100) %>%
  left_join(tb_sp_fx20_59) %>% mutate(prop_fx20_59 = (fx20_59/Casos)*100) %>%
  left_join(tb_sp_fx60) %>% mutate(prop_fx60 = (fx60/Casos)*100) %>%
  left_join(tb_sp_hiv) %>% mutate(prop_hiv = (hiv/Casos)*100) %>%
  left_join(tb_sp_pll) %>% mutate(prop_pll = (pll/Casos)*100) %>%
  left_join(tb_sp_sx_masc) %>% mutate(prop_sx_masc = (sx_masc/Casos)*100) %>%
  left_join(tb_sp_trat_superv) %>% mutate(prop_trat_superv = (trat_superv/Casos)*100)
  
  
########################################
###### Construção de Tabela Apresentação
########################################

tb_sp_format <- tb_sp_geral %>% 
  select(DA, Ano, Casos, PopTotal, Inc, prop_aids, prop_db, prop_drogas, prop_hiv, 
         prop_fx20_59, prop_fx60, prop_sx_masc, prop_trat_superv)
#write_xlsx(tb_sp_format,"tb_sp_format.xlsx", col_names=TRUE) #Criando XLSX

####################################
###### Mapas
####################################

## Carregando arquivos de mapa
crs <- st_crs("+proj=longlat +datum=WGS84")  # Defina o CRS apropriado para seus dados
dist <- st_read("Distritos.gpkg", crs =crs)

## Ajustes de dados Incidência Tuberculose
dist <- dist %>% rename(DA = AA_modelo1_DA_NOME)
dist <- dist %>% mutate(DA = str_replace(DA, "SACOMã", "SACOMÃ")) %>% 
  mutate(DA = str_replace(DA, "SOCORRO", "CAPELA DO SOCORRO"))

###### Mapas Facetados

# Anos de interesse
anos_selecionados <- c(2009, 2015, 2019, 2024)

# Criando a base com múltiplos anos
dist_anos <- dist %>%
  left_join(tb_sp_geral %>% filter(Ano %in% anos_selecionados) %>% 
      select(DA, Ano, Inc, prop_aids, prop_db, prop_fx20_59, prop_fx60, prop_hiv,
             prop_pll, prop_sx_masc, prop_trat_superv, prop_drogas), by = "DA")


### Mapa Incidência Geral Tuberculose
map_inc_geral <- ggplot(data = dist_anos) +
  geom_sf(aes(fill = Inc), color = NA) +
  geom_sf(data = st_cast(dist_anos, "MULTILINESTRING"), color = "black", size = 0.1) +
  facet_wrap(~ Ano, ncol = 4) + 
  scale_fill_gradient(low = "white", high = "brown", na.value = "grey90") +
  labs(fill = "Incidência de Tuberculose por 100.000 hab") +
  coord_sf(datum = NA) +
  theme_void() + 
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    legend.position = "bottom",
    legend.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    strip.text = element_text(size = 12, face = "bold")) + # Estilo do título de cada mapa (Ano)
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 15))
#Salvar o gráfico em PNG
ggsave("Mapas/map_inc_geral.png", plot = map_inc_geral, width = 10, height = 5.5, dpi = 300, device = "png")


### Mapa Proporção Drogas
map_prop_drogas <- ggplot(data = dist_anos) +
  geom_sf(aes(fill = prop_drogas), color = NA) +
  geom_sf(data = st_cast(dist_anos, "MULTILINESTRING"), color = "black", size = 0.1) +
  facet_wrap(~ Ano, ncol = 4) + 
  scale_fill_gradient(low = "white", high = "darkblue", na.value = "grey90") +
  labs(fill = "Proporção de Usuário de Drogas por Total de Casos(%)") +
  coord_sf(datum = NA) +
  theme_void() + 
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    legend.position = "bottom",
    legend.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    strip.text = element_text(size = 12, face = "bold")) + # Estilo do título de cada mapa (Ano)
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 15))
#Salvar o gráfico em PNG
ggsave("Mapas/map_prop_drogas.png", plot = map_prop_drogas, width = 10, height = 5.5, dpi = 300, device = "png")


### Mapa Proporção db
map_prop_db <- ggplot(data = dist_anos) +
  geom_sf(aes(fill = prop_db), color = NA) +
  geom_sf(data = st_cast(dist_anos, "MULTILINESTRING"), color = "black", size = 0.1) +
  facet_wrap(~ Ano, ncol = 4) + 
  scale_fill_gradient(low = "white", high = "#2E8B57", na.value = "grey90") +
  labs(fill = "Proporção Diabetes por Total de Casos(%)") +
  coord_sf(datum = NA) +
  theme_void() + 
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    legend.position = "bottom",
    legend.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    strip.text = element_text(size = 12, face = "bold")) + # Estilo do título de cada mapa (Ano)
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 15))
#Salvar o gráfico em PNG
ggsave("Mapas/map_prop_db.png", plot = map_prop_db, width = 10, height = 5.5, dpi = 300, device = "png")


### Mapa Proporção fx20_59
map_prop_fx20_59 <- ggplot(data = dist_anos) +
  geom_sf(aes(fill = prop_fx20_59), color = NA) +
  geom_sf(data = st_cast(dist_anos, "MULTILINESTRING"), color = "black", size = 0.1) +
  facet_wrap(~ Ano, ncol = 4) + 
  scale_fill_gradient(low = "white", high = "#556B2F", na.value = "grey90") +
  labs(fill = "Proporção Faixa Etária 20-59 anos por Total de Casos(%)") +
  coord_sf(datum = NA) +
  theme_void() + 
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    legend.position = "bottom",
    legend.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    strip.text = element_text(size = 12, face = "bold")) + # Estilo do título de cada mapa (Ano)
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 15))
#Salvar o gráfico em PNG
ggsave("Mapas/map_prop_fx20_59.png", plot = map_prop_fx20_59, width = 10, height = 5.5, dpi = 300, device = "png")


### Mapa Proporção fx60
map_prop_fx60 <- ggplot(data = dist_anos) +
  geom_sf(aes(fill = prop_fx60), color = NA) +
  geom_sf(data = st_cast(dist_anos, "MULTILINESTRING"), color = "black", size = 0.1) +
  facet_wrap(~ Ano, ncol = 4) + 
  scale_fill_gradient(low = "white", high = "#4B0082", na.value = "grey90") +
  labs(fill = "Proporção Faixa Etária 60+ anos por Total de Casos(%)") +
  coord_sf(datum = NA) +
  theme_void() + 
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    legend.position = "bottom",
    legend.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    strip.text = element_text(size = 12, face = "bold")) + # Estilo do título de cada mapa (Ano)
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 15))
#Salvar o gráfico em PNG
ggsave("Mapas/map_prop_fx60.png", plot = map_prop_fx60, width = 10, height = 5.5, dpi = 300, device = "png")


### Mapa Proporção sx_masc
map_prop_sx_masc <- ggplot(data = dist_anos) +
  geom_sf(aes(fill = prop_sx_masc), color = NA) +
  geom_sf(data = st_cast(dist_anos, "MULTILINESTRING"), color = "black", size = 0.1) +
  facet_wrap(~ Ano, ncol = 4) + 
  scale_fill_gradient(low = "white", high = "#2F4F4F", na.value = "grey90") +
  labs(fill = "Proporção Sexo Masculino por Total de Casos(%)") +
  coord_sf(datum = NA) +
  theme_void() + 
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    legend.position = "bottom",
    legend.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    strip.text = element_text(size = 12, face = "bold")) + # Estilo do título de cada mapa (Ano)
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 15))
#Salvar o gráfico em PNG
ggsave("Mapas/map_prop_sx_masc.png", plot = map_prop_sx_masc, width = 10, height = 5.5, dpi = 300, device = "png")


### Mapa Proporção trat_superv
map_prop_trat_superv <- ggplot(data = dist_anos) +
  geom_sf(aes(fill = prop_trat_superv), color = NA) +
  geom_sf(data = st_cast(dist_anos, "MULTILINESTRING"), color = "black", size = 0.1) +
  facet_wrap(~ Ano, ncol = 4) + 
  scale_fill_gradient(low = "white", high = "#A0522D", na.value = "grey90") +
  labs(fill = "Proporção Pacientes em Trat Superv. por Total de Casos(%)") +
  coord_sf(datum = NA) +
  theme_void() + 
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    legend.position = "bottom",
    legend.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    strip.text = element_text(size = 12, face = "bold")) + # Estilo do título de cada mapa (Ano)
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 15))
#Salvar o gráfico em PNG
ggsave("Mapas/map_prop_trat_superv.png", plot = map_prop_trat_superv, width = 10, height = 5.5, dpi = 300, device = "png")


### Mapa Proporção hiv
map_prop_hiv <- ggplot(data = dist_anos) +
  geom_sf(aes(fill = prop_hiv), color = NA) +
  geom_sf(data = st_cast(dist_anos, "MULTILINESTRING"), color = "black", size = 0.1) +
  facet_wrap(~ Ano, ncol = 4) + 
  scale_fill_gradient(low = "white", high = "#2F4F4F", na.value = "grey90") +
  labs(fill = "Proporção Sexo Masculino por Total de Casos(%)") +
  coord_sf(datum = NA) +
  theme_void() + 
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    legend.position = "bottom",
    legend.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    strip.text = element_text(size = 12, face = "bold")) + # Estilo do título de cada mapa (Ano)
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 15))
#Salvar o gráfico em PNG
ggsave("Mapas/map_prop_hiv.png", plot = map_prop_hiv, width = 10, height = 5.5, dpi = 300, device = "png")


##############################
### Estatistica Espacial
##############################

### Preparação dos Dados e Matriz de Vizinhança
# Filtrar dados de 2024 e remover NAs na Incidência
dist_2024 <- dist_anos %>% filter(Ano == 2024) %>% select(DA, Inc, geom) 
# Criar a lista de vizinhos (Critério de Rainha/Queen)
vizinhos <- poly2nb(dist_2024, queen = TRUE, snap = 0.001)
# Criar a matriz de pesos espaciais (estilo 'W' - padronizada pela linha)
pesos <- nb2listw(vizinhos, style = "W", zero.policy = TRUE)

### Cálculo Moran
moran_global <- moran.test(dist_2024$Inc, pesos, zero.policy = TRUE)
print(moran_global)

### Análise LISA (Local Indicators of Spatial Association)
# Desativar o uso de geometria esférica (S2) - Isso evita o erro "Edge crosses"
sf_use_s2(FALSE)

# 2. Carregar e Limpar os dados
dist_2024 <- dist %>%
  left_join(tb_sp_geral %>% filter(Ano == 2024) %>% select(DA, Inc), by = "DA") %>%
  filter(!is.na(Inc)) %>%
  st_make_valid() %>%            # Conserta auto-interseções
  st_buffer(dist = 0)            # Truque extra para fechar polígonos abertos

# 3. Recriar Pesos (usando snap para ignorar micro-erros de borda)
vizinhos <- poly2nb(dist_2024, queen = TRUE, snap = 0.001)
pesos <- nb2listw(vizinhos, style = "W", zero.policy = TRUE)

# 4. Calcular LISA
lisa_res <- localmoran(dist_2024$Inc, pesos, zero.policy = TRUE)

# 5. Organizar as colunas para o plot
dist_2024 <- dist_2024 %>%
  mutate(
    p_val = lisa_res[, 5],
    Z_inc = as.numeric(scale(Inc)),
    lag_inc = lag.listw(pesos, Inc, zero.policy = TRUE),
    Z_lag = as.numeric(scale(lag_inc)),
    lisa_cluster = case_when(
      p_val > 0.05 ~ "Não Significante",
      Z_inc > 0 & Z_lag > 0 ~ "Alto-Alto",
      Z_inc < 0 & Z_lag < 0 ~ "Baixo-Baixo",
      Z_inc > 0 & Z_lag < 0 ~ "Alto-Baixo",
      Z_inc < 0 & Z_lag > 0 ~ "Baixo-Alto"
    )
  )

# 6. Plotagem Limpa
map_lisa <- ggplot(data = dist_2024) +
  geom_sf(aes(fill = lisa_cluster), color = "black", size = 0.1) +
  scale_fill_manual(
    name = "Clusters Espaciais de Incidência de Tuberculose",
    values = c("Alto-Alto" = "red", 
               "Baixo-Baixo" = "blue", 
               "Alto-Baixo" = "pink", 
               "Baixo-Alto" = "lightblue", 
               "Não Significante" = "white"), na.value = "white") +
  coord_sf(datum = NA) +
  theme_void() +
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    legend.position = "bottom",
    legend.title = element_text(size = 8, hjust = 0.5),
    legend.text = element_text(size = 8),
    legend.key.width = unit(1.5, "cm"),
    legend.title.align = 0.5,   # centraliza o título
    strip.text = element_text(size = 8)) + 
  guides(fill = guide_legend(title.position = "top"))
#Salvar o gráfico em PNG
ggsave("Mapas/map_lisa.png", plot = map_lisa, width = 6, height = 5, dpi = 300, device = "png")




####################################
###### Gráficos
####################################

boxplot_inc_geral <- ggplot(tb_sp_geral, aes(x = factor(Ano), y = Inc)) +
  geom_boxplot() +
  labs(x = "Ano", y = "Incidência Tuberculose por 100.000 hab.", title = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6))
## Salvar o gráfico em PNG
ggsave("Graf/boxplot_inc_geral.png", plot = boxplot_inc_geral, width = 6, 
       height = 4, dpi = 300, device = "png")

## Gráfico de verificação de normalidade
qqnorm(tb_sp_geral$Inc) #qqplot
qqline(tb_sp_geral$Inc, col = "red")

## Tabela por DA
tabela_DA <- tb_sp_geral %>% #Processamento Tabelas de Dispersão
  group_by(DA) %>%
  summarize(n_obs = n(),
            Inc = mean(Inc),
            prop_aids = mean(prop_aids),
            prop_db = mean(prop_db),
            prop_drogas = mean(prop_drogas),
            prop_fx20_59 = mean(prop_fx20_59),
            prop_fx60 = mean(prop_fx60),
            prop_hiv = mean(prop_hiv),
            prop_sx_masc = mean(prop_sx_masc),
            prop_trat_superv = mean(prop_trat_superv)) %>%
  left_join(geoses_da) %>% filter (!DA == "EM BRANCO")

## Gráfico de dispersão por GeoSES
ggplot(data = tabela_DA, aes(x = GeoSES, y = Inc)) + # Informa os dados a serem utilizadps
  geom_point() + # Informa que eu quero um gráfico de dispersão.
  ylab("Incidência Tuberculose por 100.000 hab.") +
  xlab("GeoSES")

ggplot(data = tabela_DA, aes(x = GeoSES, y = prop_fx60)) + # Informa os dados a serem utilizadps
  geom_point() + # Informa que eu quero um gráfico de dispersão.
  ylab("Proporção Faixa 60+") +
  xlab("GeoSES")

ggplot(data = tabela_DA, aes(x = GeoSES, y = prop_db)) + # Informa os dados a serem utilizadps
  geom_point() + # Informa que eu quero um gráfico de dispersão.
  ylab("Proporção Diabetes") +
  xlab("GeoSES")

ggplot(data = tabela_DA, aes(x = GeoSES, y = prop_hiv)) + # Informa os dados a serem utilizadps
  geom_point() + # Informa que eu quero um gráfico de dispersão.
  ylab("Proporção HIV") +
  xlab("GeoSES")

ggplot(data = tabela_DA, aes(x = GeoSES, y = prop_trat_superv)) + # Informa os dados a serem utilizadps
  geom_point() + # Informa que eu quero um gráfico de dispersão.
  ylab("Proporção Tratamento Superv.") +
  xlab("GeoSES")

ggplot(data = tabela_DA, aes(x = GeoSES, y = prop_sx_masc)) + # Informa os dados a serem utilizadps
  geom_point() + # Informa que eu quero um gráfico de dispersão.
  ylab("Proporção Sexo Masculino") +
  xlab("GeoSES")



########################################
###### Análise de Dados em Painel
########################################

### Preparação Dados para análise de dados em painel
tb_sp_limpa <- tb_sp_geral %>% filter(!DA == "EM BRANCO")%>%
  mutate(across(c(prop_db, prop_drogas, prop_fx20_59, prop_hiv, prop_sx_masc, 
                  prop_trat_superv), ~replace_na(., 0)))
p_data <- pdata.frame(tb_sp_limpa, index = c("DA", "Ano"))


################
## Modelo 1: Apenas efeito de Distrito (Individual)
model_within_ind <- plm(Inc ~ prop_drogas + prop_fx20_59 + 
                          prop_hiv + prop_sx_masc + prop_trat_superv, 
                        data = p_data, model = "within", effect = "individual")
summary(model_within_ind)

## Modelo 2: Efeito de Distrito + Efeito de Ano (Two-Way)
model_within_2way <- plm(Inc ~ prop_db + prop_drogas + prop_fx20_59 + 
                           prop_hiv + prop_sx_masc + prop_trat_superv, 
                         data = p_data, model = "within", effect = "twoways")
summary(model_within_2way)

# D. Random Effects (RE)
model_re <- plm(Inc ~ prop_db + prop_drogas + prop_fx20_59 + 
                  prop_hiv + prop_sx_masc + prop_trat_superv, 
                data = p_data, model = "random")

# Testes de Decisão
# Hausman Test: Se p < 0.05, use Efeitos Fixos (model_within_ind ou model_within_twoway)
phtest(model_within_ind, model_re)

## Gerar a tabela comparativa
stargazer(model_within_ind, model_within_2way, 
          type = "text", 
          column.labels = c("Fixed Effects",  "Two-Way Fixed Effects"),
          model.names = FALSE,
          main = "Comparação: O Impacto do Fator Tempo na TB em SDadosP")

stargazer(model_within_ind, model_within_2way,  #exportação para word
          type = "html", 
          out = "resultado_tb.doc",
          column.labels = c("Fixed Effects",  "Two-Way Fixed Effects"),
          model.names = FALSE,
          main = "Comparação: O Impacto do Fator Tempo na TB em SDadosP")

## Para ver os efeitos fixos de cada ANO (as "dummies" de tempo)
fixef(model_within_2way, effect = "time")
## Para ver os efeitos fixos de cada DISTRITO (as "dummies" de unidade)
fixef(model_within_2way, effect = "individual")


###### Gráfico de Coeficientes (Forest Plot)
# Extrair coeficientes dos dois modelos
coef_ind <- tidy(model_within_ind) %>% mutate(modelo = "Efeito Distrito")
coef_2way <- tidy(model_within_2way) %>% mutate(modelo = "Distrito + Ano (2-Way)")
# Juntar e plotar
df_coef <- rbind(coef_ind, coef_2way)
# Plotagem
ggplot(df_coef, aes(x = estimate, y = term, color = modelo)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, 
                     xmax = estimate + 1.96*std.error), 
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(subtitle = "Intervalo de Confiança de 95%",
       x = "Estimativa do Coeficiente", y = "Variável") +
  theme_minimal() +
  scale_color_manual(values = c("steelblue", "firebrick"))

###### Gráfico de Tendência dos Efeitos de Ano (Dummies de Tempo)
# Extrair os efeitos de tempo do Modelo 2
eff_time <- fixef(model_within_2way, effect = "time")
df_time <- data.frame(Ano = as.numeric(names(eff_time)), 
                      Efeito = as.numeric(eff_time))
# Plotagem do Gráfico
ggplot(df_time, aes(x = Ano, y = Efeito)) +
  geom_line(color = "firebrick", size = 1) +
  geom_point(size = 3, color = "firebrick") +
  geom_area(fill = "firebrick", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  #annotate("text", x = 2021, y = min(df_time$Efeito), label = "Impacto Pandemia?", 
  #         vjust = -1, color = "gray30") +
  labs(title = "Efeito Isolado do Tempo na Incidência de TB",
       subtitle = "Fatores sistêmicos de SP que afetaram todos os distritos",
       x = "Ano", y = "Ajuste na Incidência") +
  theme_minimal()

###### Gráfico de Slopegraph de Sensibilidade (Antes vs. Depois)
df_slope <- df_coef %>%
  filter(term %in% c("prop_drogas", "prop_trat_superv"))
# Plotagem
ggplot(df_slope, aes(x = modelo, y = estimate, group = term, color = term)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-0.1, 0.15, 0.05)) +
  labs(title = "Sensibilidade dos Principais Preditores",
       subtitle = "Como o controle temporal altera a percepção do tratamento",
       x = "Configuração do Modelo", y = "Coeficiente Estimado") +
  theme_classic()


     