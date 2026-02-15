# clear the workspace
rm(list = ls()) # clear the workspace

#Lendo pacotes necessários
library(survey)
library(ggplot2)
library(dplyr)
library(tictoc)
library(foreign)
library(forcats)
library(tidyverse)
library(readxl) # Trabalhar com excel
library(writexl)
library(reshape2)

# Get and set working directory
#setwd("~/Dropbox/Doutorado UFABC/Tese/Artigo_1")
setwd("C:/Users/makaw/Meus_Documentos/CEBRAP/Marcel/Dados/TABNET")

# Carregar bases

### Geral
Base_geral <- read_excel(  
  paste0("tubercbr_geral.xlsx")) %>%  
  data.frame()
colnames(Base_geral)[1] <- "Municipio"
colnames(Base_geral) <- gsub("^X", "", colnames(Base_geral))

### ppl
Base_ppl_sp <- read_excel(  
  paste0("tubercbr_ppl_sp.xlsx")) %>%  
  data.frame()
colnames(Base_ppl_sp)[1] <- "ppl"
colnames(Base_ppl_sp) <- gsub("^X", "", colnames(Base_ppl_sp))
Base_ppl_sp$Municipio <- "São Paulo"
#
Base_ppl_for <- read_excel(  
  paste0("tubercbr_ppl_for.xlsx")) %>%  
  data.frame()
colnames(Base_ppl_for)[1] <- "ppl"
colnames(Base_ppl_for) <- gsub("^X", "", colnames(Base_ppl_for))
Base_ppl_for$Municipio <- "Fortaleza"
#
Base_ppl <- rbind(Base_ppl_for, Base_ppl_sp)

### fxetaria
Base_fxetaria_sp <- read_excel(  
  paste0("tubercbr_fxetaria_sp.xlsx")) %>%  
  data.frame()
colnames(Base_fxetaria_sp)[1] <- "fxetaria"
colnames(Base_fxetaria_sp) <- gsub("^X", "", colnames(Base_fxetaria_sp))
Base_fxetaria_sp$Municipio <- "São Paulo"
#
Base_fxetaria_for <- read_excel(  
  paste0("tubercbr_fxetaria_for.xlsx")) %>%  
  data.frame()
colnames(Base_fxetaria_for)[1] <- "fxetaria"
colnames(Base_fxetaria_for) <- gsub("^X", "", colnames(Base_fxetaria_for))
Base_fxetaria_for$Municipio <- "Fortaleza"
#
Base_fxetaria <- rbind(Base_fxetaria_for, Base_fxetaria_sp)

### forma
Base_forma_sp <- read_excel(  
  paste0("tubercbr_forma_sp.xlsx")) %>%  
  data.frame()
colnames(Base_forma_sp)[1] <- "forma"
colnames(Base_forma_sp) <- gsub("^X", "", colnames(Base_forma_sp))
Base_forma_sp$Municipio <- "São Paulo"
#
Base_forma_for <- read_excel(  
  paste0("tubercbr_forma_for.xlsx")) %>%  
  data.frame()
colnames(Base_forma_for)[1] <- "forma"
colnames(Base_forma_for) <- gsub("^X", "", colnames(Base_forma_for))
Base_forma_for$Municipio <- "Fortaleza"
#
Base_forma <- rbind(Base_forma_for, Base_forma_sp)

### drogas
Base_drogas_sp <- read_excel(  
  paste0("tubercbr_drogas_sp.xlsx")) %>%  
  data.frame()
colnames(Base_drogas_sp)[1] <- "drogas"
colnames(Base_drogas_sp) <- gsub("^X", "", colnames(Base_drogas_sp))
Base_drogas_sp$Municipio <- "São Paulo"
#
Base_drogas_for <- read_excel(  
  paste0("tubercbr_drogas_for.xlsx")) %>%  
  data.frame()
colnames(Base_drogas_for)[1] <- "drogas"
colnames(Base_drogas_for) <- gsub("^X", "", colnames(Base_drogas_for))
Base_drogas_for$Municipio <- "Fortaleza"
#
Base_drogas <- rbind(Base_drogas_for, Base_drogas_sp)

### aids
Base_aids_sp <- read_excel(  
  paste0("tubercbr_aids_sp.xlsx")) %>%  
  data.frame()
colnames(Base_aids_sp)[1] <- "aids"
colnames(Base_aids_sp) <- gsub("^X", "", colnames(Base_aids_sp))
Base_aids_sp$Municipio <- "São Paulo"
#
Base_aids_for <- read_excel(  
  paste0("tubercbr_aids_for.xlsx")) %>%  
  data.frame()
colnames(Base_aids_for)[1] <- "aids"
colnames(Base_aids_for) <- gsub("^X", "", colnames(Base_aids_for))
Base_aids_for$Municipio <- "Fortaleza"
#
Base_aids <- rbind(Base_aids_for, Base_aids_sp)

### beneficio
Base_beneficio_sp <- read_excel(  
  paste0("tubercbr_beneficio_sp.xlsx")) %>%  
  data.frame()
colnames(Base_beneficio_sp)[1] <- "beneficio"
colnames(Base_beneficio_sp) <- gsub("^X", "", colnames(Base_beneficio_sp))
Base_beneficio_sp$Municipio <- "São Paulo"
#
Base_beneficio_for <- read_excel(  
  paste0("tubercbr_beneficio_for.xlsx")) %>%  
  data.frame()
colnames(Base_beneficio_for)[1] <- "beneficio"
colnames(Base_beneficio_for) <- gsub("^X", "", colnames(Base_beneficio_for))
Base_beneficio_for$Municipio <- "Fortaleza"
#
Base_beneficio <- rbind(Base_beneficio_for, Base_beneficio_sp)

### alcool
Base_alcool_sp <- read_excel(  
  paste0("tubercbr_alcool_sp.xlsx")) %>%  
  data.frame()
colnames(Base_alcool_sp)[1] <- "alcool"
colnames(Base_alcool_sp) <- gsub("^X", "", colnames(Base_alcool_sp))
Base_alcool_sp$Municipio <- "São Paulo"
#
Base_alcool_for <- read_excel(  
  paste0("tubercbr_alcool_for.xlsx")) %>%  
  data.frame()
colnames(Base_alcool_for)[1] <- "alcool"
colnames(Base_alcool_for) <- gsub("^X", "", colnames(Base_alcool_for))
Base_alcool_for$Municipio <- "Fortaleza"
#
Base_alcool <- rbind(Base_alcool_for, Base_alcool_sp)


### diabetes
Base_diabetes_sp <- read_excel(  
  paste0("tubercbr_diabetes_sp.xlsx")) %>%  
  data.frame()
colnames(Base_diabetes_sp)[1] <- "diabetes"
colnames(Base_diabetes_sp) <- gsub("^X", "", colnames(Base_diabetes_sp))
Base_diabetes_sp$Municipio <- "São Paulo"
#
Base_diabetes_for <- read_excel(  
  paste0("tubercbr_diabetes_for.xlsx")) %>%  
  data.frame()
colnames(Base_diabetes_for)[1] <- "diabetes"
colnames(Base_diabetes_for) <- gsub("^X", "", colnames(Base_diabetes_for))
Base_diabetes_for$Municipio <- "Fortaleza"
#
Base_diabetes <- rbind(Base_diabetes_for, Base_diabetes_sp)

### sexo
Base_sexo_sp <- read_excel(  
  paste0("tubercbr_sexo_sp.xlsx")) %>%  
  data.frame()
colnames(Base_sexo_sp)[1] <- "sexo"
colnames(Base_sexo_sp) <- gsub("^X", "", colnames(Base_sexo_sp))
Base_sexo_sp$Municipio <- "São Paulo"
#
Base_sexo_for <- read_excel(  
  paste0("tubercbr_sexo_for.xlsx")) %>%  
  data.frame()
colnames(Base_sexo_for)[1] <- "sexo"
colnames(Base_sexo_for) <- gsub("^X", "", colnames(Base_sexo_for))
Base_sexo_for$Municipio <- "Fortaleza"
#
Base_sexo <- rbind(Base_sexo_for, Base_sexo_sp)

### Pop
Base_pop <- read_excel(  
  paste0("ibge_pop.xlsx")) %>%  
  data.frame()
colnames(Base_pop) <- gsub("^X", "", colnames(Base_pop))

### Pop sexo
Base_pop_sexo_sp <- read_excel(  
  paste0("ibge_pop_sexo_sp.xlsx")) %>%  
  data.frame()
colnames(Base_pop_sexo_sp)[1] <- "sexo"
colnames(Base_pop_sexo_sp) <- gsub("^X", "", colnames(Base_pop_sexo_sp))
Base_pop_sexo_sp$Municipio <- "São Paulo"
#
Base_pop_sexo_for <- read_excel(  
  paste0("ibge_pop_sexo_for.xlsx")) %>%  
  data.frame()
colnames(Base_pop_sexo_for)[1] <- "sexo"
colnames(Base_pop_sexo_for) <- gsub("^X", "", colnames(Base_pop_sexo_for))
Base_pop_sexo_for$Municipio <- "Fortaleza"
#
Base_pop_sexo <- rbind(Base_pop_sexo_for, Base_pop_sexo_sp)

### Pop fxetaria
Base_pop_fxetaria_sp <- read_excel(  
  paste0("ibge_pop_fxetaria_sp.xlsx")) %>%  
  data.frame()
colnames(Base_pop_fxetaria_sp)[1] <- "fxetaria"
colnames(Base_pop_fxetaria_sp) <- gsub("^X", "", colnames(Base_pop_fxetaria_sp))
Base_pop_fxetaria_sp$Municipio <- "São Paulo"
#
Base_pop_fxetaria_for <- read_excel(  
  paste0("ibge_pop_fxetaria_for.xlsx")) %>%  
  data.frame()
colnames(Base_pop_fxetaria_for)[1] <- "fxetaria"
colnames(Base_pop_fxetaria_for) <- gsub("^X", "", colnames(Base_pop_fxetaria_for))
Base_pop_fxetaria_for$Municipio <- "Fortaleza"
#
Base_pop_fxetaria <- rbind(Base_pop_fxetaria_for, Base_pop_fxetaria_sp)



###### calculo incidência geral
Base_geral <- melt(Base_geral, id.vars = c("Municipio")) %>%
  rename(Ano = variable, Casos = value)
Base_pop <- melt(Base_pop, id.vars = c("Municipio")) %>%
  rename(Ano = variable, Pop = value)
Base_geral <- Base_geral %>% left_join(Base_pop)
Base_geral <- Base_geral %>% mutate(Inc = (Casos/Pop)*100000)

# grafico incidencia geral
ginc_geral <- ggplot(Base_geral,
                      aes(x = factor(Ano), y = Inc, fill = Municipio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "Ano", y = "Incidência Tuberculose / 100.000 hab", fill = "Municipio") +
  facet_wrap(~ Municipio, ncol = 1) +
  geom_text(aes(label = round(Inc, 1)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.3,
            color = "black") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 8, hjust = 0.4),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 8)
  )
# Salvar o gráfico em SVG
ggsave("Graficos/ginc_geral.svg", plot = ginc_geral, width = 6, height = 6, dpi = 300, device = "svg")


###### calculo incidência por sexo
Base_sexo <- melt(Base_sexo, id.vars = c("Municipio", "sexo")) %>%
  rename(Ano = variable, Casos = value)
Base_pop_sexo <- melt(Base_pop_sexo, id.vars = c("Municipio", "sexo")) %>%
  rename(Ano = variable, Pop = value)
Base_sexo <- Base_sexo %>% filter(!sexo == "Ignorado")
Base_sexo <- Base_sexo %>% left_join(Base_pop_sexo)
Base_sexo <- Base_sexo %>% mutate(Inc = (Casos/Pop)*100000)

# grafico incidencia sexo
ginc_sexo <- ggplot(Base_sexo,
                     aes(x = factor(Ano), y = Inc, fill = sexo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "Ano", y = "Incidência Tuberculose / 100.000 hab", fill = "Municipio") +
  facet_wrap(~ Municipio, ncol = 1) +
  geom_text(aes(label = round(Inc, 1)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.3,
            color = "black") +
  scale_y_continuous(limits = c(0, 130)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 8, hjust = 0.4),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 8)
  )
# Salvar o gráfico em SVG
ggsave("Graficos/ginc_sexo.svg", plot = ginc_sexo, width = 6, height = 6, dpi = 300, device = "svg")



###### calculo incidência por fxetaria
Base_fxetaria <- melt(Base_fxetaria, id.vars = c("Municipio", "fxetaria")) %>%
  rename(Ano = variable, Casos = value)
Base_pop_fxetaria <- melt(Base_pop_fxetaria, id.vars = c("Municipio", "fxetaria")) %>%
  rename(Ano = variable, Pop = value)
Base_fxetaria <- Base_fxetaria %>% left_join(Base_pop_fxetaria)
Base_fxetaria <- Base_fxetaria %>% mutate(Inc = (Casos/Pop)*100000)

# grafico incidencia fxetaria
ginc_fxetaria <- ggplot(Base_fxetaria,
                    aes(x = factor(Ano), y = Inc, fill = fxetaria)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "Ano", y = "Incidência Tuberculose / 100.000 hab", fill = "Municipio") +
  facet_wrap(~ Municipio, ncol = 1) +
  #geom_text(aes(label = round(Inc, 1)),
  #          position = position_dodge(width = 0.9), vjust = -0.5, size = 2,
  #          color = "black") +
  scale_y_continuous(limits = c(0, 140)) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 20, hjust = 0.4),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 12)
  )
# Salvar o gráfico em SVG
ggsave("Graficos/ginc_fxetaria.svg", plot = ginc_fxetaria, width = 12, height = 7, dpi = 300, device = "svg")


###### calculo coinfecção TB/aids
Base_aids_sim <- Base_aids %>% filter(aids == "Sim") %>% select(-aids)
Base_aids_sim <- melt(Base_aids_sim, id.vars = c("Municipio")) %>%
  rename(Ano = variable, aids = value)
Base_geral <- Base_geral %>% left_join(Base_aids_sim)
Base_geral <- Base_geral %>% mutate(Prop_aids = (aids/Casos)*100)

# grafico coinfecção TB/aids
gprop_aids <- ggplot( Base_geral,
                    aes(x = factor(Ano), y = Prop_aids, fill = Municipio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "Ano", y = "Proporção de coinfecção TB/aids", fill = "Municipio") +
  facet_wrap(~ Municipio, ncol = 1) +
  geom_text(aes(label = paste0(round(Prop_aids, 2), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.3,
            color = "black") +
  scale_y_continuous(limits = c(0, 17)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 8, hjust = 0.4),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(size = 8))
# Salvar o gráfico em SVG
ggsave("Graficos/gprop_aids.svg", plot = gprop_aids, width = 6, height = 6, dpi = 300, device = "svg")

###### calculo RP TB/aids
Base_geral <- Base_geral %>% mutate(RP_aids = (aids/(Casos-aids)))
# grafico RP TB/aids
grp_aids <- ggplot( Base_geral,
                      aes(x = factor(Ano), y = RP_aids, fill = Municipio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "Ano", y = "Razão de Prevalência TB/aids", fill = "Municipio") +
  facet_wrap(~ Municipio, ncol = 1) +
  geom_text(aes(label = round(RP_aids, 2)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.3,
            color = "black") +
  scale_y_continuous(limits = c(0, 0.2)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 8, hjust = 0.4),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(size = 8))
# Salvar o gráfico em SVG
ggsave("Graficos/grp_aids.svg", plot = grp_aids, width = 6, height = 6, dpi = 300, device = "svg")


###### calculo proporção diabetes
Base_diabetes_sim <- Base_diabetes %>% filter(diabetes == "Sim") %>% select(-diabetes)
Base_diabetes_sim <- melt(Base_diabetes_sim, id.vars = c("Municipio")) %>%
  rename(Ano = variable, diabetes = value)
Base_geral <- Base_geral %>% left_join(Base_diabetes_sim)
Base_geral <- Base_geral %>% mutate(Prop_diabetes = (diabetes/Casos)*100)

# grafico coinfecção TB/diabetes
gprop_diabetes <- ggplot( Base_geral,
                      aes(x = factor(Ano), y = Prop_diabetes, fill = Municipio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "Ano", y = "Proporção de TB/diabetes", fill = "Municipio") +
  facet_wrap(~ Municipio, ncol = 1) +
  geom_text(aes(label = paste0(round(Prop_diabetes, 2), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.3,
            color = "black") +
  scale_y_continuous(limits = c(0, 13)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 8, hjust = 0.4),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(size = 8))
# Salvar o gráfico em SVG
ggsave("Graficos/gprop_diabetes.svg", plot = gprop_diabetes, width = 6, height = 6, dpi = 300, device = "svg")

###### calculo RP diabetes
Base_geral <- Base_geral %>% mutate(RP_diabetes = (diabetes/(Casos-diabetes)))
# grafico RP TB/diabetes
grp_diabetes <- ggplot( Base_geral,
                    aes(x = factor(Ano), y = RP_diabetes, fill = Municipio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "Ano", y = "Razão de Prevalência TB/diabetes", fill = "Municipio") +
  facet_wrap(~ Municipio, ncol = 1) +
  geom_text(aes(label = round(RP_diabetes, 2)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.3,
            color = "black") +
  scale_y_continuous(limits = c(0, 0.16)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 8, hjust = 0.4),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(size = 8))
# Salvar o gráfico em SVG
ggsave("Graficos/grp_diabetes.svg", plot = grp_diabetes, width = 6, height = 6, dpi = 300, device = "svg")


###### calculo proporção alcool
Base_alcool_sim <- Base_alcool %>% filter(alcool == "Sim") %>% select(-alcool)
Base_alcool_sim <- melt(Base_alcool_sim, id.vars = c("Municipio")) %>%
  rename(Ano = variable, alcool = value)
Base_geral <- Base_geral %>% left_join(Base_alcool_sim)
Base_geral <- Base_geral %>% mutate(Prop_alcool = (alcool/Casos)*100)

# grafico coinfecção TB/alcool
gprop_alcool <- ggplot( Base_geral,
                          aes(x = factor(Ano), y = Prop_alcool, fill = Municipio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "Ano", y = "Proporção de TB/alcool", fill = "Municipio") +
  facet_wrap(~ Municipio, ncol = 1) +
  geom_text(aes(label = paste0(round(Prop_alcool, 2), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.3,
            color = "black") +
  scale_y_continuous(limits = c(0, 27)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 8, hjust = 0.4),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(size = 8))
# Salvar o gráfico em SVG
ggsave("Graficos/gprop_alcool.svg", plot = gprop_alcool, width = 6, height = 6, dpi = 300, device = "svg")

###### calculo RP alcool
Base_geral <- Base_geral %>% mutate(RP_alcool = (alcool/(Casos-alcool)))
# grafico RP TB/alcool
grp_alcool <- ggplot( Base_geral,
                        aes(x = factor(Ano), y = RP_alcool, fill = Municipio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "Ano", y = "Razão de Prevalência TB/alcool", fill = "Municipio") +
  facet_wrap(~ Municipio, ncol = 1) +
  geom_text(aes(label = round(RP_alcool, 2)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.3,
            color = "black") +
  scale_y_continuous(limits = c(0, 0.4)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 8, hjust = 0.4),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(size = 8))
# Salvar o gráfico em SVG
ggsave("Graficos/grp_alcool.svg", plot = grp_alcool, width = 6, height = 6, dpi = 300, device = "svg")


###### calculo proporção drogas
Base_drogas_sim <- Base_drogas %>% filter(drogas == "Sim") %>% select(-drogas)
Base_drogas_sim <- melt(Base_drogas_sim, id.vars = c("Municipio")) %>%
  rename(Ano = variable, drogas = value)
Base_geral <- Base_geral %>% left_join(Base_drogas_sim)
Base_geral <- Base_geral %>% mutate(Prop_drogas = (drogas/Casos)*100)

# grafico coinfecção TB/drogas
gprop_drogas <- ggplot( Base_geral,
                        aes(x = factor(Ano), y = Prop_drogas, fill = Municipio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "Ano", y = "Proporção de TB/drogas", fill = "Municipio") +
  facet_wrap(~ Municipio, ncol = 1) +
  geom_text(aes(label = paste0(round(Prop_drogas, 2), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.3,
            color = "black") +
  scale_y_continuous(limits = c(0, 27)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 8, hjust = 0.4),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(size = 8))
# Salvar o gráfico em SVG
ggsave("Graficos/gprop_drogas.svg", plot = gprop_drogas, width = 6, height = 6, dpi = 300, device = "svg")

###### calculo RP drogas
Base_geral <- Base_geral %>% mutate(RP_drogas = (drogas/(Casos-drogas)))
# grafico RP TB/drogas
grp_drogas <- ggplot( Base_geral,
                      aes(x = factor(Ano), y = RP_drogas, fill = Municipio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "Ano", y = "Razão de Prevalência TB/drogas", fill = "Municipio") +
  facet_wrap(~ Municipio, ncol = 1) +
  geom_text(aes(label = round(RP_drogas, 2)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.3,
            color = "black") +
  scale_y_continuous(limits = c(0, 0.4)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 8, hjust = 0.4),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(size = 8))
# Salvar o gráfico em SVG
ggsave("Graficos/grp_drogas.svg", plot = grp_drogas, width = 8, height = 10, dpi = 300, device = "svg")



###### calculo proporção forma
Base_forma_sim <- Base_forma %>% filter(forma == "PULMONAR") %>% select(-forma)
Base_forma_sim <- melt(Base_forma_sim, id.vars = c("Municipio")) %>%
  rename(Ano = variable, forma = value)
Base_geral <- Base_geral %>% left_join(Base_forma_sim)
Base_geral <- Base_geral %>% mutate(Prop_forma = (forma/Casos)*100)

# grafico coinfecção TB/forma
gprop_forma <- ggplot( Base_geral,
                        aes(x = factor(Ano), y = Prop_forma, fill = Municipio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "Ano", y = "Proporção de TB/forma", fill = "Municipio") +
  facet_wrap(~ Municipio, ncol = 1) +
  geom_text(aes(label = paste0(round(Prop_forma, 2), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2,
            color = "black") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 8, hjust = 0.4),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 8))
# Salvar o gráfico em SVG
ggsave("Graficos/gprop_forma.svg", plot = gprop_forma, width = 6, height = 6, dpi = 300, device = "svg")



###### calculo proporção ppl
Base_ppl_sim <- Base_ppl %>% filter(ppl == "Sim") %>% select(-ppl)
Base_ppl_sim <- melt(Base_ppl_sim, id.vars = c("Municipio")) %>%
  rename(Ano = variable, ppl = value)
Base_geral <- Base_geral %>% left_join(Base_ppl_sim)
Base_geral <- Base_geral %>% mutate(Prop_ppl = (ppl/Casos)*100)

# grafico coinfecção TB/ppl
gprop_ppl <- ggplot( Base_geral,
                        aes(x = factor(Ano), y = Prop_ppl, fill = Municipio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "Ano", y = "Proporção de TB/ppl", fill = "Municipio") +
  facet_wrap(~ Municipio, ncol = 1) +
  geom_text(aes(label = paste0(round(Prop_ppl, 2), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2,
            color = "black") +
  scale_y_continuous(limits = c(0, 8)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 8, hjust = 0.4),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(size = 8))
# Salvar o gráfico em SVG
ggsave("Graficos/gprop_ppl.svg", plot = gprop_ppl, width = 6, height = 6, dpi = 300, device = "svg")

###### calculo RP ppl
Base_geral <- Base_geral %>% mutate(RP_ppl = (ppl/(Casos-ppl)))
# grafico RP TB/ppl
grp_ppl <- ggplot( Base_geral,
                      aes(x = factor(Ano), y = RP_ppl, fill = Municipio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "Ano", y = "Razão de Prevalência TB/ppl", fill = "Municipio") +
  facet_wrap(~ Municipio, ncol = 1) +
  geom_text(aes(label = round(RP_ppl, 2)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.3,
            color = "black") +
  scale_y_continuous(limits = c(0, 0.09)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 8, hjust = 0.4),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 8))
# Salvar o gráfico em SVG
ggsave("Graficos/grp_ppl.svg", plot = grp_ppl, width = 6, height = 6, dpi = 300, device = "svg")



###### calculo proporção beneficio
Base_beneficio_sim <- Base_beneficio %>% filter(beneficio == "Sim") %>% select(-beneficio)
Base_beneficio_sim <- melt(Base_beneficio_sim, id.vars = c("Municipio")) %>%
  rename(Ano = variable, beneficio = value)
Base_geral <- Base_geral %>% left_join(Base_beneficio_sim)
Base_geral <- Base_geral %>% mutate(Prop_beneficio = (beneficio/Casos)*100)

# grafico coinfecção TB/beneficio
gprop_beneficio <- ggplot( Base_geral,
                     aes(x = factor(Ano), y = Prop_beneficio, fill = Municipio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "Ano", y = "Proporção de TB/beneficio", fill = "Municipio") +
  facet_wrap(~ Municipio, ncol = 1) +
  geom_text(aes(label = paste0(round(Prop_beneficio, 2), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2,
            color = "black") +
  scale_y_continuous(limits = c(0, 16)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 8, hjust = 0.4),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 8))
# Salvar o gráfico em SVG
ggsave("Graficos/gprop_beneficio.svg", plot = gprop_beneficio, width = 6, height = 6, dpi = 300, device = "svg")

###### calculo RP beneficio
Base_geral <- Base_geral %>% mutate(RP_beneficio = (beneficio/(Casos-beneficio)))
# grafico RP TB/beneficio
grp_beneficio <- ggplot( Base_geral,
                   aes(x = factor(Ano), y = RP_beneficio, fill = Municipio)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "Ano", y = "Razão de Prevalência TB/beneficio", fill = "Municipio") +
  facet_wrap(~ Municipio, ncol = 1) +
  geom_text(aes(label = round(RP_beneficio, 2)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.3,
            color = "black") +
  scale_y_continuous(limits = c(0, 0.2)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 8, hjust = 0.4),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 8))
# Salvar o gráfico em SVG
ggsave("Graficos/grp_beneficio.svg", plot = grp_beneficio, width = 8, height = 10, dpi = 300, device = "svg")

