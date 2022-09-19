# Mapas por macroregiões brasileira

setwd("~/Dropbox/24 - GitHub/Mapa por macro regiões brasileiras")

if(!require("readr")) install.packages("readr", dep=T); library(readr)
if(!require("geobr")) install.packages("geobr", dep=T); library(geobr)
if(!require("ggplot2")) install.packages("ggplot2", dep=T); library(ggplot2)
if(!require("ggspatial")) install.packages("ggspatial", dep=T); library(ggspatial)




pop_municipio <- read_csv("pop_municipio_corrigida.csv")
View(pop_municipio)

# Criando as Macro-regiões
pop_municipio$MACRO <- 1 # inserindo uma nova coluna denominada MACRO e que receberá as UF

pop_municipio$MACRO[pop_municipio$UF == "RO" | 
                        pop_municipio$UF == "AC" | 
                        pop_municipio$UF == "AM" | 
                        pop_municipio$UF == "RR" |
                        pop_municipio$UF == "PA" | 
                        pop_municipio$UF == "AP" |  
                        pop_municipio$UF == "TO"]  <- "NORTE"

pop_municipio$MACRO[pop_municipio$UF == "MA" |
                        pop_municipio$UF == "PI" |
                        pop_municipio$UF == "CE" |
                        pop_municipio$UF == "RN" |
                        pop_municipio$UF == "PB" |
                        pop_municipio$UF == "PE" |
                        pop_municipio$UF == "AL" |
                        pop_municipio$UF == "SE" |
                        pop_municipio$UF == "BA"]<- "NORDESTE"

pop_municipio$MACRO[pop_municipio$UF == "MG" |
                        pop_municipio$UF == "ES" |
                        pop_municipio$UF == "RJ" |
                        pop_municipio$UF == "SP" ]<- "SUDESTE"

pop_municipio$MACRO[pop_municipio$UF == "PR"|
                        pop_municipio$UF == "SC"|
                        pop_municipio$UF == "RS"] <- "SUL"

pop_municipio$MACRO[pop_municipio$UF == "MS"|
                        pop_municipio$UF == "MT"|
                        pop_municipio$UF == "GO"|
                        pop_municipio$UF == "DF"]<- "CENTRO-OESTE"

head(pop_municipio, 10)

# Comandos apenas para confirmar se está correta a tabela (todos valores = 5570)
length(pop_municipio$MUNICIPIO) # só para confirmar se continua o número correto

# Alguns mapa utilizam como código identificador (UF+COD.MUNIC),
# Então uma nova variável denominada Key será criada uninod os textos destas duas colunas
pop_municipio$key<- as.integer(paste0(pop_municipio$COD.UF, pop_municipio$COD.MUNIC))

# Verificando a nova tabela
tibble(pop_municipio)

# Confirmando se constam os 5570 municipios brasileiros
length(unique(pop_municipio$key)) # só para confirmar se continua o número correto

## Agrupando as informações por UF e Macroregião com frequencia relativa
Percentual_Estadual<-pop_municipio %>% 
    select(key, UF, MUNICIPIO, ESTIMATIVA_2021) %>% 
    group_by(UF) %>% 
    summarise (cidades = length(key),
               est_2021 = sum (ESTIMATIVA_2021)) %>% 
    mutate(rel.freq = (100 * cidades/sum(cidades))) %>% 
    arrange(desc(rel.freq))  

head(Percentual_Estadual)


Percentual_Macro <- pop_municipio %>% 
    select(key, MACRO, ESTIMATIVA_2021) %>% 
    group_by(MACRO) %>% 
    summarise (cidades = length(key),
               est_2021 = sum (ESTIMATIVA_2021)) %>% 
    mutate(rel.freq = (100 * cidades/sum(cidades))) %>% 
    arrange(desc(rel.freq))  

head(Percentual_Macro)


# Visualisando no mapa
estados<- read_state(code_state = "all")
head(estados, 3)
class(estados) 

# Unindo a geometria + base de dados
names(estados)
head(estados,3)
str(estados)

names(Percentual_Estadual)
colnames(Percentual_Estadual)[1]<- "abbrev_state"
str(Percentual_Estadual)

juntos<- full_join(estados, Percentual_Estadual, by="abbrev_state") 

# Criando as categorias
names(juntos)
fivenum(juntos$rel.freq) #  0.01795332  1.37342908  2.58527828  4.21903052 15.31418312

# Cria a coluna Porcentagem na base de dados
juntos$Porcentagem<-cut(juntos$rel.freq, breaks = c(0, 2, 4, 8, Inf),
                        labels = c ("2%", "4%", "8%", "mais de 10%")) 

# plota no mapa os percentuais
ggplot(juntos) +
    geom_sf(aes(fill = Porcentagem))

# Definindo uma nova escala de cor
ggplot(juntos) +
    geom_sf(aes(fill = Porcentagem))+
    scale_fill_manual(values = c("#F3D4D2", "#E9A8A2", "#E9635A", "#C41617", "#6A0002"))


install.packages("ggimage")
library(ggimage)


# colocando escala e rosa dos ventos
ggplot(juntos) +
    geom_sf(aes(fill = Porcentagem))+
    scale_fill_manual(values = c("#F3D4D2", "#E9A8A2", "#E9635A", "#C41617", "#6A0002"))+
    annotation_scale(location= "br", height = unit(0.2, "cm"))+
    annotation_north_arrow(location="tr", 
                           style = north_arrow_nautical,
                           height = unit(1.5, "cm"),
                           width = unit(1.5, "cm"))

# para adicionar uma imagem (icon)
ggplot(juntos) +
    geom_sf(aes(fill = Porcentagem))+
    scale_fill_manual(values = c("#F3D4D2", "#E9A8A2", "#E9635A", "#C41617", "#6A0002"))+
    annotation_scale(location= "br", height = unit(0.2, "cm"))+
    annotation_north_arrow(location="tr", 
                           style = north_arrow_nautical,
                           height = unit(1.5, "cm"),
                           width = unit(1.5, "cm"))+
    geom_image(aes(x = -35, y = -27), 
               image = "images.jpeg",
               size = 0.22)
