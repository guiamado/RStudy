#Instalando Pacotes
#install.packages("jsonlite")
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("tidyverse")

# Utilizando pacotes instalados
library(jsonlite)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rmarkdown)

unb_DGP <- read_excel("Your Path to the files", sheet = 5)
View(unb_DGP)

arquivo_researchers_aplicada <- "Your Path to the files"
arquivo_perfis_aplicada <- "Your Path to the files"
arquivo_publicacoes_aplicada <- "Your Path to the files"
arquivo_advise_aplicada <- "Your Path to the files"
arquivo_graph_aplicada <- "Your Path to the files"

grande_area_researchers <- "Your Path to the files"
dados_research_grande_area <- fromJSON(grande_area_researchers)

linguistica_researchers <- "Your Path to the files"
dados_linguistica_researchers <- fromJSON(linguistica_researchers)

literatura_researchers <- "Your Path to the files"
dados_literatura_researchers <- fromJSON(literatura_researchers)

dados_research_aplicada <- fromJSON(arquivo_researchers_aplicada)
dados_perfis_aplicada <- fromJSON(arquivo_perfis_aplicada)
dados_ppublicacoes_aplicada <- fromJSON(arquivo_publicacoes_aplicada)
dados_advise_aplicada <- fromJSON(arquivo_advise_aplicada)
dados_graph_aplicada <- fromJSON(arquivo_graph_aplicada)

#Transformando em dataframe
df_outras_ori_concluidas <- data.frame()

for (i in 1:length(dados_advise_aplicada$OUTRAS_ORIENTACOES_CONCLUIDAS)) {
  df_outras_ori_concluidas <- rbind(df_outras_ori_concluidas, dados_advise_aplicada$OUTRAS_ORIENTACOES_CONCLUIDAS[[i]])
}
or2010 <- nrow(filter(df_outras_ori_concluidas, ano == 2010))
or2011 <- nrow(filter(df_outras_ori_concluidas, ano == 2011))
or2012 <- nrow(filter(df_outras_ori_concluidas, ano == 2012))
or2013 <- nrow(filter(df_outras_ori_concluidas, ano == 2013))
or2014 <- nrow(filter(df_outras_ori_concluidas, ano == 2014))
or2015 <- nrow(filter(df_outras_ori_concluidas, ano == 2015))
or2016 <- nrow(filter(df_outras_ori_concluidas, ano == 2016))
or2017 <- nrow(filter(df_outras_ori_concluidas, ano == 2017))

anos <- c('2010','2011','2012','2013','2014','2015','2016','2017')
valores <- c(or2010, or2011, or2012, or2013, or2014, or2015, or2016, or2017)
df_otras_orientacoes <- data_frame(anos, valores)

ggplot(df_otras_orientacoes, aes(x = anos, y = valores, group = 1)) + geom_point(color = "red") + geom_line() +
  labs(title = "Quantidade de Outras Orientações Concluídas na Área de Linguistica Aplicada", x = "Anos", y = "Quantidade de Outras OrientaÃ§Ãµes")

#Plotando Pos Doutorados concluidos
df_posdoutorado_ori_concluidas <- data.frame()

for (i in 1:length(dados_advise_aplicada$ORIENTACAO_CONCLUIDA_POS_DOUTORADO)) {
  df_posdoutorado_ori_concluidas <- rbind(df_posdoutorado_ori_concluidas, dados_advise_aplicada$ORIENTACAO_CONCLUIDA_POS_DOUTORADO[[i]])
}
df_doutorado_ori_concluidas <- data.frame()

for (i in 1:length(dados_advise_aplicada$ORIENTACAO_CONCLUIDA_DOUTORADO)) {
  df_doutorado_ori_concluidas <- rbind(df_doutorado_ori_concluidas, dados_advise_aplicada$ORIENTACAO_CONCLUIDA_DOUTORADO[[i]])
}
df_mestrado_ori_concluidas <- data.frame()

for (i in 1:length(dados_advise_aplicada$ORIENTACAO_CONCLUIDA_MESTRADO)) {
  df_mestrado_ori_concluidas <- rbind(df_mestrado_ori_concluidas, dados_advise_aplicada$ORIENTACAO_CONCLUIDA_MESTRADO[[i]])
}

or2010_pos <- nrow(filter(df_posdoutorado_ori_concluidas, ano == 2010))
or2011_pos <- nrow(filter(df_posdoutorado_ori_concluidas, ano == 2011))
or2012_pos <- nrow(filter(df_posdoutorado_ori_concluidas, ano == 2012))
or2013_pos <- nrow(filter(df_posdoutorado_ori_concluidas, ano == 2013))
or2014_pos <- nrow(filter(df_posdoutorado_ori_concluidas, ano == 2014))
or2015_pos <- nrow(filter(df_posdoutorado_ori_concluidas, ano == 2015))
or2016_pos <- nrow(filter(df_posdoutorado_ori_concluidas, ano == 2016))
or2017_pos <- nrow(filter(df_posdoutorado_ori_concluidas, ano == 2017))

or2010_dout <- nrow(filter(df_doutorado_ori_concluidas, ano == 2010))
or2011_dout <- nrow(filter(df_doutorado_ori_concluidas, ano == 2011))
or2012_dout <- nrow(filter(df_doutorado_ori_concluidas, ano == 2012))
or2013_dout <- nrow(filter(df_doutorado_ori_concluidas, ano == 2013))
or2014_dout <- nrow(filter(df_doutorado_ori_concluidas, ano == 2014))
or2015_dout <- nrow(filter(df_doutorado_ori_concluidas, ano == 2015))
or2016_dout <- nrow(filter(df_doutorado_ori_concluidas, ano == 2016))
or2017_dout <- nrow(filter(df_doutorado_ori_concluidas, ano == 2017))

or2010_mes <- nrow(filter(df_mestrado_ori_concluidas, ano == 2010))
or2011_mes <- nrow(filter(df_mestrado_ori_concluidas, ano == 2011))
or2012_mes <- nrow(filter(df_mestrado_ori_concluidas, ano == 2012))
or2013_mes <- nrow(filter(df_mestrado_ori_concluidas, ano == 2013))
or2014_mes <- nrow(filter(df_mestrado_ori_concluidas, ano == 2014))
or2015_mes <- nrow(filter(df_mestrado_ori_concluidas, ano == 2015))
or2016_mes <- nrow(filter(df_mestrado_ori_concluidas, ano == 2016))
or2017_mes <- nrow(filter(df_mestrado_ori_concluidas, ano == 2017))

valores_totais <- c((or2010 + or2010_mes + or2010_dout + or2010_pos), 
             (or2011 + or2011_mes + or2011_dout + or2011_pos),
             (or2012 + or2012_mes + or2012_dout + or2012_pos),
             (or2013 + or2013_mes + or2013_dout + or2013_pos),
             (or2014 + or2014_mes + or2014_dout + or2014_pos), 
             (or2015 + or2015_mes + or2015_dout + or2015_pos), 
             (or2016 + or2016_mes + or2016_dout + or2016_pos),
             (or2017 + or2017_mes + or2017_dout + or2017_pos))
#PLOTANDO TODAS ORIENTACOES CONCLUIDAS
df_todas_orientacoes <- data_frame(anos, valores_totais)
ggplot(df_todas_orientacoes, aes(x = anos, y = valores_totais, group = 1)) + geom_point(color = "blue") + geom_line() +
  labs(title = "Quantidade Total de Orientações Concluídas na Área de Linguistica Aplicada", x = "Anos", y = "Quantidade de Orientações")

# Caminho das bases de GRUPOS utilizadas
arquivo_DGP <- "ED_arquivo/unb.DGP.xlsx"
dados_pesquisa_grupos <- read_excel(arquivo_DGP, sheet = 1)
dados_pesquisa_linhas <- read_excel(arquivo_DGP, sheet = 2)
dados_pesquisa_parcerias <- read_excel(arquivo_DGP, sheet = 3)
dados_pesquisa_participantes <- read_excel(arquivo_DGP, sheet = 4)
dados_pesquisa_producao <- read_excel(arquivo_DGP, sheet = 5)

# Grupos
df_grupos_pesquisa_linguistica <- filter(dados_pesquisa_grupos, X__4 == "Lingüística")

n_linguistica <- nrow(df_grupos_pesquisa_linguistica)
n_total <- nrow(dados_pesquisa_grupos) - n_linguistica
qnt_grupos <- c(n_linguistica, n_total)
grupos2 <- c("Grupos de Linguistica","Outros Grupos")
grupos_dt2 <- data_frame(qnt_grupos, grupos2)

ggplot(grupos_dt2, aes(x = "", y = qnt_grupos, fill = grupos2)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  labs(title = "Relação dos Grupos de Pesquisa da UnB", x = "" ,y = "Quantidade de Grupos")

# Grupos2
df_grupos_pesquisa_linguistica <- filter(dados_pesquisa_grupos, X__4 == "Lingüística")
df_grupos_pesquisa_linguistica2 <- filter(dados_pesquisa_grupos, X__1 == "A fraseologia e sua equação nas sub-áreas da Lingüística Aplicada")

n_linguistica <- nrow(df_grupos_pesquisa_linguistica)
n_linguistica2 <- nrow(df_grupos_pesquisa_linguistica2)

n_total <- nrow(n_linguistica) - n_linguistica2
qnt_grupos <- c(n_linguistica, n_total)
grupos <- c("Grupos de Linguistica","Grupos de Liguistica Aplicada")
grupos_dt <- data_frame(qnt_grupos, grupos)

ggplot(grupos_dt, aes(x = "", y = qnt_grupos, fill = grupos)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  labs(title = "Relação da Area de Linguistica de Pesquisa da UnB", x = "" ,y = "Quantidade de Grupos")

#UTILIZANDO ORIENTACAO EM ANDAMENTO
df_mestra_ori_andamento <- data.frame()

for (i in 1:length(dados_advise_aplicada$ORIENTACAO_EM_ANDAMENTO_MESTRADO)) {
  df_mestra_ori_andamento <- rbind(df_mestra_ori_andamento, dados_advise_aplicada$ORIENTACAO_EM_ANDAMENTO_MESTRADO[[i]])
}
df_iniciacao_ori_andamento <- data.frame()

for (i in 1:length(dados_advise_aplicada$ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA)) {
  df_iniciacao_ori_andamento <- rbind(df_iniciacao_ori_andamento, dados_advise_aplicada$ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA[[i]])
}

df_posdoutorado_ori_andamento <- data.frame()

for (i in 1:length(dados_advise_aplicada$ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO)) {
  df_posdoutorado_ori_andamento <- rbind(df_posdoutorado_ori_andamento, dados_advise_aplicada$ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO[[i]])
}
df_doutorado_ori_andamento <- data.frame()

for (i in 1:length(dados_advise_aplicada$ORIENTACAO_EM_ANDAMENTO_DOUTORADO)) {
  df_doutorado_ori_andamento <- rbind(df_doutorado_ori_andamento, dados_advise_aplicada$ORIENTACAO_EM_ANDAMENTO_DOUTORADO[[i]])
}
df_graduacao_ori_andamento <- data.frame()

for (i in 1:length(dados_advise_aplicada$ORIENTACAO_EM_ANDAMENTO_GRADUACAO)) {
  df_graduacao_ori_andamento <- rbind(df_graduacao_ori_andamento, dados_advise_aplicada$ORIENTACAO_EM_ANDAMENTO_GRADUACAO[[i]])
}

todas_orientacoes_andamento <- data.frame()
todas_orientacoes_andamento <- rbind(todas_orientacoes_andamento,df_mestra_ori_andamento)
todas_orientacoes_andamento <- rbind(todas_orientacoes_andamento,df_iniciacao_ori_andamento)
todas_orientacoes_andamento <- rbind(todas_orientacoes_andamento,df_posdoutorado_ori_andamento)
todas_orientacoes_andamento <- rbind(todas_orientacoes_andamento,df_doutorado_ori_andamento)
todas_orientacoes_andamento <- rbind(todas_orientacoes_andamento,df_graduacao_ori_andamento)
qplot(as.numeric(df_mestra_ori_andamento$ano),
      geom = "histogram",
      binwidth=1,
      main = "Histograma de Orientação Mestrado em Andamento",
      xlab = "Orientações em Andamento",
      ylab = "",
      fill=I("red"), 
      col=I("blue"))
qplot(as.numeric(df_iniciacao_ori_andamento$ano),
      geom = "histogram",
      binwidth=1,
      main = "Histograma de Orientação Iniciação Cientifica em Andamento",
      xlab = "Orientações em Andamento",
      ylab = "",
      fill=I("red"), 
      col=I("blue"))
qplot(as.numeric(todas_orientacoes_andamento$ano),
      geom = "histogram",
      binwidth=1,
      main = "Histograma de Todas Orientações em Andamento",
      xlab = "Orientações em Andamento",
      ylab = "",
      fill=I("red"), 
      col=I("blue"))

#pizza de todas as orientações
valores_totais_concluida <- ((or2010 + or2010_mes + or2010_dout + or2010_pos)+ 
                    (or2011 + or2011_mes + or2011_dout + or2011_pos)+
                    (or2012 + or2012_mes + or2012_dout + or2012_pos)+
                    (or2013 + or2013_mes + or2013_dout + or2013_pos)+
                    (or2014 + or2014_mes + or2014_dout + or2014_pos)+ 
                    (or2015 + or2015_mes + or2015_dout + or2015_pos)+ 
                    (or2016 + or2016_mes + or2016_dout + or2016_pos)+
                    (or2017 + or2017_mes + or2017_dout + or2017_pos))
valores_totais_andamento <- length(todas_orientacoes_andamento$ano) 
area_nomes <- c("Em andamento", "Concluida")
area_qntd <- c(valores_totais_concluida,valores_totais_andamento)

area_df <- data.frame(area_nomes, area_qntd)

ggplot(area_df, aes(x = "", y = area_qntd, fill = area_nomes)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  labs(title = "Relação de Orientações Concluídas X Em Andamento", 
       x = "" ,y = "Quantidade de Orientações")

#todas as areas separadas
l1 <- length(df_outras_ori_concluidas$ano)
l2 <- length(df_posdoutorado_ori_concluidas)
l3 <- length(df_mestrado_ori_concluidas)
l4 <- length(df_doutorado_ori_concluidas)
l5 <- length(df_mestra_ori_andamento)
l6 <- length(df_iniciacao_ori_andamento)
l7 <- length(df_posdoutorado_ori_andamento)
l8 <- length(df_doutorado_ori_andamento)
l9 <- length(df_graduacao_ori_andamento)
qntd_orientacoes <- c(l1,l2,l3,l4,l5,l6,l7,l8,l9)
nomes_orientacoes <- c("Outras Orientações Concluídas", "Pós Doutorados Concluído",
                       "Mestrados Concluídos","Doutorados Concluídos",
                       "Mestrados Andamento","Iniciação Cientifica Andamento",
                       "Pós Doutorado Andamento","Doutorado Andamento",
                       "Graduação Andamento")
orientacoes_df <- data.frame(nomes_orientacoes, qntd_orientacoes)

ggplot(orientacoes_df, aes(x = "", y = qntd_orientacoes, fill = nomes_orientacoes)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  labs(title = "Relação de Orientações Linguistica Aplicada", 
       x = "" ,y = "Quantidade de Orientações")

#FAZENDO DATAFRAME GRANDE AREA
df_grande_area_mestra_concluido <- data.frame()

for (i in 1:length(dados_research_grande_area$ORIENTACAO_CONCLUIDA_MESTRADO)) {
  df_grande_area_mestra_concluido <- rbind(df_grande_area_mestra_concluido, dados_research_grande_area$ORIENTACAO_CONCLUIDA_MESTRADO[[i]])
}
tamanho <- length(df_grande_area_mestra_concluido)
tamanho2 <- length(df_mestrado_ori_concluidas)

qtd_mestrados_concluidos <- c(tamanho, tamanho2)
nomes <- c("Grande Area Linguistica", "Linguistica Aplicada")

mestrados_df <- data.frame(nomes, qtd_mestrados_concluidos)

ggplot(mestrados_df, aes(x = "", y = qtd_mestrados_concluidos, fill = nomes)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  labs(title = "Grande Area X Linguistica Aplicada", 
       x = "" ,y = "Quantidade de Mestrados Concluídos")

#FAZENDO GRAFICO DE OUTRAS ORIENTAÇÕES EM ANDAMENTO DE LINGUISTICA, LINGUISTICA APLICADA E LITERATURA
df_linguistica_outra_andamento <- data.frame()

for (i in 1:length(dados_linguistica_researchers$OUTRAS_ORIENTACOES_CONCLUIDAS)) {
  df_linguistica_outra_andamento <- rbind(df_linguistica_outra_andamento, dados_linguistica_researchers$OUTRAS_ORIENTACOES_CONCLUIDAS[[i]])
}
df_grande_area_outra_andamento <- data.frame()

for (i in 1:length(dados_research_grande_area$OUTRAS_ORIENTACOES_CONCLUIDAS)) {
  df_grande_area_outra_andamento <- rbind(df_grande_area_outra_andamento, dados_research_grande_area$OUTRAS_ORIENTACOES_CONCLUIDAS[[i]])
}

df_literatura_outra_andamento <- data.frame()

for (i in 1:length(dados_literatura_researchers$OUTRAS_ORIENTACOES_CONCLUIDAS)) {
  df_literatura_outra_andamento <- rbind(df_literatura_outra_andamento, dados_literatura_researchers$OUTRAS_ORIENTACOES_CONCLUIDAS[[i]])
}

todas_orientacoes_outras_andamento <- data.frame()
todas_orientacoes_outras_andamento <- rbind(todas_orientacoes_outras_andamento,df_linguistica_outra_andamento)
todas_orientacoes_outras_andamento <- rbind(todas_orientacoes_outras_andamento,df_grande_area_outra_andamento)
todas_orientacoes_outras_andamento <- rbind(todas_orientacoes_outras_andamento,df_literatura_outra_andamento)
todas_orientacoes_outras_andamento <- rbind(todas_orientacoes_outras_andamento,df_otras_orientacoes)

qplot(as.numeric(todas_orientacoes_outras_andamento$ano),
      geom = "histogram",
      binwidth=1,
      main = "Histograma de Outras Orientações em Andamento",
      xlab = "Orientações em Andamento",
      ylab = "",
      fill=I("black"), 
      col=I("blue"))
#ver qual area tem mais orientações em andamento
todas <- length(df_linguistica_outra_andamento$ano)
todas2 <- length(df_grande_area_outra_andamento$ano)
todas3 <- length(df_literatura_outra_andamento$ano)
todas4 <- length(df_otras_orientacoes$ano)

qtd_ori_andamento <- c(todas, todas2, todas3, todas4)
nomes_andamento <- c("Linguistica","Grande Area Linguistica", "Literatura","Linguistica Aplicada")

andamento_df <- data.frame(nomes_andamento, qtd_ori_andamento)

ggplot(andamento_df, aes(x = "", y = qtd_ori_andamento, fill = nomes_andamento)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  labs(title = "Orientações em andamento", 
       x = "" ,y = "Quantidade de Orientações em Andamento")
