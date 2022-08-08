library(readr)
library(tidyverse)
library(readr)
library(quanteda)
library(wordcloud)
library(ggplot2)
library(DT)

#carregar todos os arquivos dos tweets coletados:
coleta01 <- read_csv("Área de Trabalho/Tweets_Leandro/coleta01.csv")
coleta02 <- read_csv("Área de Trabalho/Tweets_Leandro/coleta02.csv")

# criar o corpora de cada arquivo e juntá-los no mesmo (criamos de cada um caso haja a necessidade de alguma análise temporal)
corpora01 <- corpus(coleta01)
corpora01
corpora02 <- corpus(coleta02)

corpora_beto <- rbind(corpora01, corpora02)

#vamos criar uma matriz a partir do corpora,  a partir do qual vamos conseguir fazer todas as análises
corpora.dfm<-dfm(corpora_beto, 
                 remove_punct = TRUE,
                 case_insensitive=TRUE,
                 remove = stopwords("portuguese"),
                 verbose = TRUE)
colnames(corpora.dfm)

#se você quiser salvar o resultado em uma tabela(acho bem interessante)

library(MASS)
save(corpora.dfm, file = "corpora.dfm.RData")

#Quais são as hashtags? 
tag.dfm.corpora <- dfm_select(corpora.dfm, pattern = ("#*"))
toptag.corpora <- names(topfeatures(tag.dfm.corpora, 50))

head(toptag.corpora, 50)

tag_fcm.corpora <- fcm(tag.dfm.corpora)

colnames(tag_fcm.corpora)

# E as mais frequentes? 
topgat_fcm.corpora <- fcm_select(tag_fcm.corpora, pattern = toptag.corpora )

head(topgat_fcm.corpora)

textplot_network(topgat_fcm.corpora, 
                 min_freq = 0.1, 
                 edge_alpha = 0.8, 
                 edge_size = 4)

#Nuvem de hashtags:

set.seed(132)
textplot_wordcloud(tag.dfm.corpora, max_words = 100)

#Vamos analisar os tweets. Primeiro, devemos transformá-los em texto: 

corpora01.v <- corpora01
corpora02.v <- corpora02

#mexendo no corpus 01
corpora01.lower.v <- char_tolower(corpora01.v)

corpora1.word.v <- tokens(corpora01.lower.v,
                          remove_punct = TRUE) %>% as.character()

corpora1.dfm.2 <- dfm(corpora01.lower.v,
                      remove_punct = TRUE,
                      remove = stopwords("portuguese"))

corpora01.wl<-textstat_frequency(corpora1.dfm.2)
View(corpora01.wl)

#frequência apresentada de forma quantitativa
theme_set(theme_minimal())
textstat_frequency(corpora1.dfm.2, n = 50) %>%
  ggplot(aes(x = rank, y = frequency)) +
  geom_point() +
  labs(x = "Frequency rank", y = "Term frequency")


tri.grams<-textstat_collocations(
  corpora01,
  method = "lambda",
  size = 3,
  min_count = 5,
  smoothing = 0.5,
  tolower = TRUE)
head(tri.grams)

#mexendo no corpus 02: 
corpora02.lower.v <- char_tolower(corpora02.v)

corpora2.word.v <- tokens(corpora02.lower.v,
                          remove_punct = TRUE) %>% as.character()

corpora2.dfm.2 <- dfm(corpora02.lower.v,
                      remove_punct = TRUE,
                      remove = stopwords("portuguese"))

corpora02.wl<-textstat_frequency(corpora2.dfm.2)
View(corpora02.wl)

#frequência apresentada de forma quantitativa
theme_set(theme_minimal())
textstat_frequency(corpora2.dfm.2, n = 50) %>%
  ggplot(aes(x = rank, y = frequency)) +
  geom_point() +
  labs(x = "Frequency rank", y = "Term frequency")


tri.grams<-textstat_collocations(
  corpora02,
  method = "lambda",
  size = 3,
  min_count = 5,
  smoothing = 0.5,
  tolower = TRUE)
head(tri.grams)

