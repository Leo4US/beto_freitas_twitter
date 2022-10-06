# Último acesso: 03/10/2022
# Grupo de Células 1 > Pacotes > Célula 1.1 - Pacotes Básicos
# Instalando quanteda package e relacionados. Estou me baseando inicialmente no tutorial https://tutorials.quanteda.io/introduction/install/
# BASIC PACKAGES
install.packages("quanteda")
install.packages("quanteda.textmodels")
install.packages("quanteda.textstats")
install.packages("quanteda.textplots")
install.packages("readtext")
#
#----------------
# Célula 1.3 - ADDITIONAL PACKAGES
# The tutorials do not cover syntactical analysis, but you should install spacyr for part-of-speech tagging, entity recognition, and dependency parsing. It provides an interface to the spaCy library and works well with quanteda. To install it we have to have Python installed to use the spacyr package. For more info check: https://github.com/quanteda/spacyr/blob/master/README.md
install.packages("spacyr")
# Use newsmap to classify documents based on "seed words" in dictionaries and the seededlda package to run topic models.
install.packages("newsmap")
install.packages("seededlda")
#
#-------------------------
# Célula 1.4 - Para dados pré-formatados, requerer os pacotes abaixo.
require(quanteda)
require(readtext)
# Setting WD (Working Directory) through code
# Verificar WD atual
getwd()
# Setting properly the WD, if wanted by coding setwd() [replacing \ in a path with /]
# setwd("C:/Users/labo2/OneDrive/Documentos/ROOT_BFreitas")
#-------------------------
# Alternatively, you can use the readtext package to import character (comma- or tab-separated) values. readtext reads files containing text, along with any associated document-level variables.
library(readr)
#
# Célula 2 - Raw Data > Célula 2.1 - Carregando/visualizando Coleta01
coleta01 <- read_csv("coleta01.csv")
View(coleta01)
#
# library(readr)
# Célula 2.2 - Carregando/visualizando Coleta01
# Criar pasta no driver no caminho igual ao abaixo ou alterar o caminho para um caminho específico no drive
coleta02 <- read_csv("coleta02.csv")
View(coleta02)
#
# Célula 2.3 - Visualização/cabeçalho dos datasets
head(coleta01)
head(coleta02)
#
# Célula 2.4 - Combinando as duas coletas em uma só.
coleta0102 <- rbind(coleta01, coleta02)
#
# Grupo de célula 3 - Corpus > Célula 3.1 - Criando matriz a partir do corpus, de onde se observam as métricas [Data Exploring]
corpusBF.dfm<-dfm(corpus_BFreitas, 
                 remove_punct = TRUE,
                 case_insensitive=TRUE,
                 remove = stopwords("portuguese"),
                 verbose = TRUE)
colnames(corpusBF.dfm)
corpusBF.dfm
head(corpusBF.dfm)
#
# Célula 3.2 - Salvando a matriz em uma tabela
library(MASS)
save("corpusBF.dfm", file = "corpusBF.dfm")
#
# Grupo de Células 4 - Perguntas de Pesquisa/Procedural > Célula 4.1 - Quais são as hashtags?
tag.dfm.corpusBF <- dfm_select(corpusBF.dfm, pattern = ("#*"))
#
# Célula 4.2 - Carrega as n hashtags que mais ocorrem no texto
# https://quanteda.io/reference/topfeatures.html
# List the most (or least) frequently occurring features in a dfm, either as a whole or separated by document.
toptag.corpusBF <- names(topfeatures(tag.dfm.corpusBF, 50))
head(toptag.corpusBF, 50)
#
#Célula 3.3 - FCM
tag_fcm.corpusBF <- fcm(tag.dfm.corpusBF)
save("tag_fcm.corpusBF", file = "tag_fcm.corpusBF")
colnames(tag_fcm.corpusBF)
#
# Célula 4.3 - E as mais frequentes? 
topgat_fcm.corpusBF <- fcm_select(tag_fcm.corpusBF, pattern = toptag.corpusBF)
save("topgat_fcm.corpusBF", file = "topgat_fcm.corpusBF")
head(topgat_fcm.corpusBF)
#
# Célula 4.4 - Rede/visualização das hashtags
# Tive problemas com a função textplot_network(). Aparece erro.
# Por isso, abrir https://cran.r-project.org/web/packages/quanteda.textplots/quanteda.textplots.pdf para informações completas sobre a função textplot
library("quanteda.textplots")
# Depois de pesquisar, li que a função textplot_network tinha sido movido para o pacote quanteda.textplots, conforme p. 78, Package 'quanteda' August 9, 2022 <https://cran.r-project.org/web/packages/quanteda/quanteda.pdf>
textplot_network(topgat_fcm.corpusBF, 
                 min_freq = 0.1, 
                 edge_alpha = 0.8, 
                 edge_size = 4)
# Para mais informações sobre network, ver <https://www.kateto.net/wp-content/uploads/2015/06/Polnet%202015%20Network%20Viz%20Tutorial%20-%20Ognyanova.pdf>
#Doc: https://quanteda.io/articles/pkgdown/examples/twitter.html?q=network
#
# Célula 4.5 - Nuvem de hashtags
set.seed(132) #random
textplot_wordcloud(tag.dfm.corpusBF, min_size = 0.5, max_size = 4, min_count = 3, max_words = 150, color = "darkblue", 
  font = NULL, adjust = 0, rotation = 0.1, random_order = FALSE, random_color = FALSE, ordered_color = FALSE, 
  labelcolor = "gray20", labelsize = 1.5, labeloffset = 0, fixed_aspect = TRUE, comparison = FALSE
)
# Apareceram avisos sobre tamanho e outras configurações do resultado gerado. O comando abaixo disponibiliza os 50 primeiros. Depois vejo isso!
warnings()
# Diminuí de 500 para 150 o 'max_words'. Os warnings diminuíram.
#
# Grupo de células 5 - Tweets > Célula 5.1 - Tratamento dos tweets para análise os tweets. Primeiro, devemos transformá-los em texto: 
v_0102 <- coleta0102 #parei aqui, 21/08/2022.
# Neste ponto, busquei as correlações entre as palavras - rede de palavras.
# Encontrei um artigo: <file:///C:/Users/labo2/OneDrive/Documentos/R/1859-Texto%20do%20artigo-3983-2-10-20210608.pdf>.
# Manejando o corpus0102 (as duas coletas de dados ao mesmo tempo)
v_0102.lower <- char_tolower(corpus_BFreitas)
#
# Célula 5.2 - Limpeza dos tweets e tokenização. Encontrei as informações abaixo:
#https://tutorials.quanteda.io/basic-operations/tokens/tokens/
#https://quanteda.io/articles/pkgdown/examples/chinese.html
stop_words <- stopwords("portuguese")
#
v_0102.tokens <- tokens(v_0102.lower, remove_punct = TRUE) %>%
                  tokens_remove(pattern = stop_words)

v_0102.word <-  v_0102.tokens %>% as.character()
#
# Célula 5.3 - Bag of Words para testar este manejo.
bag_of_words <- dfm(v_0102.word, remove_punct = TRUE, remove = stop_words)
#
# Célula 5.4 - Visualização do Bag of Words
View(bag_of_words)
#
# Célula 5.5 - Visualização da tokenização
View(v_0102.tokens)
#
# 1) rede de palavras para quantificar e recortar uma amostra representativa
# texto1 = "bola sapo prato caco"
# texto2 = "sapo caco frita brita"
# texto3 = "cc frita bota embrochavel"
# conjunto = c(texto1, texto2, texto2)
# tokens = tokens(conjunto)
# tokens_ngrams(tokens, n = 3)
#
# Grupo de células 6 - recorte por n-gram > Célula 6.1 - correlação de palavras (n-grams);
#
# https://tutorials.quanteda.io/basic-operations/tokens/tokens_ngrams/
# https://quanteda.io/reference/tokens_ngrams.html
#
# Arguments
# n= integer vector specifying the number of elements to be concatenated in each ngram. 
# Each element of this vector will define a n in the n-gram(s) that are produced
# skip	= integer vector specifying the adjacency skip size for tokens forming the ngrams, 
# default is 0 for only immediately neighbouring words. For skipgrams, skip can be a vector of integers, 
# as the "classic" approach to forming skip-grams is to set skip = k where k is the distance for which k or fewer 
# skips are used to construct the n-gram. Thus a "4-skip-n-gram" defined as skip = 0:4 
# produces results that include 4 skips, 3 skips, 2 skips, 1 skip, and 0 skips 
# (where 0 skips are typical n-grams formed from adjacent words). See Guthrie et al (2006).
#
# concatenator=character for combining words, default is _ (underscore) character
toks_ngram <- tokens_ngrams(v_0102.tokens, n = 2:3)
#
View(toks_ngram[1])
View(toks_ngram[[1]])
View(length(toks_ngram[[1]]))
#
# Célula 6.2 - filtrar correlação de palavras
# Para filtrar, basta utilizar o parâmetro pattern
tokens_compound <- tokens_compound(v_0102.tokens, pattern = phrase("homem *"))
head(tokens_compound, 1)
tokens_compound_select <- tokens_select(tokens_compound, pattern = phrase("homem*"))
head(tokens_compound_select[[1]], 30)
#
# Célula 6.3 - Tratamento e teste por comprimento em comparação
#dfm_select remove features do dfm
#Como já retiramos anteriormente, não seria necessário refazer.
dfmat_inaug_nostop <- dfm_select(bag_of_words, pattern = stop_words, selection = "remove")
print(length(bag_of_words))
print(length(dfmat_inaug_nostop))
#
# Célula 6.3.1 - letramento de código/dados lolo 'dfm_remove' tem o mesmo efeito que dfm_select remove
dfmat_inaug_nostop <- dfm_remove(bag_of_words, pattern = stop_words)
print(length(bag_of_words))
print(length(dfmat_inaug_nostop))
#
# Célula 6.3.2 - seleção por meio do comprimento das características. Ver: 'You can also select features based on the length of features. In the example below, we only keep features consisting of at least five characters.'
dfmat_inaug_long <- dfm_keep(bag_of_words, min_nchar = 5)
print(length(bag_of_words))
print(length(dfmat_inaug_long))
print(dfmat_inaug_long)
#
# Célula 6.4 - visualização do modo bag of words
topfeatures(bag_of_words, 50)
#
# Célula 6.5 - Revisar comando
#considerar esta célula para fazer o recorte da(s) frequência
#https://quanteda.io/reference/dfm_trim.html
#If max_docfreq = 0.1, features that occur in more than 10% of the documents are removed.
dfmat_inaug_docfreq <- dfm_trim(bag_of_words, max_docfreq = 0.5, docfreq_type = "quantile")
#
#print(length(featnames(bag_of_words)))
#print(length(featnames(dfmat_inaug_docfreq)))
#print(bag_of_words)
print(dfmat_inaug_docfreq)
#
# Imagem sobre quantile: https://miro.medium.com/max/546/1*dkDFGXLRP64LtA26IqlsJg.png
# Ngrams Vs Frequência Bag of Words
# carrefour: 78048
#
# Célula 6.6 - composição de tokens/seleção de tokens por n-gram e testes
#tokens_compound () = A tokens object whose documents have been split into chunks of length size (QUANTEDA, 2020, p.116. Disponível em: <https://quanteda.io>. Atualização: 23 de set. de 2020.)
tokens_compound_carrefour <- tokens_compound(v_0102.tokens, pattern = phrase("carrefour *"))
#
#Testar se funciona:
tokens_compound_select_carrefour <- tokens_select(tokens_compound, pattern = phrase("*carrefour*"))
#
tokens_compound_select_carrefour_inicio_da_palavra <- tokens_select(tokens_compound, pattern = phrase("carrefour_*"))
tokens_compound_select_carrefour_fim_da_palavra <- tokens_select(tokens_compound, pattern = phrase("*_carrefour"))
#
#Maior que bigrama(3gramas ou maior)
tokens_compound_select_carrefour_medio_da_palavra <- tokens_select(tokens_compound, pattern = phrase("*_carrefour_*"))
#
# Célula 6.7 - combinando as composições de n-gramas
tokens_compound_list_carrefour = c()
for(i in 1:length(tokens_compound_select_carrefour)){
  tokens_compound_list_carrefour <- c(tokens_compound_list_carrefour, tokens_compound_select_carrefour[[i]])
}
print(length(tokens_compound_list_carrefour))
#
# Célula 6.7 - lista de tokens correlacionados
tokens_compound_list_carrefour = c()
for(i in 1:length(tokens_compound_select_carrefour_inicio_da_palavra)){
  tokens_compound_list_carrefour <- c(tokens_compound_list_carrefour, tokens_compound_select_carrefour_inicio_da_palavra[[i]])
}
#
for(i in 1:length(tokens_compound_select_carrefour_fim_da_palavra)){
  tokens_compound_list_carrefour <- c(tokens_compound_list_carrefour, tokens_compound_select_carrefour_fim_da_palavra[[i]])
}
#
# Célula 6.8 - vamos nomear ainda 
tokens_compound_list.tokens <- tokens(tokens_compound_list_carrefour, remove_punct = TRUE) %>%
                  tokens_remove(pattern = stop_words)
tokens_compound_list.word <-  tokens_compound_list.tokens %>% as.character()
#
# Célula 6.9 - verificação do tipo da variável
typeof(tokens_compound_list.word)
#
# Célula 6.10 - DFM do recorte
tokens_compound_dfm = dfm(tokens_compound_list.word, remove_punct = TRUE, remove = stop_words)
#
# Célula 6.11 - visualização
View(topfeatures(tokens_compound_dfm, 50))
