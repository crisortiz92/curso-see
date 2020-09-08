#Analisis sobre las noticias vinculadas a Andres Arauz y Rafael Correa
#Trabajo realizado por: Cristhian Ortiz
#Codigo basado en los tutoriales preparados por Hugo Porras https://rpubs.com/hugoporras

#Algunas de las Librerias Necesarias

library(tm)
library(topicmodels)
library(dplyr)
library(tidytext)

#Obtencion de los datos en google news

library(dplyr) # Manipulación de datos tidy
library(rvest) # Web scrapping
library(stringr) # Manipulación de cadenas de texto
library(purrr) # Iteración eficiente 


#Funcion de busqueda de noticias

obtieneNoticiasBusqueda = function(busqueda){
  news_pag = "https://news.google.com/"
  parametro_busqueda = "search?q="
  busqueda_no_espacios = gsub(" ","%20", busqueda)
  parametro_final = "&hl=es-419&gl=US&ceid=US:es-419"
  html_dir = paste0(news_pag,parametro_busqueda,busqueda_no_espacios,parametro_final)
  google_news = read_html(html_dir)
  noticias = google_news %>% 
    html_nodes(css = ".xP6mwf") %>% 
    html_children()
  noticiasDF = map(noticias,obtieneNoticiasData)
  noticiasDF = bind_rows(noticiasDF)
  noticiasDF = noticiasDF[!is.na(noticiasDF$Titular),]
  return(noticiasDF)
}

#Función de caracterización de noticias:

obtieneNoticiasData = function(noticia){
  news_pag = "https://news.google.com/"
  titular = noticia %>% html_node("h3") %>% html_text()
  fecha = noticia %>% html_node("time") %>% html_attr("datetime")
  diario = noticia %>% html_node("a.wEwyrc.AVN2gc.uQIVzc.Sksgp") %>% html_text()
  link_enmascarado = noticia %>% html_node("h3 a") %>% html_attr("href")
  link_enmascarado = paste0(news_pag,substring(link_enmascarado,3))  
  link_leido = read_html(link_enmascarado)
  link = link_leido %>% 
    html_nodes(css='a') %>% 
    tail(1) %>% 
    html_attr("href")
  noticiaDF = data.frame(Titular=titular, Fecha=fecha, Diario=diario, Link=link, stringsAsFactors = F)
  return(noticiaDF)
}

#Función de extracción de noticias:

obtenerNoticiaNacional = function(link_noticia, diario, diccionario_css){
  
  noticia_leida = read_html(link_noticia)
  css = diccionario_css$CSS[diccionario_css$Diario==diario]
  
  text_nodes = noticia_leida %>% 
    html_nodes(css = css) %>% 
    html_nodes("p")
  
  text = text_nodes %>% 
    html_text()
  
  text = paste0(text, collapse = " ")
  
  return(text)
  
}

#Definir los criterios de busqueda

noticiasCorreaDF = obtieneNoticiasBusqueda(busqueda = "Rafael Correa")
noticiasArauzDF = obtieneNoticiasBusqueda("Andres Arauz")
noticiasArauzDF %>% select(Fecha, Diario, Titular, Link) #Rpub
noticiasCorreaDF %>% select(Fecha, Diario, Titular, Link)


# Set nombre de los Diarios Nacionales de Ecuador

Diarios = c("El Comercio (Ecuador)", "El Universo", "Primicias")
Estructura = data.frame(Diario=Diarios)
Estructura$CSS = NA
Estructura$CSS[Estructura$Diario=='El Comercio (Ecuador)'] = '.paragraphs'
Estructura$CSS[Estructura$Diario=='El Universo'] = '.field-name-body'
Estructura$CSS[Estructura$Diario=='Primicias'] = '#entry-content-inarticle'


#Ahora obtenemos las noticias

# Andres Arauz
noticiasArauzDF = noticiasArauzDF %>% filter(Diario %in% Diarios)
news = map2_chr(noticiasArauzDF$Link, noticiasArauzDF$Diario, obtenerNoticiaNacional, diccionario_css=Estructura)
noticiasArauzDF$Noticia = news
print(noticiasArauzDF$Link[1])
print(noticiasArauzDF$Titular[1])
print(news[1])

# Rafael Correa
noticiasCorreaDF = noticiasCorreaDF %>% filter(Diario %in% Diarios)
news = map2_chr(noticiasCorreaDF$Link, noticiasCorreaDF$Diario, obtenerNoticiaNacional, diccionario_css=Estructura)
noticiasCorreaDF$Noticia = news
print(noticiasCorreaDF$Link[1])
print(noticiasCorreaDF$Titular[1])
print(news[1])


#Guardamos los dataframes obtenidos:
saveRDS(noticiasArauzDF, "Caso1_Noticias/noticiasArauzDF.RDS")
saveRDS(noticiasCorreaDF, "Caso1_Noticias/noticiasCorreaDF.RDS")

#####################################################################################

#Inicio parte 2 una vez teniendo ya el data frame listo empiezamos a jugar

library(tidyverse)

noticiasArauzDF = readRDS("Caso1_Noticias/noticiasArauzDF.RDS")
noticiasArauzDF$Noticia = unlist(noticiasArauzDF$Noticia)
noticiasCorreaDF = readRDS("Caso1_Noticias/noticiasCorreaDF.RDS")
noticiasCorreaDF$Noticia = unlist(noticiasCorreaDF$Noticia)
head(noticiasArauzDF %>% select(Fecha, Diario, Titular, Link, Noticia))

# El titular de cada noticia será el identificador de documento 

#Aplicamos flujo de tidytext

# Tokenización: unnest_tokens

library(tidytext)
tidy_Arauz = noticiasArauzDF %>% unnest_tokens(output = Token, input = Noticia)
tidy_Correa = noticiasCorreaDF %>% unnest_tokens(output = Token, input = Noticia)

#vemos si va por buen camino

ejemplo_Arauz = noticiasArauzDF$Titular[1]
ejemplo_Correa = noticiasCorreaDF$Titular[1]
tidy_Arauz %>% 
  filter(Titular==ejemplo_Arauz) %>% 
  select(Diario, Titular, Token) %>% 
  head()

tidy_Correa %>% 
  filter(Titular==ejemplo_Correa) %>%  
  select(Diario, Titular, Token) %>% 
  head()

#Cambiar oraciones a minusculas solo ejemplos
noticiasCorreaDF %>% 
  filter(Titular==ejemplo_Correa) %>% 
  unnest_tokens(output = Token, input = Noticia, token = 'sentences', to_lower = T) %>% 
  select(Token)

#Cambiar a n grmas trabajar con type sentences
noticiasCorreaDF %>% 
  filter(Titular==ejemplo_Correa) %>% 
  unnest_tokens(output = Token, input = Noticia, token = 'ngrams', n=2, to_lower = F) %>% 
  select(Token)

#Ahora usaremos tm para quitar signos de puntuacion, remover numeros, etc

library(tm)

noticiasCorreaDF %>% 
  filter(Titular==ejemplo_Correa) %>% 
  unnest_tokens(output = Token, input = Noticia, token = 'sentences', to_lower = F) %>% 
  mutate(Token = removePunctuation(Token)) %>%
  mutate(Token = removeNumbers(Token)) %>% 
  select(Token)

# Eliminamos espacios extra, pasamos las palabras a minúsculas, se remueve acentos,

library(stringi)
noticiasArauzDF %>% 
  filter(Titular==ejemplo_Arauz) %>% 
  unnest_tokens(output = Token, input = Noticia, token = 'sentences', to_lower = F) %>% 
  mutate(Token = str_squish(string = Token)) %>%
  mutate(Token = str_to_upper(Token, locale = 'es')) %>% 
  mutate(Token = stri_trans_general(Token, id = "Latin-ASCII")) %>% 
  select(Token)


#Parte de Manipulacion

#Eliminar Stopwords
library(readxl)
stopwords_es_1 = read_excel("CustomStopWords.xlsx")
names(stopwords_es_1) = c("Token","Fuente")
stopwords_es_2 = tibble(Token=tm::stopwords(kind = "es"), Fuente="tm")
stopwords_es = rbind(stopwords_es_1, stopwords_es_2)
stopwords_es = stopwords_es[!duplicated(stopwords_es$Token),]
remove(stopwords_es_1, stopwords_es_2)

stopwords_es[sample(nrow(stopwords_es),size = 10, replace = F),]


#Removiendo las palabras vacias

tidy_Arauz = tidy_Arauz %>%
  anti_join(stopwords_es)

tidy_Correa = tidy_Correa %>%
  anti_join(stopwords_es)



#Chequeando todo ok
tidy_Arauz %>% 
  filter(Titular==ejemplo_Arauz) %>% 
  select(Token) %>% 
  head()

tidy_Correa %>% 
  filter(Titular==ejemplo_Correa) %>% 
  select(Token) %>% 
  head()


# Lematización palabra raiz
# Función de lematización
library(rvest)


# Lematización pre-entrenada de la librería udpipe.
library(udpipe)

#udpipe::udpipe_download_model('spanish') # Descomentar al ejecutar por primera vez

model = udpipe_load_model(file = "spanish-gsd-ud-2.4-190531.udpipe")
tidy_Correa_annotated = udpipe_annotate(model, 
                                        x = noticiasCorreaDF$Noticia, 
                                        doc_id = noticiasCorreaDF$Titular)
tidy_Correa_annotated = as_tibble(tidy_Correa_annotated)
names(tidy_Correa_annotated)[6] = "Token" 
tidy_Correa_annotated = tidy_Correa_annotated %>% 
  anti_join(stopwords_es) %>% 
  mutate(Token=removePunctuation(Token)) %>% 
  filter(Token!="")

tidy_Arauz_annotated = udpipe_annotate(model, 
                                       x = noticiasArauzDF$Noticia, 
                                       doc_id = noticiasArauzDF$Titular)
tidy_Arauz_annotated = as_tibble(tidy_Arauz_annotated)
names(tidy_Arauz_annotated)[6] = "Token" 
tidy_Arauz_annotated = tidy_Arauz_annotated %>% 
  anti_join(stopwords_es) %>% 
  mutate(Token=removePunctuation(Token)) %>% 
  filter(Token!="")

#Revisamos resultados

tidy_Correa_annotated %>%
  filter(doc_id==ejemplo_Correa) %>% 
  select(Token,lemma)



#Guardamos los resultados

saveRDS(tidy_Correa, "Caso1_Noticias/tidy_Correa.RDS")
saveRDS(tidy_Arauz, "Caso1_Noticias/tidy_Arauz.RDS")
saveRDS(tidy_Correa_annotated, "Caso1_Noticias/tidy_Correa_annotated.RDS")
saveRDS(tidy_Arauz_annotated, "Caso1_Noticias/tidy_Arauz_annotated.RDS")
###Fin de segunda parte

######################################################################################

###Inicio tercer parte

#Frecuencia de palabras

library(tidyverse)
tidy_Arauz_annotated = readRDS("Caso1_Noticias/tidy_Arauz_annotated.RDS")
tidy_Arauz = tidy_Arauz_annotated %>% 
 filter(!is.na(lemma)) 

tidy_Correa_annotated = readRDS("Caso1_Noticias/tidy_Correa_annotated.RDS")
tidy_Correa = tidy_Correa_annotated %>% 
  filter(!is.na(lemma))

ejemplo_Arauz = noticiasArauzDF$Titular[1]
ejemplo_Correa = noticiasCorreaDF$Titular[1]

#Explorando los datos

token1_Arauz = tidy_Arauz %>% 
  count(doc_id, lemma, sort = TRUE)

token1_total = token1_Arauz %>%
  group_by(doc_id) %>%
  summarize(total = sum(n))



token1_Arauz = token1_Arauz %>% 
  left_join(token1_total)

token1_Arauz %>% 
  filter(doc_id==ejemplo_Arauz) %>% 
  head()


library(tidyverse)
token1_Correa = tidy_Correa %>% 
  count(doc_id, lemma, sort = TRUE)

token1_total = token1_Correa %>%
  group_by(doc_id) %>%
  summarize(total = sum(n))

token1_Correa = token1_Correa %>% 
  left_join(token1_total)

token1_Correa %>% 
  filter(doc_id==ejemplo_Correa) %>% 
  head()

# Graficando


ggplot(token1_Arauz %>% 
         filter(doc_id %in% unique(token1_Arauz$doc_id)[1:4]), aes(n/total, fill = doc_id)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~doc_id, ncol = 2, scales = "free_y") + 
  labs(x="Frecuencia relativa de cada palabra en cada documento", y="Frecuencia",
       title = "Frecuencias relativas: Andres Arauz")


ggplot(token1_Correa %>% 
         filter(doc_id %in% unique(token1_Correa$doc_id)[1:4]), aes(n/total, fill = doc_id)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~doc_id, ncol = 2, scales = "free_y") + 
  labs(x="Frecuencia relativa de cada palabra en cada documento", y="Frecuencia",
       title = "Frecuencias relativas: Rafael Correa")




# Tf-idf
# La medida Tf-idf lo que hace es buscar las palabras importantes

library(tidytext)


token1_Arauz <- token1_Arauz %>%
  bind_tf_idf(lemma, doc_id, n)

token1_Arauz %>%
  select(-total) %>%
  arrange(desc(tf_idf))



token1_Arauz %>%
  filter(doc_id %in% unique(token1_Arauz$doc_id)[1:4]) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(lemma = factor(lemma, levels = rev(unique(lemma)))) %>% 
  group_by(doc_id) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(lemma, tf_idf, fill = doc_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~doc_id, ncol = 2, scales = "free") +
  coord_flip()


#Correa

library(tidytext)


token1_Correa <- token1_Correa %>%
  bind_tf_idf(lemma, doc_id, n)

token1_Correa %>%
  select(-total) %>%
  arrange(desc(tf_idf))



token1_Correa %>%
  filter(doc_id %in% unique(token1_Correa$doc_id)[1:4]) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(lemma = factor(lemma, levels = rev(unique(lemma)))) %>% 
  group_by(doc_id) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(lemma, tf_idf, fill = doc_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~doc_id, ncol = 2, scales = "free") +
  coord_flip()


#Construcción de bigramas
#Si guardar utilizar los datos guardados


noticiasArauzDF = readRDS("Caso1_Noticias/noticiasArauzDF.RDS")
noticiasArauzDF$Noticia = unlist(noticiasArauzDF$Noticia)
noticiasCorreaDF = readRDS("Caso1_Noticias/noticiasCorreaDF.RDS")
noticiasCorreaDF$Noticia = unlist(noticiasCorreaDF$Noticia)

bigramas_Arauz = noticiasArauzDF %>%
  unnest_tokens(bigrama, Noticia, token = "ngrams", n = 2)

bigramas_Arauz %>% 
  filter(Titular==ejemplo_Arauz) %>% 
  select(Titular, bigrama)



bigramas_Correa = noticiasCorreaDF %>%
  unnest_tokens(bigrama, Noticia, token = "ngrams", n = 2)

bigramas_Correa %>% 
  filter(Titular==ejemplo_Correa) %>% 
  select(Titular, bigrama)



#Conteo y tf-idf de bigramas


library(readxl)
stopwords_es_1 = read_excel("CustomStopWords.xlsx")
names(stopwords_es_1) = c("Token","Fuente")
stopwords_es_2 = tibble(Token=tm::stopwords(kind = "es"), Fuente="tm")
stopwords_es = rbind(stopwords_es_1, stopwords_es_2)
stopwords_es = stopwords_es[!duplicated(stopwords_es$Token),]
remove(stopwords_es_1, stopwords_es_2)


bigramas_Arauz = bigramas_Arauz %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>%
  filter(!palabra1 %in% c(stopwords_es$Token)) %>%
  filter(!palabra2 %in% c(stopwords_es$Token))

bigramas_frec_Arauz = bigramas_Arauz %>% 
  count(Titular, palabra1, palabra2, sort = TRUE) %>% 
  unite(bigrama, palabra1, palabra2, sep = " ")

bigramas_frec_Arauz



bigramas_Correa = bigramas_Correa %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>%
  filter(!palabra1 %in% c(stopwords_es$Token)) %>%
  filter(!palabra2 %in% c(stopwords_es$Token))

bigramas_frec_Correa = bigramas_Correa %>% 
  count(Titular, palabra1, palabra2, sort = TRUE) %>% 
  unite(bigrama, palabra1, palabra2, sep = " ")

bigramas_frec_Correa


bigramas_tfidf_Correa = bigramas_frec_Correa %>%
  bind_tf_idf(bigrama, Titular, n)

bigramas_tfidf_Correa




bigramas_tfidf_Arauz = bigramas_frec_Arauz %>%
  bind_tf_idf(bigrama, Titular, n)

bigramas_tfidf_Arauz



# Red de bigramas
library(igraph)

bigrama_grafo_Arauz = bigramas_Arauz %>%
  count(palabra1, palabra2, sort = TRUE) %>% 
  filter(n >= 4) %>%
  graph_from_data_frame()

bigrama_grafo_Arauz



bigrama_grafo_Correa = bigramas_Correa %>%
  count(palabra1, palabra2, sort = TRUE) %>% 
  filter(n >= 4) %>%
  graph_from_data_frame()

bigrama_grafo_Correa


library(ggraph)

set.seed(123)

ggraph(bigrama_grafo_Arauz, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_label(aes(label = name), vjust = 1, hjust = 1)

ggraph(bigrama_grafo_Correa, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_label(aes(label = name), vjust = 1, hjust = 1)



# Nubes de palabras

library(wordcloud2)

library(stringi)
wc_Correa = tidy_Correa_annotated %>% 
  anti_join(stopwords_es, by=c("lemma"="Token")) %>% 
  mutate(lemma = stri_trans_general(lemma, id = "Latin-ASCII")) %>%
  filter(lemma!='"') %>% 
  count(lemma, sort=T) %>% 
  rename("word"=lemma,"freq"=2) %>% 
  wordcloud2(shape="circle", minSize = 10)

wc_Correa



library(wordcloud2)

library(stringi)
wc_Arauz = tidy_Arauz_annotated %>% 
  anti_join(stopwords_es, by=c("lemma"="Token")) %>% 
  mutate(lemma = stri_trans_general(lemma, id = "Latin-ASCII")) %>%
  filter(lemma!='"') %>% 
  count(lemma, sort=T) %>% 
  rename("word"=lemma,"freq"=2) %>% 
  wordcloud2(shape="circle", minSize = 10)

wc_Arauz


# Extracción de palabras clave
library(udpipe)


Arauz_keywords = keywords_rake(x = tidy_Arauz, term = "lemma", group = "doc_id", relevant = tidy_Arauz$upos %in% c("NOUN","PROPN","VERB"), ngram_max = 1)
head(Arauz_keywords)


Correa_keywords = keywords_rake(x = tidy_Correa, term = "lemma", group = "doc_id", relevant = tidy_Correa$upos %in% c("NOUN","PROPN","VERB"), ngram_max = 1)
head(Correa_keywords)
