
rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

################################################################################

library(pdftools)

text <- pdf_text("https://apisije-e.jne.gob.pe/TRAMITE/ESCRITO/1531/ARCHIVO/FIRMADO/4984.PDF")
#text <- pdf_text("5314.PDF")
#text <- pdf_text("5262.PDF")
#text <- pdf_text("7346.PDF")

#text <- iconv(text, to="ASCII//TRANSLIT")
#text <- iconv(text, to="UTF-8")
text <- iconv(text, to="latin1")

#cat(text)

################################################################################

text <- gsub("público", "pública", text)
text <- gsub("públicas", "pública", text)
text <- gsub("políticas", "política", text)
text <- gsub("sistemas", "sistema", text)
text <- gsub("ciudadana", "ciudadano", text)
text <- gsub("ciudadanos", "ciudadano", text)
text <- gsub("gobiernos", "gobierno", text)
text <- gsub("ambientales", "ambiental", text)
text <- gsub("ambiente", "ambiental", text)
text <- gsub("cultural", "cultura", text)
text <- gsub("servicios", "servicio", text)
text <- gsub("sociales", "social", text)
text <- gsub("mejora", "mejor", text)
text <- gsub("ciudades", "ciudad", text)
text <- gsub("peruanas", "peruano", text)
text <- gsub("peruana", "peruano", text)
text <- gsub("peruanos", "peruano", text)
text <- gsub("todas", "todos", text)

################################################################################

library(tm)

corpus <- Corpus(VectorSource(text))

d <- corpus

d <- tm_map(d, tolower)
d <- tm_map(d, stripWhitespace)
d <- tm_map(d, removePunctuation)
d <- tm_map(d, removeNumbers)

#stopwords("spanish")
lista_remover <- c('maría','ello','así','cada','hace','hacia','vez','través',
                   'sino','dentro','asimismo','mediante','además','¡')
d <- tm_map(d, removeWords, c(stopwords("spanish"), lista_remover))

tdm <- TermDocumentMatrix(d)

#findFreqTerms(tdm, lowfreq=20)
#findAssocs(tdm, "morado", 0.45)
#findAssocs(tdm, frecuentes, rep(0.45, rep=5))

################################################################################

m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing=TRUE)
df <- data.frame(word=names(v), freq=v)

################################################################################

library(ggplot2)
library(tidyverse)

df[1:20,] %>% 
  ggplot() +
  aes(freq, y=reorder(word,freq)) +
  geom_bar(stat="identity", color="white", fill="midnightblue") +
  geom_text(aes(label=freq, hjust=1.5), color="white") +
  labs(
    x = NULL,
    y = "Palabras más frecuentes"
  )

################################################################################

#library(wordcloud)

#x11()
#wordcloud(words = df$word, freq = df$freq, min.freq = 6,
          #max.words=100, random.order=FALSE, rot.per=0.35, 
          #colors=brewer.pal(12, "Set3"))

################################################################################

library(wordcloud2)

nube <- wordcloud2(df, size=0.9, backgroundColor="black", color='random-light',
                   shuffle=F, shape='circle', ellipticity=0.7)

################################################################################

library("htmlwidgets")

saveWidget(nube, "nube.html", selfcontained=F)
