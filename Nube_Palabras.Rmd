---
title: "Nube de palabras"
author: "Autor: Jorge L. C. Musaja"
date: "Marzo 2021"
output: 
  rmdformats::downcute:
    self_contained: true
    thumbnails: false
    lightbox: false
    gallery: false
    highlight: tango
    use_bookdown: true
---

---

# Introducción

El presente documento tiene por objetivo brindar un resumen rápido y dinámico de los planes de gobierno de los partidos políticos que postulan a las Elecciones Generales 2021 en Perú y que vienen encabezando las encuestas de intención de voto.

Para lograr el objetivo mencionado, se elaborarán nubes de palabras con el contenido de los planes de gobierno. 

Las nubes de palabras permiten realizar una representación visual de las palabras que conforman un texto, donde las palabras que aparecen con más frecuencia vienen representadas con textos de mayor tamaño.

Empezaremos por el plan de gobierno del partido político "Acción Popular" que, a la fecha, se encuentra liderando las mencionadas encuestas.

![Fuente: La República. Edición 28/Feb/2021.](/home/jorge/Dropbox/NubePalabras/encuesta.jpeg)

# Importar texto

Se importa el PDF del plan de gobierno directamente de la publicación en el diario oficial "El Peruano"

```{r, warning=FALSE, message=FALSE}
library(pdftools)

text <- pdf_text(
  "https://apisije-e.jne.gob.pe/TRAMITE/ESCRITO/1531/ARCHIVO/FIRMADO/4984.PDF")
text <- iconv(text, to="latin1")
```

# Reemplazar texto

Es normal que en la redacción de los documentos se usen múltiples sinónimos que, como la definición de sinónimos indica, son palabras con el mismo contenido semántico. Por ello, conviene efectuar un reemplazo de palabras.

```{r}
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
```

# Elaboración del corpus

```{r, warning=FALSE, message=FALSE}
library(tm)

corpus <- Corpus(VectorSource(text))
d <- corpus

d <- tm_map(d, tolower) #cambio a minúsculas
d <- tm_map(d, stripWhitespace) #eliminar espacios en blanco
d <- tm_map(d, removePunctuation) #eliminar signos de puntuación
d <- tm_map(d, removeNumbers) #eliminar números
```

# Eliminación de stopwords

Los stopwords son las palabras que por sí solas no guardan un significado. Son comunmente los artículos, pronombres, preposiciones y adverbios. No significa que carezcan de importancia, pero sí es necesario eliminarlas del corpus.

```{r, warning=FALSE, message=FALSE}
lista_remover <- c('maría','ello','así','cada','hace','hacia','vez',
                   'través','sino','dentro','asimismo','mediante',
                   'además') #lista personalizada de stopwords
d <- tm_map(d, removeWords, c(stopwords("spanish"), lista_remover))
```

# Matriz de términos

```{r, warning=FALSE, message=FALSE}
m <- as.matrix(TermDocumentMatrix(d))
v <- sort(rowSums(m), decreasing=TRUE)
df <- data.frame(word=names(v), freq=v)
```

# Revisión de frecuencias

Este paso no es necesario, pero es útil para identificar si la frecuencia de términos es la esperada.

```{r, warning=FALSE, message=FALSE}
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
```

# Nube de palabras

Una vez elaborada la nube de palabras, si el usuario posiciona el puntero del mouse encima de alguna de las palabras podrá observar que debajo de ella aparecerá un texto indicando la frecuencia de la palabra señalada con el puntero.

## Acción Popular

```{r}
cat(paste0("Nube de ",sum(df$freq)," palabras."))

library(wordcloud2)
wordcloud2(df, size=0.75, backgroundColor="black", 
           color='random-light', shuffle=F)
```

Cabe agregar que, las representaciones de los planes de gobierno no son representaciones que permitan, en sí mismas, concluir que plan de gobierno es mejor que otro. Las nubes de palabras se han elaborado con el objetivo de ayudar a una representación resumida del contenido del plan de gobierno que cada partido político propone.

# Función corpus

Dado que los pases realizados los repetiremos para cada plan de gobierno, conviene elaborar una función que facilite esa tarea.

```{r}
library(pdftools)
library(tm)
library(wordcloud2)

makeCorpus <- function(url){
  text <- pdf_text(url)
  text <- iconv(text, to="latin1")
  
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
  
  corpus <- Corpus(VectorSource(text))
  d <- corpus
  
  d <- tm_map(d, tolower)
  d <- tm_map(d, stripWhitespace)
  d <- tm_map(d, removePunctuation)
  d <- tm_map(d, removeNumbers)
  
  lista_remover <- c('maría','ello','así','cada','hace','hacia','vez',
                   'través','sino','dentro','asimismo','mediante',
                   'además') 
  d <- tm_map(d, removeWords, c(stopwords("spanish"), lista_remover))
  
  m <- as.matrix(TermDocumentMatrix(d))
  v <- sort(rowSums(m), decreasing=TRUE)
  df <- data.frame(word=names(v), freq=v)
  
  return(df)
} 

```

# Aplicación de la función makeCorpus

Aplicaremos la función creada en el paso anterior para obtener la nube de palabras de los 5 partidos políticos restantes que encabezan las intenciones de voto presidencial 2021.

## Juntos por el Perú

```{r, warning=FALSE}
df <- makeCorpus(
  "https://apisije-e.jne.gob.pe/TRAMITE/ESCRITO/1587/ARCHIVO/FIRMADO/5262.PDF")
cat(paste0("Nube de ",sum(df$freq)," palabras."))
wordcloud2(df, size=0.75, backgroundColor="black",
           color='random-light', shuffle=F)
```

## Victoria Nacional

```{r, warning=FALSE}
df <- makeCorpus(
  "https://apisije-e.jne.gob.pe/TRAMITE/ESCRITO/1859/ARCHIVO/FIRMADO/7346.PDF")
cat(paste0("Nube de ",sum(df$freq)," palabras."))
wordcloud2(df, size=0.75, backgroundColor="black",
           color='random-light', shuffle=F)
```

## Fuerza Popular

```{r, warning=FALSE}
df <- makeCorpus(
  "https://apisije-e.jne.gob.pe/TRAMITE/ESCRITO/1095/ARCHIVO/FIRMADO/3017.PDF")
cat(paste0("Nube de ",sum(df$freq)," palabras."))
wordcloud2(df, size=0.75, backgroundColor="black",
           color='random-light', shuffle=F)
```

## Renovación Popular

```{r, warning=FALSE}
df <- makeCorpus(
  "https://apisije-e.jne.gob.pe/TRAMITE/ESCRITO/1494/ARCHIVO/FIRMADO/5717.PDF")
cat(paste0("Nube de ",sum(df$freq)," palabras."))
wordcloud2(df, size=0.75, backgroundColor="black",
           color='random-light', shuffle=F)
```

## Podemos Perú

```{r, warning=FALSE}
df <- makeCorpus(
  "https://apisije-e.jne.gob.pe/TRAMITE/ESCRITO/2017/ARCHIVO/FIRMADO/8661.PDF")
cat(paste0("Nube de ",sum(df$freq)," palabras."))
wordcloud2(df, size=0.75, backgroundColor="black",
           color='random-light', shuffle=F, ellipticity=2.0)
```

---
