#librerias necesarias------------------
library(udpipe)
library(textrank)
library(ggplot2)
library(lattice)
library(igraph)
library(ggraph)
library(tm)

#creacion de modelos para analizar co-ocurrencias---------
ud_model <- udpipe_download_model(language = "spanish")
ud_model <- udpipe_load_model(ud_model$file_model)

#creacion de categorias con la base filtrada por sector agroindustrial--------------- 
agroindustria <- read.csv2("agroindustria.csv", header=T,
                           encoding="UTF-8")

#creacion del vector para procesamiento de lenguaje natural
cadena0 <- tolower(na.omit(agroindustria$ref1))
cadena1 <- tolower(na.omit(agroindustria$ref2))
cadena2 <- tolower(na.omit(agroindustria$subsector))
cadena3 <- tolower(na.omit(agroindustria$descripcion))
concatenar <- c(cadena0, cadena1, cadena2, cadena3)

#Filtrado de texto 
corp <- Corpus(VectorSource(concatenar))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords("spanish"))
corp <- tm_map(corp, PlainTextDocument)
concatenar <- corp[["content"]]
concatenar <- concatenar[["content"]]

#Analsis de procesamiento de lenguaje natural
x <- udpipe_annotate(ud_model, x=concatenar)
x <- as.data.frame(x)

#Elininar palapras comunes 
stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)

#Grafico de frecuencia de palabras
stats$key <- factor(stats$key, levels=rev(stats$key))
barchart(key~freq, data=head(stats, 15), 
         col="cadetblue", main="Most occurring nouns", xlab="Freq")

## Colocación (palabras que se concurren)
stats <- keywords_collocation(x=x, 
                              term="token", group=c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)

#frecuencia las palabras se siguen unas a otras, incluso
#si omitimos 2 palabras en el medio
stats <- cooccurrence(x=x$lemma, 
                      relevant=x$upos %in% c("NOUN","ADJ"), 
                      skipgram=2)

#Grafico de co-ocurencia para analizar posibles categorias 
wordnetwork <- head(stats, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") + theme_light() +
  labs(title = "Co-ocurencias", subtitle="Nouns & Adjective")

#creacion de categorias con la base filtrada por sector manufactura--------------- 
manufactura <- read.csv2("manufactura.csv", header=T,
                           encoding="UTF-8")

#creacion del vector para procesamiento de lenguaje natural
cadena0 <- tolower(na.omit(manufactura$ref1))
cadena1 <- tolower(na.omit(manufactura$ref2))
cadena2 <- tolower(na.omit(manufactura$subsector))
cadena3 <- tolower(na.omit(manufactura$descripcion))
concatenar <- c(cadena0, cadena1, cadena2, cadena3)

#Filtrado de texto 
corp <- Corpus(VectorSource(concatenar))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords("spanish"))
corp <- tm_map(corp, PlainTextDocument)
concatenar <- corp[["content"]]
concatenar <- concatenar[["content"]]

#Analsis de procesamiento de lenguaje natural
x <- udpipe_annotate(ud_model, x=concatenar)
x <- as.data.frame(x)

#Elininar palapras comunes 
stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)

#Grafico de frecuencia de palabras
stats$key <- factor(stats$key, levels=rev(stats$key))
barchart(key~freq, data=head(stats, 20), 
         col="cadetblue", main="Most occurring nouns", xlab="Freq")

## Colocación (palabras que se concurren)
stats <- keywords_collocation(x=x, 
                              term="token", group=c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)

#frecuencia las palabras se siguen unas a otras, incluso
#si omitimos 2 palabras en el medio
stats <- cooccurrence(x=x$lemma, 
                      relevant=x$upos %in% c("NOUN","ADJ"), 
                      skipgram=2)

#Grafico de co-ocurencia para analizar posibles categorias 
wordnetwork <- head(stats, 200)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") + theme_light() +
  labs(title = "Co-ocurencias", subtitle="Nouns & Adjective")

#creacion de categorias con la base filtrada por sector personas--------
personas <- read.csv2("personas.csv", header=T,
                      encoding="UTF-8")

#creacion del vector para procesamiento de lenguaje natural
cadena0 <- tolower(na.omit(personas$ref1))
cadena1 <- tolower(na.omit(personas$ref2))
cadena2 <- tolower(na.omit(personas$subsector))
cadena3 <- tolower(na.omit(personas$descripcion))
concatenar <- c(cadena0, cadena1, cadena2, cadena3)

#Filtrado de texto 
corp <- Corpus(VectorSource(concatenar))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords("spanish"))
corp <- tm_map(corp, PlainTextDocument)
concatenar <- corp[["content"]]
concatenar <- concatenar[["content"]]

#Analsis de procesamiento de lenguaje natural
x <- udpipe_annotate(ud_model, x=concatenar)
x <- as.data.frame(x)

#Elininar palapras comunes 
stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)

#Grafico de frecuencia de palabras
stats$key <- factor(stats$key, levels=rev(stats$key))
barchart(key~freq, data=head(stats, 20), 
         col="cadetblue", main="Most occurring nouns", xlab="Freq")

## Colocación (palabras que se concurren)
stats <- keywords_collocation(x=x, 
                              term="token", group=c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)

#frecuencia las palabras se siguen unas a otras, incluso
#si omitimos 2 palabras en el medio
stats <- cooccurrence(x=x$lemma, 
                      relevant=x$upos %in% c("NOUN","ADJ"), 
                      skipgram=2)

#Grafico de co-ocurencia para analizar posibles categorias 
wordnetwork <- head(stats, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") + theme_light() +
  labs(title = "Co-ocurencias", subtitle="Nouns & Adjective")

#creacion de categorias con la base filtrada por sector comercio------
comercio <- read.csv2("comercio.csv", header=T,
                      encoding="UTF-8")

#Creacion de variables para procesamiento de lenguaje natural
cadena0 <- tolower(na.omit(comercio$ref1))
cadena1 <- tolower(na.omit(comercio$ref2))
cadena2 <- tolower(na.omit(comercio$subsector))
cadena3 <- tolower(na.omit(comercio$descripcion))
concatenar <- c(cadena0, cadena1, cadena2, cadena3)

#Filtrado de texto 
corp <- Corpus(VectorSource(concatenar))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords("spanish"))
corp <- tm_map(corp, PlainTextDocument)
concatenar <- corp[["content"]]
concatenar <- concatenar[["content"]]

#Analsis de procesamiento de lenguaje natural
x <- udpipe_annotate(ud_model, x=concatenar)
x <- as.data.frame(x)

#Elininar palapras comunes 
stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)

#Grafico de frecuencia de palabras
stats$key <- factor(stats$key, levels=rev(stats$key))
barchart(key~freq, data=head(stats, 20), 
         col="cadetblue", main="Most occurring nouns", xlab="Freq")

## Colocación (palabras que se concurren)
stats <- keywords_collocation(x=x, 
                              term="token", group=c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)

#frecuencia las palabras se siguen unas a otras, incluso
#si omitimos 2 palabras en el medio
stats <- cooccurrence(x=x$lemma, 
                      relevant=x$upos %in% c("NOUN","ADJ"), 
                      skipgram=2)

#Grafico de co-ocurencia para analizar posibles categorias 
wordnetwork <- head(stats, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") + theme_light() +
  labs(title = "Co-ocurencias", subtitle="Nouns & Adjective")

#creacion de categorias con la base filtrada por sector construccion------
construccion <- read.csv2("construccion.csv", header=T,
                      encoding="UTF-8")

#Creacion de variables para procesamiento de lenguaje natural
cadena0 <- tolower(na.omit(construccion$ref1))
cadena1 <- tolower(na.omit(construccion$ref2))
cadena2 <- tolower(na.omit(construccion$subsector))
cadena3 <- tolower(na.omit(construccion$descripcion))
concatenar <- c(cadena0, cadena1, cadena2, cadena3)

#Filtrado de texto 
corp <- Corpus(VectorSource(concatenar))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords("spanish"))
corp <- tm_map(corp, PlainTextDocument)
concatenar <- corp[["content"]]
concatenar <- concatenar[["content"]]

#Analsis de procesamiento de lenguaje natural
x <- udpipe_annotate(ud_model, x=concatenar)
x <- as.data.frame(x)

#Elininar palapras comunes 
stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)

#Grafico de frecuencia de palabras
stats$key <- factor(stats$key, levels=rev(stats$key))
barchart(key~freq, data=head(stats, 20), 
         col="cadetblue", main="Most occurring nouns", xlab="Freq")

## Colocación (palabras que se concurren)
stats <- keywords_collocation(x=x, 
                              term="token", group=c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)

#frecuencia las palabras se siguen unas a otras, incluso
#si omitimos 2 palabras en el medio
stats <- cooccurrence(x=x$lemma, 
                      relevant=x$upos %in% c("NOUN","ADJ"), 
                      skipgram=2)

#Grafico de co-ocurencia para analizar posibles categorias 
wordnetwork <- head(stats, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") + theme_light() +
  labs(title = "Co-ocurencias", subtitle="Nouns & Adjective")

#creacion de categorias con la base filtrada por sector servicios no financieros------
servicios_nf <- read.csv2("servicios_nf.csv", header=T,
                          encoding="UTF-8")

#Creacion de variables para procesamiento de lenguaje natural
cadena0 <- tolower(na.omit(servicios_nf$ref1))
cadena1 <- tolower(na.omit(servicios_nf$ref2))
cadena2 <- tolower(na.omit(servicios_nf$subsector))
cadena3 <- tolower(na.omit(servicios_nf$descripcion))
concatenar <- c(cadena0, cadena1, cadena2, cadena3)

#Filtrado de texto 
corp <- Corpus(VectorSource(concatenar))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords("spanish"))
corp <- tm_map(corp, PlainTextDocument)
concatenar <- corp[["content"]]
concatenar <- concatenar[["content"]]

#Analsis de procesamiento de lenguaje natural
x <- udpipe_annotate(ud_model, x=concatenar)
x <- as.data.frame(x)

#Elininar palapras comunes 
stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)

#Grafico de frecuencia de palabras
stats$key <- factor(stats$key, levels=rev(stats$key))
barchart(key~freq, data=head(stats, 20), 
         col="cadetblue", main="Most occurring nouns", xlab="Freq")

## Colocación (palabras que se concurren)
stats <- keywords_collocation(x=x, 
                              term="token", group=c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)

#frecuencia las palabras se siguen unas a otras, incluso
#si omitimos 2 palabras en el medio
stats <- cooccurrence(x=x$lemma, 
                      relevant=x$upos %in% c("NOUN","ADJ"), 
                      skipgram=2)

#Grafico de co-ocurencia para analizar posibles categorias 
wordnetwork <- head(stats, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") + theme_light() +
  labs(title = "Co-ocurencias", subtitle="Nouns & Adjective")

#creacion de categorias con la base filtrada por sector recursos naturales-----
recursos_n <- read.csv2("recursos_n.csv", header=T,
                        encoding="UTF-8")

#Creacion de variale para procesamiento de lenguaje natural
recursos_n <- recursos_n[sample(x=442490, size=50000),]
cadena0 <- tolower(na.omit(recursos_n$ref1))
cadena1 <- tolower(na.omit(recursos_n$ref2))
cadena2 <- tolower(na.omit(recursos_n$subsector))
cadena3 <- tolower(na.omit(recursos_n$descripcion))
concatenar <- c(cadena0, cadena1, cadena2, cadena3)

#Filtrado de texto 
corp <- Corpus(VectorSource(concatenar))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords("spanish"))
corp <- tm_map(corp, PlainTextDocument)
concatenar <- corp[["content"]]
concatenar <- concatenar[["content"]]

#Analsis de procesamiento de lenguaje natural
x <- udpipe_annotate(ud_model, x=concatenar)
x <- as.data.frame(x)

#Elininar palapras comunes 
stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)

#Grafico de frecuencia de palabras
stats$key <- factor(stats$key, levels=rev(stats$key))
barchart(key~freq, data=head(stats, 20), 
         col="cadetblue", main="Most occurring nouns", xlab="Freq")

## Colocación (palabras que se concurren)
stats <- keywords_collocation(x=x, 
                              term="token", group=c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)

#frecuencia las palabras se siguen unas a otras, incluso
#si omitimos 2 palabras en el medio
stats <- cooccurrence(x=x$lemma, 
                      relevant=x$upos %in% c("NOUN","ADJ"), 
                      skipgram=2)

#Grafico de co-ocurencia para analizar posibles categorias 
wordnetwork <- head(stats, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") + theme_light() +
  labs(title = "Co-ocurencias", subtitle="Nouns & Adjective")

#creacion de categorias con la base filtrada por sector gobierno---------
gobierno <- read.csv2("gobierno.csv", header=T,
                      encoding="UTF-8")

#Creacion de variables para procesamiento de lenguaje natural
gobierno <- gobierno[sample(x=520126, size=50000),]
cadena0 <- tolower(na.omit(gobierno$ref1))
cadena1 <- tolower(na.omit(gobierno$ref2))
cadena2 <- tolower(na.omit(gobierno$subsector))
cadena3 <- tolower(na.omit(gobierno$descripcion))
concatenar <- c(cadena0, cadena1, cadena2, cadena3)

#Filtrado de texto 
corp <- Corpus(VectorSource(concatenar))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords("spanish"))
corp <- tm_map(corp, PlainTextDocument)
concatenar <- corp[["content"]]
concatenar <- concatenar[["content"]]

#Analsis de procesamiento de lenguaje natural
x <- udpipe_annotate(ud_model, x=concatenar)
x <- as.data.frame(x)

#Elininar palapras comunes 
stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)

#Grafico de frecuencia de palabras
stats$key <- factor(stats$key, levels=rev(stats$key))
barchart(key~freq, data=head(stats, 20), 
         col="cadetblue", main="Most occurring nouns", xlab="Freq")

## Colocación (palabras que se concurren)
stats <- keywords_collocation(x=x, 
                              term="token", group=c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)

#frecuencia las palabras se siguen unas a otras, incluso
#si omitimos 2 palabras en el medio
stats <- cooccurrence(x=x$lemma, 
                      relevant=x$upos %in% c("NOUN","ADJ"), 
                      skipgram=2)

#Grafico de co-ocurencia para analizar posibles categorias 
wordnetwork <- head(stats, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") + theme_light() +
  labs(title = "Co-ocurencias", subtitle="Nouns & Adjective")

#creacion de categorias con la base filtrada por sector servicios no finacieros----------
servicios_f <- read.csv2("servicios_f.csv", header=T,
                         encoding="UTF-8")

#Creacion de variables para procesamiento de lenguaje natural
servicios_f <- servicios_f[sample(x=1061605, size=50000),]
cadena0 <- tolower(na.omit(servicios_f$ref1))
cadena1 <- tolower(na.omit(servicios_f$ref2))
cadena2 <- tolower(na.omit(servicios_f$subsector))
cadena3 <- tolower(na.omit(servicios_f$descripcion))
concatenar <- c(cadena0, cadena1, cadena2, cadena3)

#Filtrado de texto 
corp <- Corpus(VectorSource(concatenar))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords("spanish"))
corp <- tm_map(corp, PlainTextDocument)
concatenar <- corp[["content"]]
concatenar <- concatenar[["content"]]

#Analsis de procesamiento de lenguaje natural
x <- udpipe_annotate(ud_model, x=concatenar)
x <- as.data.frame(x)

#Elininar palapras comunes 
stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)

#Grafico de frecuencia de palabras
stats$key <- factor(stats$key, levels=rev(stats$key))
barchart(key~freq, data=head(stats, 20), 
         col="cadetblue", main="Most occurring nouns", xlab="Freq")

## Colocación (palabras que se concurren)
stats <- keywords_collocation(x=x, 
                              term="token", group=c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)

#frecuencia las palabras se siguen unas a otras, incluso
#si omitimos 2 palabras en el medio
stats <- cooccurrence(x=x$lemma, 
                      relevant=x$upos %in% c("NOUN","ADJ"), 
                      skipgram=2)

#Grafico de co-ocurencia para analizar posibles categorias 
wordnetwork <- head(stats, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") + theme_light() +
  labs(title = "Co-ocurencias", subtitle="Nouns & Adjective")

#creacion de categorias con la base filtrada por sector medios de comunicacion--------
medios_c <- read.csv2("medios_c.csv", header=T,
                      encoding="UTF-8")

#Creacion de variables para procesamineto de lenguaje natural
medios_c <- medios_c[sample(x=1173421, size=50000),]
cadena0 <- tolower(na.omit(medios_c$ref1))
cadena1 <- tolower(na.omit(medios_c$ref2))
cadena2 <- tolower(na.omit(medios_c$subsector))
cadena3 <- tolower(na.omit(medios_c$descripcion))
concatenar <- c(cadena0, cadena1, cadena2, cadena3)

#Filtrado de texto 
corp <- Corpus(VectorSource(concatenar))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords("spanish"))
corp <- tm_map(corp, PlainTextDocument)
concatenar <- corp[["content"]]
concatenar <- concatenar[["content"]]

#Analsis de procesamiento de lenguaje natural
x <- udpipe_annotate(ud_model, x=concatenar)
x <- as.data.frame(x)

#Elininar palapras comunes 
stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)

#Grafico de frecuencia de palabras
stats$key <- factor(stats$key, levels=rev(stats$key))
barchart(key~freq, data=head(stats, 20), 
         col="cadetblue", main="Most occurring nouns", xlab="Freq")

## Colocación (palabras que se co-ocurrencias)
stats <- keywords_collocation(x=x, 
                              term="token", group=c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)

#frecuencia las palabras se siguen unas a otras, incluso
#si omitimos 2 palabras en el medio
stats <- cooccurrence(x=x$lemma, 
                      relevant=x$upos %in% c("NOUN","ADJ"), 
                      skipgram=2)

#Grafico de co-ocurencia para analizar posibles categorias 
wordnetwork <- head(stats, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") + theme_light() +
  labs(title = "Co-ocurencias", subtitle="Nouns & Adjective")
