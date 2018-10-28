
# cargando librerias
library(tm) # text mining
library(e1071) # maquinas de soporte vectorial



# informacion recopilada de una nueva transaccion (pse) que luego sera categorizada 
# acontinuacion se muestra un ejemplo de de una transaccion (pse)

ref1 <- "CPV"
ref2 <- ""
subsector <- "BANCOS"
descripcion <- "Bancos comerciales"


nueva_pse <- data.frame(nueva_pse = c(ref1, ref2, subsector, descripcion))




# se hacen los data frame con las palabras clave seleccionadas

Mascotas <- data.frame(Mascota=c("mineral","produccion","agua",
                                 "alocohólico","bebido","concentrado",
                                 "conserva","alimento","bebida",
                                 "elaboración","animal","alimenticio",
                                 "aliño","producto","salsa,elaboración"))

Comidas <- data.frame(Comidas=c("pedido","grupo","comedor",
                                "factura","refrigerio","pago",
                                "compra"))

Industria <- data.frame(Industria=c("cemento","fabricaciòn","cal","yeso",
                                    "ferretería","construcción","material",
                                    "pintura","producto","vidrio","artículo",
                                    "edicion","imprenta","maquinaria,comercio"))

Pagos_recurrentes <- data.frame(Pagos_recurrentes=c("pago","mensualidad","arrendamiento",
                                                    "pensión","capital","rentista",
                                                    "persona","natural","capital",
                                                    "contrato","colegio","local",
                                                    "apartamento",
                                                    "peaje","transporte","pasajeros","pasajero",
                                                    "mensualidad","deposito","internacional",
                                                    "terrestre","ingeniería","construcción",
                                                    "carga","edificación","edificio","vía",
                                                    "aéreo","infraestructura","aereo","obra",
                                                    "ferrocarril","construcción")) 

Comercio <- data.frame(Comercio=c("pago","pse","terpel","recarga","numero",
                                  "combustible","lubricante","automobil",
                                  "concecionario","surtido","especializado",
                                  "establecimiento","líquido","lubricante",
                                  "cambio","automotor","gases","recarga",
                                  "terpel","pse","numero","pago","web","sitio",
                                  "compra","farmacéutico","utensilio","escritorio",
                                  "material","producto","libro","papelería",
                                  "tabaco","bebido"))

Actividades_corporativas <- data.frame(Actividades_corporativas=
                                         c("actividad","asociación","empleador","apoyo",
                                           "empresarial","religioso","educación","personal",
                                           "inmobiliario","propio","arrendado","procesamiento",
                                           "dato"))

Educación <- data.frame(Educación=c("pago","educativo","estudiante","documento","cuenta",
                                    "certificado","electrónico","usuario"))

Servicios <- data.frame(Servicios=c("publico","público","empresa","medellín",
                                    "recurso","natural","electricidad","generación",
                                    "energía","eléctrico"))

Impuestos <- data.frame(Impuestos=c("gobierno","público","actividad","ejecutivo",
                                    "central","administración","compensación",
                                    "cajo","municipio","educativo","establecimiento",
                                    "seguridad","afiliación","social","obligatorio",
                                    "técnico","profesional","educación","pago",
                                    "automotor","impuesto","vehículo","presentación",
                                    "predial","comprar"))

Pago_deudas <- data.frame(Pago_deuda =c("banco","comercial","seguro","pensión",
                                  "financiero","actividad","servicio",
                                  "occidente","tarjeta","visa","pse",
                                  "factura","credito","card","pago",
                                  "obligación","obligacion","prestamo",
                                  "tarjeto","credito","factura","card",
                                  "vehiculo","recaudo","occidente",
                                  "crédito","mastercard","consumo",
                                  "vehículo","peso","hipotecario","pago",
                                  "cartera","pedido","factura","anticipo","internet"))

Tecnologia_comunicacion <- data.frame(Tecnologia_comunicacion =c("comunicación","medio","fijar",
                                              "telefonía","valor","agregado",
                                              "ciclo","factura","recarga","fijo",
                                              "pago","saldo","celular","referencia",
                                              "prepago","movil","número","express",
                                              "ref","express","servicio",
                                              "telecomunicación","inalámbrico",
                                              "actividad","telefonía"))





# Uniendo las palabras

Mascotas <-paste(Mascotas$Mascota, collapse = " ")
Comidas <- paste(Comidas$Comidas, collapse = " ")
Industria <- paste(Industria$Industria, collapse = "")
Pagos_recurrentes <-paste(Pagos_recurrentes$Pagos_recurrentes, collapse = " ") 
Comercio <- paste(Comercio$Comercio, collapse = " ")
Actividades_corporativas <- paste(Actividades_corporativas$Actividades_corporativas, collapse = " ")
Educación <- paste(Educación$Educación, collapse = " ")
Servicios <- paste(Servicios$Servicios, collapse = " ")
Impuestos <- paste(Impuestos$Impuestos, collapse = " ")
Pago_deudas <- paste(Pago_deudas$Pago_deuda, collapse = " ")
Tecnologia_comunicacion <-paste(Tecnologia_comunicacion$Tecnologia_comunicacion, collapse = " ")

nueva_pse <- paste(nueva_pse$nueva_pse, collapse = " ")


# convirtiendo a objeto vector corpus (coleccion de textos=Categorias)
unidos <- c(nueva_pse, Mascotas, Comidas,Industria,Pagos_recurrentes,Comercio,Actividades_corporativas,Educación,Servicios,Impuestos,Pago_deudas,Tecnologia_comunicacion)
corpus <- VCorpus(VectorSource(unidos))



# areglando el texto 
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))


# conviertiendo en matrix terminos-documentos (terminos-categorias)
corpus <- DocumentTermMatrix(corpus)
inspect(corpus)


# matriz e terminos-documentos 
# necesito calcular el mejor indicador para evaluar en este caos el (tf-Idf)
tfidf_DT <- suppressWarnings(weightTfIdf(corpus))
terms_DT <- tfidf_DT$dimnames$Terms
str(tfidf_DT)

cat1 <- as.vector(tfidf_DT["1", ])
cat2 <- as.vector(tfidf_DT["2",])
cat3 <- as.vector(tfidf_DT["3",])
cat4 <- as.vector(tfidf_DT["4",])
cat5 <- as.vector(tfidf_DT["5",])
cat6 <- as.vector(tfidf_DT["6",])
cat7 <- as.vector(tfidf_DT["7",])
cat8 <- as.vector(tfidf_DT["8",])
cat9 <- as.vector(tfidf_DT["9",])
cat10 <- as.vector(tfidf_DT["10",])
cat11 <- as.vector(tfidf_DT["11",])
cat12 <- as.vector(tfidf_DT["12",])





#Distancia del coseno para medir similutud entre las paralabras en cada categoria 
coseno <- function(x, y){
  resultado <- x%*%y / (sqrt(x %*% x) * sqrt(y %*%y ))
  return(as.numeric(resultado))
}


resultadosfuncioncoseno  <- data.frame(Mascotas=coseno(cat1, cat2),Comidas=coseno(cat1, cat3),Industria=coseno(cat1, cat4),Pagos_recurrentes=coseno(cat1, cat5),Comercio=coseno(cat1, cat6),Actividades_corporativas=coseno(cat1, cat7),Educación=coseno(cat1, cat8),Servicios=coseno(cat1, cat9),Impuestos=coseno(cat1, cat10),Pago_deudas=coseno(cat1, cat11),Tecnologia_comunicacion=coseno(cat1, cat12))
which.max(resultadosfuncioncoseno)  # mostrar la categoria con la que la conincidencia es maxima


####  creando modelo de clasificacion con maquinas de soporte vectorial ***

# para entrenar el modelo se parte de la matiz de terminos-documentos antes calculada 
a <- data.frame(as.matrix(tfidf_DT))
categorias <- c("nueva_pse","Mascotas","Comidas","Industria","Pagos_recurrentes","Comercio","Actividades_corporativas","Educación","Servicios","Impuestos","Pago_deudas","Tecnologia_comunicacion")
a <- cbind(a, categorias)
modelo <- svm(categorias~. ,data = a[2:12,], kernel="radial") 


# se predice a que categoria esta asignada la nueva transaccion (pse)
prediccion <- predict(modelo, newdata = a[1,])
prediccion

