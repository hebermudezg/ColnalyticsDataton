#Librerias necesarias para utilizar Rstudio Server y deperacion de base--------------
library(RODBC)
library(lubridate)
library(chron)
library(car)

con <- odbcDriverConnect('driver={ODBC Driver 17 for SQL Server};Server=tcp:hebermudezg.database.windows.net,1433;Database=SENTIMIENTOS_DATATON;Uid=hebermudezg@hebermudezg;Pwd=Esteban7758;Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;')
pagadores_muestra <- sqlQuery(con,'SELECT * FROM dbo.pagadores_muestra')


con2 <- odbcDriverConnect('driver={ODBC Driver 17 for SQL Server};Server=tcp:hebermudezg.database.windows.net,1433;Database=SENTIMIENTOS_DATATON22;Uid=hebermudezg@hebermudezg;Pwd=Esteban7758;Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;')
trxpse_personas <- sqlQuery(con2,'SELECT * FROM dbo.trxpse_personas')

#************************************************************************************

#Crear base para realizar depuracion----------------------------------
datos1 <- trxpse_personas
datos1 <- datos1[,-c(4,8)]

#Elinimar transacciones incorrectas con informacion suficiente, mala tabulacion de datos etc
datos1 <- subset(datos1, datos1$fecha!=is.na(datos1$fecha))

#Corvertir variables a formato correcto------------------
datos1$id_trn_ach <- as.character(datos1$id_trn_ach)
datos1$id_trn_ach <- as.numeric(datos1$id_trn_ach)
datos1$id_cliente <- as.character(datos1$id_cliente)
datos1$id_cliente <- as.numeric(datos1$id_cliente)
datos1$valor_trx <- as.character(datos1$valor_trx)
datos1$valor_trx <- as.numeric(datos1$valor_trx)

#Correccion de espacios NA, nully espacios en blanco 
datost <- datos1

datost$ref1[datost$ref1==""] <- NA
datost$ref1[datost$ref1=="null"] <- NA

datost$ref2[datost$ref2==""] <- NA
datost$ref2[datost$ref2=="null"] <- NA

datost$sector[datost$sector==" "] <- NA
datost$sector[datost$sector==""] <- NA
datost$sector[datost$sector=="null"] <- NA
datost$sector[datost$sector=="\\N"] <- NA

datost$subsector[datost$subsector==" "] <- NA
datost$subsector[datost$subsector==""] <- NA
datost$subsector[datost$subsector=="null"] <- NA
datost$subsector[datost$subsector=="\\N"] <- NA

datost$descripcion[datost$descripcion==" "] <- NA
datost$descripcion[datost$descripcion==""] <- NA
datost$descripcion[datost$descripcion=="null"] <- NA
datost$descripcion[datost$descripcion=="\\N"] <- NA

#Eliminar filas con NA en todas las columnas de (referencias, sector, 
#subsector y descripcion) 
na_ref1  <- which(is.na(datost$ref1)==T)
borrar <- c()
cont <- 1
for (i in 1:length(na_ref1)) {
  if(is.na(datost$ref2[na_ref1[i]])==T & 
     is.na(datost$subsector[na_ref1[i]])==T &
     is.na(datost$descripcion[na_ref1[i]])==T & 
     is.na(datost$sector[na_ref1[i]])==T){
    borrar[cont] <- na_ref1[i]
    cont <- cont+1
  }
}
datost <- datost[-borrar,]

#Convertir culomna fecha al formato adecuado---------------
datost$fecha <- as.character(datost$fecha)
datost$fecha <- ymd(datost$fecha)

#Creacion de bases filtradas por sector------------------
agroindustria <- subset(datost, datost$sector=="AGROINDUSTRIA")
comercio <- subset(datost, datost$sector=="COMERCIO")
construccion <- subset(datost, datost$sector=="CONSTRUCCION")
gobierno <- subset(datost, datost$sector=="GOBIERNO")
manufactura <- subset(datost, datost$sector=="MANUFACTURA INSUMOS")
medios_c <- subset(datost, datost$sector=="MEDIOS DE COMUNICACION")
personas <- subset(datost, datost$sector=="PERSONAS")
recursos_n <- subset(datost, datost$sector=="RECURSOS NATURALES")
servicios_f <- subset(datost, datost$sector=="SERVICIOS FINANCIEROS")
servicios_nf <- subset(datost, datost$sector=="SERVICIOS NO FINANCIEROS")
datosna <- datost[is.na(datost$sector),]

#Guardado de sub base--------------------------------
write.csv2(agroindustria, file="agroindustri.csv", row.names=F)
