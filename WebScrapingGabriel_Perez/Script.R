#       Instituto  Tecnológico de Costa Rica S.C
#   Created by Gabriel Pérez    gabrielapb1996@gmail.com
#   Web Scraping with R   
#   Introducción al desarrollo de aplicaciones web 

library(rvest)    #Librería utilizada para Web scraping
library(jsonlite) #Librería para manipular JSONs
library(stringr)  #Librería para manipular strings
library(mongolite) #Librería para conectarse con mongoDB
library(xml2)

  #se configura la coneccion con mongodb
  #mongo <- mongo(collection = "Cars", db = "webScrapingDB", url = "mongodb://localhost", verbose = TRUE)
  
  #system("ssh -L 9988:13.85.13.241:27017 gabrielperezb@13.85.13.241 -fN -p22")
  #mongos <- mongo("Cars", url = "mongodb://gabrielperezb:Gabriel75157515@localhost:9988/webScrapingDB")
  mongos <- mongo("Cars", url = "mongodb://gabrielperezb:Gabriel75157515@13.85.13.241:27017/webScrapingDB")
  mongos1 <- mongo("Auditoria", url = "mongodb://gabrielperezb:Gabriel75157515@13.85.13.241:27017/webScrapingDB")
                        
  
  
  #obtenemos la estructura de la pagina por medio del URL
  urlCars <- "http://www.usados.cr/?gclid=CLrg7eiohtMCFQgDhgod3wQD6Q&f_ucr_s_marca=Chevrolet&page=1"
  paginaWeb <- read_html(urlCars)
  #obtenemos los links de paginación
  paginacion <-  paginaWeb %>% html_nodes(".car-pagination a") %>% html_attr('href')
  linksPaginacion = c(urlCars)
  for(i in paginacion){
    linksPaginacion <- c(linksPaginacion,paste0("http://www.usados.cr/",i))
  }
  #eliminamos la última posición porque se repite
  linksPaginacion <- linksPaginacion[-length(linksPaginacion)]
  
  #obtenemos todos los links de los autos
  allLinks <- c()
  for(i in linksPaginacion){
    paginaWeb <- read_html(i)
    allLinks <- c( allLinks,paginaWeb %>% html_nodes("a.show-more") %>% html_attr('href'))
  }
  allLinks <-allLinks[-2]#este link tiene problemas
  allCars <- c()
  for(i in allLinks){
    allCars <- c(allCars,paste0("http://www.usados.cr/",i))
  }
  
  
  
  dataDes <- c()
  dataDescName <- c()
  dataDescValue <- c()
  df <- data.frame()
  listJson <- c()
  for(i in allCars){
    paginaWeb <- read_html(i)
    dataDes <- c()
    dataDes <- c(dataDes, str_replace_all(paste0("Imagen: ",paginaWeb %>% html_nodes("#dtlist_imgVehiculo_0") %>% html_attr('src')), "[\r\n\t]" , ""))
    dataDes <- c(dataDes, str_replace_all( paginaWeb %>% html_nodes(".technical-data ul li") %>% html_text(), "[\r\n\t]" , ""))
    dataDes <- c(dataDes, str_replace_all(paginaWeb %>% html_nodes("h1 span") %>% html_text(), "[\r\n\t' | ']" , ""))
    
    dataDescName <- c()
     for(i in dataDes){
       dataDescName <- c(dataDescName, strsplit(i,":")[[1]][-2][-2])
     }
     
     dataDescValue <- c()
     for(i in dataDes){z
       dataDescValue <- c(dataDescValue, strsplit(i,":")[[1]][-1])
     }
     dataDescValue <- dataDescValue[-1]
     dataDescValue[1] <- paste0("http:",dataDescValue[1]) 
    df <- data.frame(dataDescName,dataDescValue)
    
    dataJsons <- toJSON(df)
    listJson <- c(listJson,dataJsons)
  }


#Para la coleccion de auditoria
  dataFrameAuditoria <- data.frame()
#se toma la fecha del sistema
date <- Sys.Date()
webPage <- "http://www.usados.cr/?gclid=CLrg7eiohtMCFQgDhgod3wQD6Q&f_ucr_s_marca=Chevrolet&page=1"
registersNumber <- length(allCars)
remove(error)
error <-  paste(traceback())
error <- toString(error)
if(length(error)==0){
  status <- "Finalizado"  
}else{
  status <- "Pendiente"
}

listaDatosAuditoriaDesc <- c("fecha","paginaWeb","numeroRegistros","estado","errores")
listaDatosAuditoriaValue <- c(date,webPage,registersNumber,status,error)

dataFrameAuditoria <- data.frame(listaDatosAuditoriaDesc,listaDatosAuditoriaValue)
dataJsons <- toJSON(dataFrameAuditoria)
mongos1$insert(fromJSON(dataJsons))




#insercion DB
mongos$drop()
for(data in listJson){
  dfTemp <- data.frame(data)
  dataJsonsTemp <- toJSON(dfTemp)
  mongos$insert(fromJSON(dataJsonsTemp))
}


