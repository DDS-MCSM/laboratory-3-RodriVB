#******************************************************************************#
#                                                                              #
#                    Lab 3 - Data Acquisition & Analysis                       #
#                                                                              #
#                     Your Name - Data Driven Securty                          #
#                                                                              #
#******************************************************************************#

## Crawling y Scrapping
library(httr)
library(xml2)
library(XML)

url = "https://www.mediawiki.org/wiki/MediaWiki"

### 1.1 Obtención de la página web

ObtenerWeb <- function(){
  mediawiki_http <- GET(url)
  res <- XML::xmlParse(mediawiki_http)
  web_xml <- XML::xmlRoot(XML::xmlRoot(res))
  return(list("web_html" = mediawiki_http, "web_xml" = web_xml))
}


### 1.2 Analisis de el contenido de la web

ObtenerContenido <- function(web_xml){
  titulo <- XML::xpathSApply(web_xml,"//title", XML::xmlValue)
  dns <- XML::xpathSApply(web_xml, "//*/link[@rel='dns-prefetch']", XML::xmlGetAttr, 'href')
  return(list("titulo" = titulo, "dns" = dns))
}

### 1.3.	Extracción de enlaces
ObtenerEnlaces <- function(web_xml){
  valor <- XML::xpathSApply(web_xml, "//*/a/@href/parent::a", XML::xmlValue)
  enlace <- XML::xpathSApply(web_xml, "//*/a/@href/parent::a", XML::xmlGetAttr, 'href')
  df_web <- data.frame(valor,enlace)
  return(df_web)
  }

### 1.4 Exploración de enlaces
RegularUrl <- function(df_web){
  url_absoluta <- df_web[grep(pattern = "^http", x = df_web$enlace, perl = F), ]
  url_delDomnio <- df_web[grep(pattern = "^/w", x = df_web$enlace, perl = F), ]
  url_sinHttp <- df_web[grep(pattern = "^//", x = df_web$enlace, perl = F), ]

  url_absoluta$url <- paste0("", url_absoluta$enlace)
  url_absoluta$tipo <- "Absoluta"
  url_delDomnio$url <- paste0("https://www.mediawiki.org", url_delDomnio$enlace)
  url_delDomnio$tipo <- "Del Dominio"
  url_sinHttp$url <- paste0("https:", url_sinHttp$enlace)
  url_sinHttp$tipo <- "Sin HTTPS"
  df_regulado <- rbind(url_absoluta,url_delDomnio, url_sinHttp)
  
  nombre <- gsub("http://|https://|www\\.", "", df_regulado$url)
  dominio <- sapply(strsplit(nombre, "/"), `[`, 1)
  
  df_regulado$dominio <- dominio
  
  num_items <- dim(df_regulado)[1]
  
  for(i in 1:num_items) {
    # Get Response Code for each URL HEAD(url)
    tmphead <- HEAD(df_regulado$url[1])
    df_regulado$codigo_estado[i] <- tmphead$status
    
    Sys.sleep(time = 0.001)
  }
  
  df <- df_regulado[,c(1,4,5,6,2,3)]
  
  return(df)
}

UrlRaras <- function(df_web){
  url_raros <- df_web[-grep(pattern = "^http", x = df_web$enlace, perl = F), ]
  url_raros <- url_raros[-grep(pattern = "^/w", x = url_raros$enlace, perl = F), ]
  url_raros <- url_raros[-grep(pattern = "^//", x = url_raros$enlace, perl = F), ]
  
  return(url_raros)
}


### Gráficos en R

### 2.1 Histograma

### 2.2 Un gráfico de barras

### 2.3 Pie Chart

