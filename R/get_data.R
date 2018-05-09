#' @title Download and read morbidity data
#' @description Download morbidity data from INE ftp and parse it
#' @param year Year of morbidity data
#' @return data frame with morbidity data prov_hosp, sexo, prov_res, diag_in, diag_ppal, motivo_alta, estancia, fecha_ingreso, edad
#' @details Uses read.fwf 
#' @examples
#' data <- ReadZip(2010)

ReadZip <- function(year){
  #descomprime
  #filezip <- sprintf("https://github.com/rafaelmenmell/MorbiditySpain/raw/master/data/datos_morbi%s.zip",substr(year,3,4))
  if (year<2015){
    filezip <- sprintf("ftp://www.ine.es/temas/morbihos/datos_%s.zip",year,3,4)
    temp <- tempfile()
    download.file(filezip,temp)
    fileu <- unzip(temp,list=TRUE)$Name
    unzip(temp)
    unlink(temp)
    data <- read_fwf(fileu,fwf_widths(c(8,2,1,2,1,6,4,1,3,2,2,6,8,8)))
    unlink(fileu)
  } else {
    data <- ReadZip2015(year)
    if (year==2016){
      data$elevacion <- NA
    }
  }
  #data <- read.fwf(fileu,widths = c(8,2,1,2,1,6,4,1,3,2,2,6,8,8),colClasses=rep("character",14))
  colnames(data) <- c("numero","prov_hosp","sexo","prov_res","diag_in","fecha_alta","diag_ppal","motivo_alta","edad_anyos","edad_meses","edad_dias","estancia","elevacion","filler")
  #vamos a hacer unos cast para reducir el tamaño del data frame
  data$numero <- as.integer(data$numero)
  data$prov_hosp <- as.integer(data$prov_hosp)
  data$sexo <- as.integer(data$sexo)
  data$prov_res <- as.integer(data$prov_res)
  data$fecha_alta <- ISOdate(year=as.integer(substr(data$fecha_alta,1,2))+trunc(year/100)*100,month = as.integer(substr(data$fecha_alta,3,4)),day = as.integer(substr(data$fecha_alta,5,6)))
  data$motivo_alta <- as.integer(data$motivo_alta)
  data$edad_anyos <- as.integer(data$edad_anyos)
  data$edad_meses <- as.integer(data$edad_meses)
  data$edad_dias <- as.integer(data$edad_dias)
  data$estancia <- as.integer(data$estancia)
  data$fecha_ingreso <- as.Date(data$fecha_alta-data$estancia*24*60*60)
  data$edad <- as.integer(round(data$edad_anyos+data$edad_meses/12+data$edad_dias/365))
  data$edad_anyos <- NULL
  data$edad_meses <- NULL
  data$edad_dias <- NULL
  data$filler <- NULL
  data$elevacion <- NULL
  data$numero <- NULL
  data$fecha_alta <- NULL
  
  return(data)
}

#' @title Download and read morbidity data form year 2015
#' @description Download morbidity data from INE ftp and parse it
#' @param year Year of morbidity data
#' @return data frame with morbidity data prov_hosp, sexo, prov_res, diag_in, diag_ppal, motivo_alta, estancia, fecha_ingreso, edad
#' @details Uses script provided from INE
#' @examples
#' data <- ReadZip(2015)

ReadZip2015 <- function(year){
  filezip <- sprintf("ftp://www.ine.es/temas/morbihos/datos_%s.zip",year,3,4)
  temp <- tempfile()
  download.file(filezip,temp)
  fileu <- unzip(temp,list=TRUE)$Name
  file1 <- fileu[grepl("MD",fileu)]
  unzip(zipfile = temp,files = file1,exdir = ".",junkpaths = TRUE)
  file2 <- fileu[grepl("md_EMH",fileu)]
  unzip(zipfile = temp,files = file2,exdir = ".",junkpaths = TRUE)
  filezip <- sprintf("ftp://www.ine.es/temas/morbihos/disreg_morbi%s.zip",year,3,4)
  temp2 <- tempfile()
  download.file(filezip,temp2)
  fileu <- unzip(temp2,list=TRUE)$Name
  unzip(zipfile = temp2,files = fileu,exdir = ".",junkpaths = TRUE)
  fichero_meta  <- list.files(path = ".",pattern = ".xlsx",full.names = FALSE)
  fichero_micro <- list.files(path = ".", pattern = glob2rx("md*.txt"),full.names = FALSE)
  fileR <- list.files(path = ".",pattern = "MD_EMH",full.names = FALSE)
  #################codigo INE#############################
  #Lectura del fichero de metadatos (METAD), Hoja "Dise?o" de archivo .xlsx
  tryCatch((workBook <- XLConnect::loadWorkbook(fichero_meta)), error=function(e) 
    stop(paste("Error. No se puede abrir el fichero: ", e, fichero_meta,". Saliendo de la ejecucion...", sep = "")))
  df <- XLConnect::readNamedRegion(workBook, name = "METADATOS")
  
  nombresVarbls <- df[,1]
  nombresTablas <- df[,2]
  posiciones    <- df[,3]
  tipo          <- df[,4]
  tamanio       <- length(nombresVarbls)
  
  # Lectura del fichero de microdatos (MICROD)
  if(length(df) == 4){
    cat("Sin formato")
    
    #Capturamos las columnas con su tipo de dato
    tipDatos <- as.vector(sapply(df[,4], function(x){
      if(identical(x, "A"))
        "character"
      else{
        if(identical(x, "N"))
          "numeric"
      }
    }
    )
    )
    # Lectura del fichero de microdatos (MICROD), decimales con punto en MD  
    tryCatch((df1 <- read.fwf(file = fichero_micro, widths= posiciones, colClasses=tipDatos)), error=function(e)
      stop(paste("Error. No se encuentra el fichero: ", e, fichero_micro,". Saliendo de la ejecucion...", sep = "")))
    
  }else{
    formatos <- df[,5]  
    cat("Con formato")
    
    # Lectura del fichero de microdatos (MICROD), decimales sin punto en MD
    tryCatch((df1 <- read.fortran(file = fichero_micro, format= formatos)), error=function(e)
      stop(paste("Error. No se encuentra el fichero: ", e, fichero_micro,". Saliendo de la ejecucion...", sep = "")))
  }
  
  #Aplicamos los nombres de la cabecera del registro
  names(df1) <- df[,1]
  fichero_salida <- df1
  
  
  #Liberacion de memoria y aclaraci?n de variables 
  #Values
  rm(flag_num,workBook,nombresVarbls,nombresTablas,posiciones,tamanio,df,df1)
  if(length(df) == 4){rm(tipDatos)}
  #########################################################
  unlink(temp)
  unlink(temp2)
  unlink(fileR)
  unlink(fichero_micro)
  unlink(fichero_meta)
  fichero_salida$DiagEntr <- as.integer(fichero_salida$DiagEntr)
  return(fichero_salida)
}


#' @title Download and read morbidity data for several years
#' @description Download morbidity data from INE ftp and parse it
#' @param year1 Begining year of morbidity data
#' @param year2 Ending year of morbidity data
#' @return data frame with morbidity data prov_hosp, sexo, prov_res, diag_in, diag_ppal, motivo_alta, estancia, fecha_ingreso, edad
#' @details Uses ReadZip
#' @examples
#' data <- GetMorbiData(y1=2010,y2=2011)

GetMorbiData <- function(y1=2005,y2=2015){
  ys <- y1:y2
  data.m <- vector("list",length(ys))
  n <- 1
  for (y in ys){
    #print(y)
    data.m[[n]] <- suppressMessages(ReadZip(y))
    n <- n+1
  }
  data.m <- dplyr::bind_rows(data.m)
  return(data.m)
}

#' @title Filter morbidity by provincia
#' @description Filter morbidity by provincia using official id of provincias
#' @param data Morbidity data
#' @param provincia ID of provincia
#' @return data frame with morbidity data prov_hosp, sexo, prov_res, diag_in, diag_ppal, motivo_alta, estancia, fecha_ingreso, edad
#' @details Uses dplyr filter
#' @examples
#' data <- data_ejemplo %>% FilterProvincia(28)

FilterProvincia <- function(data,provincia){
  if (!(provincia %in% 1:52)){
    print(provincias)
    stop("provincia must be an integer between 1 and 52")
  }
  data <- data %>% dplyr::filter(prov_hosp==provincia)
  return(data)
}

#' @title Filter morbidity by ER item
#' @description Filter morbidity by ER
#' @param data Morbidity data
#' @return data frame with morbidity data prov_hosp, sexo, prov_res, diag_in, diag_ppal, motivo_alta, estancia, fecha_ingreso, edad
#' @details Uses dplyr filter
#' @examples
#' data <- data_ejemplo %>% FilterEmergency()

FilterEmergency <- function(data){
  data <- data %>% dplyr::filter(diag_in==2)
  return(data)
}

#' @title Filter morbidity by principal diagnosis
#' @description Filter morbidity by principal diagnosis following international classification of diseases
#' @param data Morbidity data
#' @param diagnosis_id id of principal diagnosis
#' @return data frame with morbidity data prov_hosp, sexo, prov_res, diag_in, diag_ppal, motivo_alta, estancia, fecha_ingreso, edad
#' @details Uses dplyr filter
#' @examples
#' data <- data_ejemplo %>% FilterDiagnosis1(2)

FilterDiagnosis1 <- function(data,diagnosis_id){
  if (!(diagnosis_id %in% 1:17)) {
    print(diag1)
    stop("diagnosis_id must be an integer between 1 and 17")
  }
  dd <- diag1 %>% dplyr::filter(id == diagnosis_id)
  data$temp <- as.numeric(substr(gsub("V","",data$diag_ppal),1,3))
  data <- data %>% dplyr::filter(temp >= dd$start) %>% dplyr::filter(temp <= dd$end) %>% dplyr::select(-temp)
  return(data)
}

#' @title Filter morbidity by secondary diagnosis
#' @description Filter morbidity by secondary diagnosis following international classification of diseases
#' @param data Morbidity data
#' @param diagnosis_id id of secondary diagnosis
#' @return data frame with morbidity data prov_hosp, sexo, prov_res, diag_in, diag_ppal, motivo_alta, estancia, fecha_ingreso, edad
#' @details Uses dplyr filter
#' @examples
#' data <- data_ejemplo %>% FilterDiagnosis2(20)

FilterDiagnosis2 <- function(data,diagnosis_id){
  if (!(diagnosis_id %in% 1:123)) {
    print(diag2)
    stop("diagnosis_id must be an integer between 1 and 123")
  }
  dd <- diag2 %>% dplyr::filter(id == diagnosis_id)
  if (!dd$V) {
    data <- data %>% dplyr::filter(grepl("V",diag_ppal) == FALSE)
    data$temp <- as.numeric(substr(gsub("V","",data$diag_ppal),1,3))
    data <-  data %>% dplyr::filter(temp >= dd$start) %>% dplyr::filter(temp <= dd$end) %>% dplyr::select(-temp)
  } else {
    data <- data %>% dplyr::filter(grepl("V",diag_ppal) == TRUE)
    data$temp <- as.numeric(substr(gsub("V","",data$diag_ppal),1,2))
    data <- data  %>% dplyr::filter(temp >= as.numeric(gsub("V","",dd$start))) %>% dplyr::filter(temp <= as.numeric(gsub("V","",dd$end))) %>% dplyr::select(-temp)
  }
  return(data)
}

#' @title Add principal diagnosis to morbity data
#' @description Add principal diagnosis following international classification of diseases
#' @param data Morbidity data
#' @return data frame with morbidity data prov_hosp, sexo, prov_res, diag_in, diag_ppal, motivo_alta, estancia, fecha_ingreso, edad, diag1
#' @examples
#' data <- data_ejemplo %>% AddDiagnosis1()

AddDiagnosis1 <- function(data){
  data$diag1 <- NA
  for (i in 1:nrow(diag1)){
    message(sprintf("%s de %s\r",i,nrow(diag1)),appendLF = FALSE)
    start <- diag1[i,]$start
    end <- diag1[i,]$end
    id <- diag1[i,]$id
    data$temp <- as.numeric(substr(gsub("V","",data$diag_ppal),1,3))
    if(nrow(data[data$temp>=start & data$temp<=end,])>0){
      data[data$temp>=start & data$temp<=end,]$diag1 <- id
    }
  }
  data <- data %>% dplyr::select(-temp)
  return(data)
}

#' @title Add secondary diagnosis to morbity data
#' @description Add secondary diagnosis following international classification of diseases
#' @param data Morbidity data
#' @return data frame with morbidity data prov_hosp, sexo, prov_res, diag_in, diag_ppal, motivo_alta, estancia, fecha_ingreso, edad, diag2
#' @examples
#' data <- data_ejemplo %>% AddDiagnosis2()

AddDiagnosis2 <- function(data){
  data$diag2 <- NA
  for (i in 1:nrow(diag2)){
    message(sprintf("%s de %s\r",i,nrow(diag2)),appendLF = FALSE)
    start <- as.numeric(gsub("V","",diag2[i,]$start))
    end <- as.numeric(gsub("V","",diag2[i,]$end))
    id <- diag2[i,]$id
    if(!diag2[i,]$V){
      data$temp <- as.numeric(substr(gsub("V","",data$diag_ppal),1,3))
      if (nrow(data[!grepl("V",data$diag_ppal) & data$temp>=start & data$temp<=end,])>0){
        data[!grepl("V",data$diag_ppal) & data$temp>=start & data$temp<=end,]$diag2 <- id
      }
    } else {
      data$temp <- as.numeric(substr(gsub("V","",data$diag_ppal),1,2))
      if (nrow(data[grepl("V",data$diag_ppal) & data$temp>=start & data$temp<=end,])>0){
        data[grepl("V",data$diag_ppal) & data$temp>=start & data$temp<=end,]$diag2 <- id
      }
    }
  }
  data <- data %>% dplyr::select(-temp)
  return(data)
}

#' @title Get specific diagnosis
#' @description Get specific diagnosis following international classification of diseases
#' @param codigo code from morbidity data
#' @return Specific diagnosis
#' @examples
#' data <- TraduceCodigoEspecifico(3019)

TraduceCodigoEspecifico <- function(codigo){
  if (nchar(codigo)==4){
    if (grepl("V",codigo)==FALSE){
      codigo <- paste(substr(codigo, 1, 3), ".", substr(codigo, 4, 4), sep = "")
    }
  }
  url <- sprintf("http://icd9cm.chrisendres.com/index.php?srchtype=diseases&srchtext=%s&Submit=Search&action=search",codigo)
  info <- readLines(url)
  info <- info[grepl(codigo,info)][2]
  info <- strsplit(info,codigo)[[1]][2]
  info <- gsub("</div>","",info)
  return(info)
}

#' @title Add specific diagnosis to morbity data
#' @description Add specific diagnosis following international classification of diseases
#' @param data Morbidity data
#' @return data frame with morbidity data prov_hosp, sexo, prov_res, diag_in, diag_ppal, motivo_alta, estancia, fecha_ingreso, edad, diag3
#' @examples
#' data <- data_ejemplo %>% AddDiagnosis3()

AddDiagnosis3 <- function(data){
  codes <- unique(data$diag_ppal)
  data$diag3 <- NA
  for (code in codes){
    i <- 1
    message(sprintf("%s de %s\r",i,length(codes)),appendLF = FALSE)
    diag3 <- TraduceCodigoEspecifico(code)
    data[data$diag_ppal==code,]$diag3 <- diag3
    i <- i + 1
  }
  return(data)
}

