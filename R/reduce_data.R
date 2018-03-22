#' @title Reduce data to temporal serie
#' @description Reduce morbidity data to temporal serie, including diagnosis, province or sex
#' @param data Morbidity data
#' @param province Boolean, reduce by provicia
#' @param date day, month or year
#' @param diag diag1, daig2, diag3, if NULL not reduced by diagnosis
#' @param sex Boolean, reduce by sex
#' @return data frame with data serie of morbidity
#' @details Uses group_by
#' @examples
#' data <- data_ejemplo %>% ReduceData(provincia = TRUE,date = "day")

ReduceData <- function(data,provincia=TRUE,date="day",diag=NULL,sex=FALSE){
  if(provincia){
    data <- data %>% dplyr::group_by(prov=prov_hosp)
  }
  if (!(date %in% c("day","month","year"))){
    stop("We only know about daily, monthly,yearly reduction")
  }
  if(date=="day"){
    data <- data %>% dplyr::group_by(fecha=fecha_ingreso,add = TRUE)
  }
  if(date=="month"){
    data <- data %>% dplyr::group_by(fecha=format(as.Date(fecha_ingreso),"%Y-%m-01"),add=TRUE)
  }
  if(date=="year"){
    data <- data %>% dplyr::group_by(fecha=format(as.Date(fecha_ingreso),"%Y-01-01"),add=TRUE)
  }
  if(!is.null(diag)){
    if (diag=="diag1"){
      if (!("diag1" %in% colnames(data))){
        data <- data %>% AddDiagnosis1()
      }
      data <- data %>% dplyr::group_by(diag1=diag1,add=TRUE)
    }
    if (diag=="diag2"){
      if (!("diag2" %in% colnames(data))){
        data <- data %>% AddDiagnosis2()
      }
      data <- data %>% dplyr::group_by(diag2=diag2,add=TRUE)
    }
    if (diag=="diag3"){
      if (!("diag3" %in% colnames(data))){
        data <- data %>% AddDiagnosis3()
      }
      data <- data %>% dplyr::group_by(diag3=diag3,add=TRUE)
    }
    if (!(diag %in% c("diag1","diag2","diag3"))){
      stop("We only know about diag1, diag2, diag3 reduction")
    }
  }
  if (sex){
    data <- data %>% dplyr::group_by(sex=sexo,add=TRUE)
  }
  data <- data %>% dplyr::summarise(total=n())
  return(data)
}

#' @title Calculate prevalence, relative values.
#' @description Calculate prevalence, relative values by total population of preovince
#' @param data Morbidity data
#' @param pop total, male, females
#' @return relative values of prevalence
#' @details Uses poblacion
#' @examples
#' data <- data_ejemplo %>%  ReduceData(provincia = TRUE,date="year") %>% SetPrevalence()

SetPrevalence <- function(data,pop="total"){
  data <- data %>% dplyr::filter(lubridate::year(fecha)>=2000)
  if (nrow(data)==0){
    stop("De momento esta funcion no soporta años anteriores a 2000")
  }
  if(!("total" %in% colnames(data))){
    stop("Prevalences is only for accumulated values")
  }
  data$total.prev <- NA
  if (!(pop %in% c("total","males","females"))){
    stop("You have to choose, total, males or females")
  }
  provs <- unique(data$prov)
  years <- unique(lubridate::year(as.Date(data$fecha)))
  for (p in provs){
    for (y in years){
      prov <- provincias[provincias$Codigo==p,]$nombre
      pob <- poblacion %>% dplyr::filter(tipo==pop) %>% dplyr::filter(year==y) %>% dplyr::filter(provincia==prov)
      pob <- as.numeric(pob$pob)
      data[data$prov==p & lubridate::year(data$fecha)==y,]$total.prev <- round(data[data$prov==p & lubridate::year(as.Date(data$fecha))==y,]$total / (pob / 100000), 4)
    }
  }
  return(data)
}
