# MorbiditySpainR

*ATENCIÓN* Recientemente se han añadido datos de los años 2015 y 2016 con formatos distintos, todavía no se actualizado el paquete para trabajar con estos datos.

[![Build Status](https://api.travis-ci.org/rOpenSpain/MorbiditySpainR.svg?branch=master)](https://travis-ci.org/rOpenSpain/MorbiditySpainR)

R package to read, parse and do basic manipulation of INE Morbidity microdata [Morbilidad Hospitalaria Microdatos INE](http://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176778&menu=resultados&secc=1254736195291&idp=1254735573175). 
The metadata of the microdata is documented [here](http://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176778&menu=resultados&secc=1254736195291&idp=1254735573175).

This packages uses international classification of diseases documented [here](https://eciemaps.msssi.gob.es/ecieMaps/browser/index_9_mc.html)

## Installation

```
library(devtools)
install_github("rOpenSpain/MorbiditySpainR")
```

## Downloading and reading data

The function `GetMorbiData` recives the years to read morbidity data, downloads the files from INE's ftp server and parses them.


```
data <- GetMorbiData(y1=2010,y2=2011)
head(data)
``` 

## Filtering data

The function `FilterProvincia` recives the id of the _provincia_ (regional administration) to filter data.


```
data <- data <- data_ejemplo %>% FilterProvincia(28)
head(data)
``` 

The function `FilterEmergency` recives a boolean (defect TRUE) to filter data by wether or not is  an ER item.

```
data <- data_ejemplo %>% FilterEmergency()
head(data)
``` 

The function `FilterDiagnosis` recives a integer (id of diagnosis) to filter data by principal diagnosis.

```
data <- data_ejemplo %>% FilterDiagnosis1(2)
head(data)
``` 

The function `FilterDiagnosis2` recives a integer (id of diagnosis) to filter data by secondary diagnosis.

```
data <- data_ejemplo %>% FilterDiagnosis2(20)
head(data)
``` 

## Manipulating data

The function `AddDiagnosis1` add column daig1 with principal diagnosis.

```
data <- data_ejemplo %>% AddDiagnosis1()
head(data)
``` 

The function `AddDiagnosis2` add column daig2 with secondary diagnosis.

```
data <- data_ejemplo %>% AddDiagnosis2()
head(data)
``` 

The function `AddDiagnosis3` add column daig3 with specific diagnosis.

```
data <- data_ejemplo %>% AddDiagnosis3()
head(data)
``` 

The function `ReduceData` does different grouping manipulations by _provincia_, date, diagnosis or sex.

```
data <- data_ejemplo %>% ReduceData(provincia = TRUE,date = "day")
head(data)
```

The function `SetPrevalence` gets relative values from grouped values and population (total or by sex) of provinces.

```
data <- data_ejemplo %>%  ReduceData(provincia = TRUE,date="year") %>% SetPrevalence()
head(data)
```
