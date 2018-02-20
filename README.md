# MorbiditySpainR

[![Build Status](https://api.travis-ci.org/rafaelmenmell/MorbiditySpainR.svg?branch=master)](https://travis-ci.org/rafaelmenmell/MorbiditySpainR)

R package to read, parse and do basic manipation of INE Morbidity microdata [Morbilidad Hospitalaria Microdatos INE](http://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176778&menu=resultados&secc=1254736195291&idp=1254735573175). 
The metadata of the microdata is documented [here](http://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176778&menu=resultados&secc=1254736195291&idp=1254735573175).

This packages uses international classification of diseases documented [here](https://eciemaps.msssi.gob.es/ecieMaps/browser/index_9_mc.html)

## Installation

```
library(devtools)
install_github("rafaelmenmell/MorbiditySpainR")
```

## Downloading and reading data

The function `GetMorbiData` recives the years to read morbidity data, downloads the files from INE's ftp server and parses them.


```
data <- MorbiditySpain::GetMorbiData()
head(data)
``` 

## Filtering data

The function `FilterProvincia` recives the id of the _provincia_ (regional administration) to filter data.


```
data <- MorbiditySpain::FilterProvincia(provincia = 28)
head(data)
``` 

The function `FilterEmergency` recives a boolean (defect TRUE) ti filter data by wether or not is  an ER item.

```
data <- MorbiditySpain::FilterEmergency()
head(data)
``` 

## Manipulating data

The function `ReduceData` does different grouping manipulations by _provincia_, date, diagnosis or sex.

```
data <- MorbiditySpain::ReduceData()
head(data)
```

The function `SetPrevalence` gets relative values from grouped values and population (total or by sex) of provinces.

```
data <-MorbiditySpain::SetPrevalence(pop = "total")
head(data)
```
