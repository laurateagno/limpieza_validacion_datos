---
title: "Limpieza y validación de datos"
author: "Laura Teagno"
date: '`r format(Sys.Date(),"%e de %B, %Y")`'
output:
  html_document:
    toc: yes
  pdf_document:
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r load_libraries, include=FALSE}
library(knitr)
library(eeptools)
library(VIM)
library(scales)
library(outliers)
library(olsrr)
```

## 0. Carga del fichero
Cargar el fichero `viviendas.csv` en R. Antes de cargar el fichero, se debe inspeccionar de qué tipo de formato csv se trata para que la lectura sea apropiada. Explicar cómo se ha cargado el fichero.

Se ha inspeccionado el formato de los números del archivo .csv y al ver que los separadores de valores y decimal corresponder al formato español, se ha decidido utilizar la función `read.csv2`.

```{r read}
# carga de fichero
Inmuebles <- read.csv2("/Users/laurateagno/Desktop/viviendas.csv", header=TRUE, sep=";", na.strings="NA", dec=",", strip.white=TRUE)

```

Se estudia la estructura del dataset: número de registros y variables. 

```{r check}
# chequeo del fichero cargado
str(Inmuebles)

```

Importante: la carga de datos no es del todo correcta, debido a que el es sistema operativo es un MacOs. Se ha intentando cargar con en Encoding `UTF-8`, pero no se ha tenido éxito.


## 1. Descripción del dataset
¿Por qué es importante y qué pregunta/problema pretende responder?
En el dataset “Viviendas.csv” se recoge información sobre las primas de seguro de hogares, características de los clientes y de las viviendas. Se quiere estudiar el precio del seguro y la tipología de clientes.

## 2. Limpieza de los datos
### 2a. Selección de los datos de interés a analizar
¿Cuáles son los campos más relevantes para responder al problema? 
El dataset contiene 17 variables. Las más significativas para responder al problema son: tipo de vivienda, prima, superficie, siniestros por vivienda, valor de la vivienda.

### 2b. Estudio de los datos
Estos campos en concreto no tienen ceros o elementos vacíos, pero en el caso de que tuvieran, haría falta decidir un criterio para el análisis. En el caso de que sean pocos respecto al conjunto total, estos se pueden excluir. Eventualmente se les puede asignar la media, así que en los análisis estadísticos no penalizarían los resultados.

Se buscan los elementos vacíos:
```{r missings}
# buscar los valores missings
missing <- which(is.na(Inmuebles), arr.ind=TRUE)
missing

Inmuebles_n <- kNN(Inmuebles, variable = colnames(Inmuebles), k = 6)

```

Se estudian las variables significativas para averiguar si contienen valores extremos. En caso afirmativo, hay que comprobar que no se trate de un error en la introducción de los datos. Una vez comprobado que no lo es, conviene averiguar la causa de este comportamiento anómalo. Los datos extremos afectan mucho al valor de la media y si hay pocos datos, este efecto es todavía más importante.

Se buscar valores atípicos en las variables cuantitativas: prima, superficie, siniestros, valor.

```{r graphics, fig.height=4, fig.width=6}

# boxplot de la variable prima
boxplot(Inmuebles$prima, main="Boxplot de la variable Prima",ylab="Prima (€)")

lab <- c("Media", "Standard Deviation")
est <- c(mean(Inmuebles$prima), sd(Inmuebles$prima))
estlab <- rbind(lab, est)
estlab
boxplot.stats(Inmuebles$prima)$out 

# boxplot de la variable superficie
boxplot(Inmuebles$superficie, main="Boxplot de la variable Superficie",ylab="Prima (m^2)")

est <- c(mean(Inmuebles$superficie), sd(Inmuebles$superficie))
estlab <- rbind(lab, est)
estlab
boxplot.stats(Inmuebles$superficie)$out 

# boxplot de la variable siniestros
boxplot(Inmuebles$siniestros, main="Boxplot de la variable Siniestros",ylab="Siniestros")

est <- c(mean(Inmuebles$siniestros), sd(Inmuebles$siniestros))
estlab <- rbind(lab, est)
estlab
boxplot.stats(Inmuebles$siniestros)$out 

# boxplot de la variable valor
boxplot(Inmuebles$valor, main="Boxplot de la variable Valor",ylab="Valor (€)")

est <- c(mean(Inmuebles$valor), sd(Inmuebles$valor))
estlab <- rbind(lab, est)
estlab
boxplot.stats(Inmuebles$valor)$out 
```

Los outliers hacen que el valor de la media sea mucho más alto respecto a la mediana. Si se eliminaran estos valores de la muestra, la media bajaría.

## 3. Análisis de los datos
### 3a. Selección de los grupos de datos que se quieren analizar/comparar
Se estudian las distribuciones de los datos más importantes para el análisis.

```{r histograma}
# histograma de la variable Valor con curva normal superpuesta
h <- hist(Inmuebles$valor, breaks = 80, density = 15,
          col = "lightgray", xlab = "€", main = "Histograma de la variable Valor") 
xfit <- seq(min(Inmuebles$valor), max(Inmuebles$valor), length = 40) 
yfit <- dnorm(xfit, mean = mean(Inmuebles$valor), sd = sd(Inmuebles$valor)) 
yfit <- yfit * diff(h$mids[1:2]) * length(Inmuebles$valor) 

lines(xfit, yfit, col = "black", lwd = 2)

```


### 3b. Comprobación de la normalidad y homogeneidad de la varianza 
Si es necesario (y posible), aplicar transformaciones que normalicen los datos.

```{r histograma estandarizado}
# histograma estandarizado de la variable Valor con curva normal superpuesta
Inmuebles_valor_estand <- rescale(Inmuebles$valor)

h <- hist(Inmuebles_valor_estand, breaks = 80, density = 15,
          col = "lightgray", main = "Histograma de la variable Valor estandarizada") 
xfit <- seq(min(Inmuebles_valor_estand), max(Inmuebles_valor_estand), length = 40) 
yfit <- dnorm(xfit, mean = mean(Inmuebles_valor_estand), sd = sd(Inmuebles_valor_estand)) 
yfit <- yfit * diff(h$mids[1:2]) * length(Inmuebles_valor_estand) 

lines(xfit, yfit, col = "black", lwd = 2)

```

Al estandarizar la variable *Valor*, se nota que la distribución sigue siendo la misma, sólo que ahora los valores son más "comparables".

### 3c. Aplicación de pruebas estadísticas
* ¿El valor de la prima es superior en caso de siniestro reportado?
El cliente que contrata el seguro cree que el valor de la prima es superior si anteriormente se ha comunicado un siniestro. ¿Podemos darle la razón al cliente con un 99% de confianza?

Distinguimos la muestra de observaciones en dos grupos:  
- grupo 1: uno o más siniestros  
- grupo 2: cero siniestros  

Verificamos ahora las siguientes hipótesis:  
- H~O~: $\mu$~1~ prima grupo 1 = prima grupo 2  
- H~1~: $\mu$~2~ prima grupo 1 > prima grupo 2  

No sabiendo si las dos muestras presentan distribución normales, y el tamaño de las dos muestras es *n* > 30, aplicamos el teorema del límite central, pudiendo así usar la distribución Z como estadístico de contraste.

```{r contraste de hipotesis primas}
# calcular estadísticos de las primas de los 2 grupos
prima_grupo1 <- Inmuebles$prima[which(Inmuebles$siniestros >= 1)]
prima_grupo2 <- Inmuebles$prima[which(Inmuebles$siniestros < 1)]
media_grupo1 <- mean(prima_grupo1)
media_grupo1
media_grupo2 <- mean(prima_grupo2)
media_grupo2
n_grupo1 <- length(prima_grupo1)
n_grupo2 <- length(prima_grupo2)
sd_grupo1 <- sd(prima_grupo1)*sqrt((n_grupo1-1)/(n_grupo1))
sd_grupo2 <- sd(prima_grupo2)*sqrt((n_grupo2-1)/(n_grupo2))

# calcular el estadístico de contraste
diff_medias <- media_grupo1 - media_grupo2
diff_s <- sd_grupo1 - sd_grupo2
z <- diff_medias / diff_s
z
p <- pnorm(z)
p

# comprobamos si aceptar o rechazar la hipótesis nula
confianza <- 0.01

if (p > confianza) {
   print("Aceptamos hipótesis nula")
} else {
    print("Rechazamos hipótesis nula")
}

```


* ¿La proporción de siniestros que se comunican es menor de un 45%?
Una inquietud de la compañía de seguros es la proporción de viviendas que reportan al menos un siniestro. Este cálculo es básico para poder calcular las primas y obtener los beneficios necesarios para la compañía. Se estima que la proporción de viviendas que comunican uno o varios siniestros es menor que 45%. ¿Esta estimación es correcta?

Antes de todo, se fija la hipótesis nula (H~O~) y la alternativa (H~1~):  
- H~O~: proporción de viviendas que comunican uno o más siniestros = 45%  
- H~1~: proporción de viviendas que comunican uno o más siniestros < 45%  

Además decidimos que el nivel de confianza es de (1-0.05)%.

```{r contraste de hipotesis siniestros}
# calcular cuántas viviendas comunican haber tenido uno o más siniestros
piso_siniestros <- Inmuebles$siniestros[which(Inmuebles$siniestros>=1)]
n_piso_siniestros <- length(piso_siniestros)
n_piso_siniestros

# calcular frecuencia observada
p_obs <- n_piso_siniestros / length(Inmuebles$siniestros)
p_obs

# calcular estadístico de contraste
p0 <- 0.45
Z <- (p_obs - p0) / sqrt((p0 * (1 - p0))/length(Inmuebles$siniestros))
Z

p <- pnorm(Z)
p

# comprobamos si aceptar o rechazar la hipótesis nula
confianza <- 0.05

if (p > confianza) {
   print("Aceptamos hipótesis nula")
} else {
    print("Rechazamos hipótesis nula")
}
```

Se busca en las tablas para distribución Z, la siguiente probabilidad:  
P(Z < -1.84774) = 1 - P(Z <= 1.84774) = 1 - 0.9678 < 0.05 --> Rechazamos la hipotesis nula H~O~.

* Modelo de regresión lineal múltiple (regresores cuantitativos y cualitativos):
Se estima por mínimos cuadrados ordinarios un modelo lineal que explique el precio del seguro (prima) en función de cuatro factores (superficie de la vivienda, su valor, valor de su contenido y tipo de vivienda).

Se mira la bondad del ajuste a través del coeficiente de determinación (R2).

```{r regression lineal multiple 4 variables}
tipo <- factor(Inmuebles$tipo)
tipo <- relevel(tipo, "PI")
regresion2 <- lm(prima ~ superficie + valor + contenido + tipo, data=Inmuebles)
summary(regresion2)

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(regresion2)

```

El coeficiente de correlación al cuadrado mide la bondad del ajuste de la recta a los datos. A partir de la salida anterior, vemos que su valor en este caso es Multiple R-squared: 0.7962.

* Selección de los inmuebles con más riesgo de siniestro:
Se establece un nivel de probabilidad (umbral de discriminación a partir del cual el inmueble tiene demasiado riesgo de siniestro según el modelo), por ejemplo, se puede elegir 30%. Comparar el nivel de riesgo que da el modelo con el número de siniestros observados.

```{r plot observado vs probabilidad}
acc = (Inmuebles$siniestros>= 1) * 1

modelo.logit2 <- glm (formula = acc ~ superficie + valor + contenido + edad + sexo, data=Inmuebles, family = "binomial")
summary(modelo.logit2)
prob_acc <- predict(modelo.logit2, Inmuebles, type = "response")

plot(Inmuebles$siniestro, prob_acc, xlab = "Siniestros observados", ylab = "Probabilidad asociada")
abline(h = 0.3, v = 0.5, col = "gray60")
rect(0, 0, 0.5, 0.3, col= rgb(0,0,1.0,alpha=0.5))

```

Sólo los inmuebles en los cuales no se han observado accidentes (Siniestros observados = 0) y su probabilidad de accidente es <30% (por debajo de la línea horizontal) se han comportado como esperado.


## 4. Representación de los resultados
Los resultados se representan a lo largo de los puntos anteriores.

## 5. Resolución del problema
A partir de los resultados obtenidos, ¿cuáles son las conclusiones? ¿Los resultados permiten responder al problema? Las conclusiones se plantean a lo largo de los puntos anteriores, según el caso que se estudia.

## 6. Exportación del fichero
```{r export}
# exportación del fichero inmuebles en formato csv.
write.csv(Inmuebles, file = "/Users/laurateagno/Desktop/Teagno_vivienda_clean.csv", row.names = FALSE, fileEncoding = "UTF-8")

```
