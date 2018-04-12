## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
set.seed(3)

## ----Librerías utilizadas, include=FALSE---------------------------------
##LIBRERIAS PARA MAC
library("caret", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("AppliedPredictiveModeling",lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library") 
library("e1071", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library") 
library("leaps", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("glmnet", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

##LIBRERIAS PARA WINDOWS
#library("AppliedPredictiveModeling", lib.loc="~/R/win-library/3.3")
#library("caret", lib.loc="~/R/win-library/3.3")
#library("e1071", lib.loc="~/R/win-library/3.3")
#library("glmnet", lib.loc="~/R/win-library/3.3")
#library("leaps", lib.loc="~/R/win-library/3.3")

## ----Lectura del data set: SAHeart---------------------------------------
set.seed(3)

# Leemos los datos
SAHeart = read.csv("./datos/SAHeart.csv")

# Eliminamos la primera columna (un identificador de la tupla)
# Además, dado que una de las caracteristicas (famhist) tiene 
# un dominio binario cualitativo, vamos a cambiarlas por 0 y 1.
SAHeart = SAHeart[,!colnames(SAHeart)=="row.names"]
SAHeart = data.frame(famHist = (ifelse(SAHeart$famhist=="Absent",0,1)),SAHeart)
SAHeart = SAHeart[,!colnames(SAHeart)=="famhist"]
SAHeartLabels = SAHeart$chd


## ----Diagramas de Dispersion: SAHeart------------------------------------

# Ejecucion de pairs para obtener los diagramas de dispersion
# Se enfrenta cada atributo con todos los demas
pairs(~ sbp + tobacco +adiposity + obesity + ldl + typea + alcohol + age, data = SAHeart)


## ----Creacion de particiones---------------------------------------------

# Utilizamos el 80% de las muestras del conjunto de datos
# para crear el conjunto Train y las restantes para el Test
porcentajeMuestrasTrain = 0.8
train = sample(nrow(SAHeart), round(nrow(SAHeart)*porcentajeMuestrasTrain))
SAHeart.Train = SAHeart[train,]
SAHeart.Test = SAHeart[-train,]

# Guardamos las etiquetas del train y test en estructuras aparte
SAHeartLabels.Train = SAHeartLabels[train]
SAHeartLabels.Test = SAHeartLabels[-train]


## ----Prueba 1 BoxCoxTrans: asimetria-------------------------------------
# Aplicacion de la funcion BoxCox a un atributo elegido tras realizar
# distintas pruebas y al que no se le aplica transformacion
par(mfrow=c(2,2)) 
hist(SAHeart.Train$tobacco, main = "Antes de BoxCoxTrans")
tobacco_trans = BoxCoxTrans(SAHeart.Train$tobacco)
tobacco_trans
hist(predict(tobacco_trans,SAHeart.Train$tobacco),main = "Despues de BoxCox Trans")

hist(SAHeart.Train$sbp,main = "Antes de BoxCoxTrans")
sbp_trans = BoxCoxTrans(SAHeart.Train$sbp)
sbp_trans
hist(predict(sbp_trans,SAHeart.Train$sbp),main = "Despues de BoxCoxTrans" )


## ----Prueba 1 PCA: reduccion de dimensionalidad, escalado y normalizacion----
# Ejecucion del algoritmo PCA aplicando escalado y normalizacion.
pcaObject = prcomp(SAHeart.Train[,!colnames(SAHeart.Train)=="chd"], center = TRUE,
                   scale = TRUE)
attributes(pcaObject)

# A continuacion obtenemos el porcentaje con el que cada uno de los
# atributos contribuye a la varianza total.
porcentVariance = pcaObject$sdev^2/sum(pcaObject$sdev^2)*100
porcentVariance

# Representacion de los porcentajes de las varianzas
par(mfrow=c(1,2)) 
plot(cumsum(porcentVariance),main = "Suma acumulada de varianzas")
plot(pcaObject,type="l",main = "Porcentaje de varianza por atrib.")


## ----Preprocesado de los datos-------------------------------------------

# El valor thres indica el porcentaje de variabilidad de los datos que tendran que 
# explicar las componentes elegidas por el algoritmo, en nuestro caso usaremos thres=0.86
thres = 0.86
objetoTrans = preProcess(SAHeart.Train[,!colnames(SAHeart.Train)=="chd"],
                         method = c("BoxCox", "center", "scale", "pca"), thres)
SAHeart.Train2 = predict(objetoTrans, SAHeart.Train)

# Eliminamos las variables con varianza 0 o muy proximas.
nearZeroVar(SAHeart.Train2)

## ----Evaluacion previa del conjunto de datos-----------------------------

# Para simplificar y prescindir del prefijo SAHeart
attach(SAHeart.Train2)
# Matriz de rotacion.
pcaObject$rotation


## ----Preprocesado del dataset original-----------------------------------
# Preprocesado automatico
objetoTrans = preProcess(SAHeart.Train[,!colnames(SAHeart.Train)=="chd"],
                         method = c("BoxCox", "center", "scale"))
SAHeart.TrainProcessed = predict(objetoTrans, SAHeart.Train)

# Comprobamos que no exista ninguna componente con varianza 0
nearZeroVar(SAHeart.TrainProcessed)


## ----obtencion del lambda para validacion cruzada------------------------
# fijamos la semilla aleatoria
set.seed(14)

# Creacion de conjuntos
matrizX = model.matrix(chd ~ ., SAHeart.Train)
vectorY = SAHeart.Train$chd

# Obtenemos el mejor lambda mediante validacion cruzada
# para el cual el error es minimo sobre los datos.
# utilizamos un alpha = 0 para indicar que queremos usar Weight Decay.
cv.out = cv.glmnet(matrizX, vectorY, alpha = 0)
# calculamos el mejor lambda.
bestlam =cv.out$lambda.min
print(sprintf("Mejor valor lambda obtenido: %f", bestlam))



## ----Necesidad de aplicar Regularizacion---------------------------------

# El primer paso consiste en ejecutar glmnet con lambda = 0 para obtener la desviacion
# sin considerar regularizacion alguna
desv_GLMnet_NoReg = glmnet(matrizX, vectorY, alpha = 0, lambda = 0)
print(sprintf("Desviacion sin considerar regularizacion: %f ",desv_GLMnet_NoReg$dev.ratio))

# Comprobamos como varia el valor de desviacion si se considera regularizacion
desv_GLMnet_REG= glmnet(matrizX, vectorY, alpha = 0, lambda = bestlam)
print(sprintf("Desviacion considerando regularizacion: %f ",desv_GLMnet_REG$dev.ratio))




## ----Seleccion de modelos con Regsubsets---------------------------------

# Nuestra variable de respuesta (clase) son las etiquetas que indican
# si una persona padeceria o no una cardiopatia coronaria
pruebaRegSubsets1 = regsubsets(chd ~ . , data=SAHeart.TrainProcessed, 
                               method = "exhaustive")
summary(pruebaRegSubsets1)

## ----preProcesado de los datos-------------------------------------------

# no consideramos la variable respuesta: chd
objetoTrans_Test = preProcess(SAHeart.Test[,!colnames(SAHeart.Test)=="chd"],
                         method = c("BoxCox", "center", "scale"))
SAHeart.TestProcessed = predict(objetoTrans_Test, SAHeart.Test)


## ----Modelos de Regresión Lineal-----------------------------------------

# Nuestra variable de respuesta seran las etiquetas del Train
# Aprendemos el modelo Lineal con el subconjunto de tamaño 1 = age
modeloLineal1 = lm(SAHeartLabels.Train ~ age, data = SAHeart.TrainProcessed)

# Subconjunto de tamaño 2 = age + famHist
modeloLineal2 = lm(SAHeartLabels.Train ~ age + famHist, data = SAHeart.TrainProcessed)

# Subconjunto de tamaño 3 = age + famHist + ldl
modeloLineal3 = lm(SAHeartLabels.Train ~ age + famHist + ldl, data = SAHeart.TrainProcessed)

# Subconjunto de tamaño 5 
modeloLineal4 = lm(SAHeartLabels.Train ~ age + famHist + ldl + tobacco + typea,
                   data = SAHeart.TrainProcessed)


## ----Funcion para evaluar un modelo--------------------------------------
evaluacionModelo = function(modeloTrain, datosTest, etiquetasTest, indiceModelo)
{
  # Calculamos la probabilidad del modelo lineal
  probTest = predict(modeloTrain, data.frame(datosTest), type="response")
  # Calculamos la prediccion con el modelo lineal: inicializadas a 0
  predTest = rep(0, length(probTest))
  #Calculo de las predicciones reales
  predTest[probTest >=0.5] = 1 # >= 0.5 clase 1
  # Matriz de confusion
  print(sprintf("Matriz de confusion del Modelo %i", indiceModelo))
  print(table(predTest, etiquetasTest))
  # Calculo del Eout con el modelo lineal
  Eval = mean(predTest != etiquetasTest)
  print(sprintf("Evaluacion (Porcentaje de Error) del ML-%i -> %f",indiceModelo, Eval*100))
  cat("----------------------------------- \n")
}

## ----Evaluacion de los modelos-------------------------------------------

#Procedemos a evaluar los 4 modelos lineales aprendidos
evaluacionModelo(modeloLineal1, SAHeart.TestProcessed, SAHeartLabels.Test, 1)
evaluacionModelo(modeloLineal2, SAHeart.TestProcessed, SAHeartLabels.Test,2)
evaluacionModelo(modeloLineal3, SAHeart.TestProcessed, SAHeartLabels.Test,3)
evaluacionModelo(modeloLineal4, SAHeart.TestProcessed, SAHeartLabels.Test,4)


## ----Modelos de Regresion Logistica--------------------------------------
# Variable de respuesta = etiquetas del Train
# Aprendemos el modelo de Regresion Logistica con el subconjunto de tamaño 1 = age
# y una familia de funciones binomiales
modeloRegLog1 = glm(SAHeartLabels.Train ~ age, family = binomial(logit) ,
                    data = SAHeart.TrainProcessed)

# Subconjunto de tamaño 2 = age + famHist
modeloRegLog2 = glm(SAHeartLabels.Train ~ age + famHist, family = binomial(logit) ,
                    data = SAHeart.TrainProcessed)

# Subconjunto de tamaño 3 = age + famHist + ldl
modeloRegLog3 = glm(SAHeartLabels.Train ~ age + famHist + ldl,family = binomial(logit) ,
                    data = SAHeart.TrainProcessed)

# Subconjunto de tamaño 5 
modeloRegLog4 = glm(SAHeartLabels.Train ~ age + famHist + ldl + tobacco + typea,
                    family = binomial(logit) ,data = SAHeart.TrainProcessed)

## ----Evaluacion de los modelos de regresion logistica--------------------

#Procedemos a evaluar los 4 modelos de regresion logistica aprendidos
evaluacionModelo(modeloRegLog1, SAHeart.TestProcessed, SAHeartLabels.Test, 1)
evaluacionModelo(modeloRegLog2, SAHeart.TestProcessed, SAHeartLabels.Test,2)
evaluacionModelo(modeloRegLog3, SAHeart.TestProcessed, SAHeartLabels.Test,3)
evaluacionModelo(modeloRegLog4, SAHeart.TestProcessed, SAHeartLabels.Test,4)


## ----Modelos de Regresión Lineal trans-----------------------------------

# Subconjunto de tamaño 1
modeloLinealTrans1 = lm(SAHeartLabels.Train ~ I(age^2) , data = SAHeart.TrainProcessed)

# Subconjunto de tamaño 3
modeloLinealTrans2 = lm(SAHeartLabels.Train ~ age + famHist + ldl + I(age^3) + I(ldl^3) 
                        + (I(age^3)*I(ldl^2)), data = SAHeart.TrainProcessed)

# Subconjunto de tamaño 5
modeloLinealTrans3 = lm(SAHeartLabels.Train ~ age + famHist + ldl + tobacco + typea  + 
                          I(age^3) + I(typea^3), data = SAHeart.TrainProcessed)

# Subconjunto de tamaño 4
modeloLinealTrans4 = lm(SAHeartLabels.Train ~ I(age^2) + I(famHist^2) + I(ldl^2) + 
                          I(tobacco^2), data = SAHeart.TrainProcessed)


## ----Calculo de probabilidades con Regresión Lineal trans----------------

#Procedemos a evaluar los 4 modelos de regresion logistica aprendidos
evaluacionModelo(modeloLinealTrans1, SAHeart.TestProcessed, SAHeartLabels.Test, 1)
evaluacionModelo(modeloLinealTrans2, SAHeart.TestProcessed, SAHeartLabels.Test,2)
evaluacionModelo(modeloLinealTrans3, SAHeart.TestProcessed, SAHeartLabels.Test,3)
evaluacionModelo(modeloLinealTrans4, SAHeart.TestProcessed, SAHeartLabels.Test,4)


## ----Ajuste del Eout en clasificacion------------------------------------
ajusteEoutClasificacion = function(dataset, datasetLabels)
{
  # Utilizamos el 80% de las muestras del conjunto de datos
  # para crear el conjunto Train y las restantes para el Test
  porcentajeMuestrasTrain = 0.8
  train = sample(nrow(dataset), round(nrow(dataset)*porcentajeMuestrasTrain))
  dataset.Train = dataset[train,]
  dataset.Test = dataset[-train,]
  
  # Guardamos las etiquetas del train y test en estructuras aparte
  datasetLabels.Train = datasetLabels[train]
  datasetLabels.Test = datasetLabels[-train]
  
  objetoTrans = preProcess(dataset.Train[,!colnames(dataset.Train)=="chd"],
                           method = c("BoxCox", "center", "scale"))
  dataset.TrainProcessed = predict(objetoTrans, dataset.Train)
  dataset.TestProcessed = predict(objetoTrans, dataset.Test)
  
  # Aprendemos el mejor modelo de regresion lineal
  modeloLineal = lm(datasetLabels.Train ~ age + famHist + ldl + tobacco + 
                      typea, data = dataset.TrainProcessed)
  
  # Calculamos la probabilidad del modelo lineal
  probTest = predict(modeloLineal, data.frame(dataset.TestProcessed), type="response")
  # Calculamos la prediccion con el modelo lineal: inicializadas a 0
  predTest = rep(0, length(probTest))
  #Calculo de las predicciones reales
  predTest[probTest >=0.5] = 1 # >= 0.5 clase 1
  # Calculo del Eout con el modelo lineal
  Eval = mean(predTest != datasetLabels.Test)
  
}

# Llamamos a la funcion para calcular el Eout medio con 100 iteraciones
errorMedio = mean(replicate(100,ajusteEoutClasificacion(SAHeart, SAHeartLabels)))*100
print(sprintf("El error medio obtenido con 100 iteraciones es: %f", errorMedio))


## ----Lectura del data set: LAozone---------------------------------------
# Leemos los datos
LAozone = read.csv("./datos/LAozone.csv")

# transformamos el dataset en un dataframe
LAozone = data.frame(LAozone)

# Eliminamos el atributo day of year (doy) ya que no
# aporta ninguna informacion util
LAozone = LAozone[,!colnames(LAozone) == "doy"]

## ----Creacion de particiones: LAozone------------------------------------
set.seed(14)
# Utilizamos el 80% de las muestras del conjunto de datos
# para crear el conjunto Train y las restantes para el Test
porcentajeMuestrasTrain = 0.8
train = sample(nrow(LAozone), round(nrow(LAozone)*porcentajeMuestrasTrain))
LAozone.Train = LAozone[train,]
LAozone.Test = LAozone[-train,]

## ----Preprocesado de los datos con PCA: LAozone--------------------------
# Preproceado de los datos con un thres del 0.85
objetoTransOzone = preProcess(LAozone.Train[,!colnames(LAozone.Train)=="ozone"],
                method = c("BoxCox", "center", "scale","pca"),thres=0.95)

# Matriz de rotacion
objetoTransOzone$rotation

## ----Preprocesado de los datos sin PCA: LAozone--------------------------

# calculamos las transformaciones necesarias en objetoTransOzone
objetoTransOzone = preProcess(LAozone.Train[,!colnames(LAozone.Train)=="ozone"],
                              method = c("BoxCox", "center", "scale"))

# realizamos las tranformaciones calculadas con predict
Laozone.TrainProcessed = predict(objetoTransOzone, LAozone.Train)

# Mostramos las transformaciones que se han aplicado con el preProcess 
# y sobre que atributos se han realizado
objetoTransOzone$method

## ----Obtencion del mejor lambda (con cross-validation)-------------------
set.seed(14)

# Creacion de conjuntos
matrizX = model.matrix(ozone ~ ., LAozone.Train)
vectorY = LAozone.Train$ozone

# Obtenemos el mejor lambda mediante validacion cruzada
cv.out = cv.glmnet(matrizX, vectorY, alpha = 0)
# calculamos el mejor lambda.
bestlam =cv.out$lambda.min
print(sprintf("Mejor valor lambda obtenido: %f", bestlam))

## ----Necesidad de aplicar Regularizacion: LAozone------------------------

# El primer paso consiste en ejecutar glmnet con lambda = 0 para obtener la desviacion
# sin considerar regularizacion alguna
desv_GLMnet_NoReg = glmnet(matrizX, vectorY, alpha = 0, lambda = 0)
print(sprintf("Desviacion sin considerar regularizacion: %f ",
              desv_GLMnet_NoReg$dev.ratio))

# Comprobamos como varia el valor de desviacion si se considera regularizacion
desv_GLMnet_REG= glmnet(matrizX, vectorY, alpha = 0, lambda = bestlam)
print(sprintf("Desviacion considerando regularizacion: %f ",
              desv_GLMnet_REG$dev.ratio))


## ----Seleccion de modelos con Regsubsets: LAozone------------------------
# Nuestra variable de respuesta son lo valores que
# indican los niveles de ozono en la atmosfera.
regSubsetsLA = regsubsets(ozone ~ . , data=Laozone.TrainProcessed, method = "exhaustive")
summary(regSubsetsLA)

## ----preProcesado de los datos Test: LAozone-----------------------------
# No realizamos el preprocesado sobre la variable respuesta
objetoTrans_TestLA = preProcess(LAozone.Test[,!colnames(LAozone.Test)=="ozone"],
                        method = c("BoxCox", "center", "scale"))
#objetoTrans_TestLA = preProcess(LAozone.Test,
#                        method = c("BoxCox", "center", "scale"))
LAozone.TestProcessed = predict(objetoTrans_TestLA, LAozone.Test)


## ----Aprendizaje de modelos de regresion lineal - Exp. 1-----------------
# Nuestra variable de respuesta sera el atributo ozone.
modeloLineal1 = lm(ozone ~ temp, data = Laozone.TrainProcessed)
modeloLineal2 = lm(ozone ~ humidity + ibt, data = Laozone.TrainProcessed)
modeloLineal3 = lm(ozone ~ humidity + temp + ibh, data = Laozone.TrainProcessed)
modeloLineal4 = lm(ozone ~ humidity + temp + vh + ibt, data = Laozone.TrainProcessed)
modeloLineal5 = lm(ozone ~ humidity + temp + vh + ibt + vis, data = Laozone.TrainProcessed)

## ----Funcion para evaluar un modelo: LAozone-----------------------------
evaluacionModelo = function(modeloTrain, datosTest, varRespuesta, indiceModelo)
{
  # Calculamos la probabilidad del modelo lineal
  probTest = predict(modeloTrain, data.frame(datosTest), type="response")
  # Calculo del Eout con el modelo lineal
  Eval = mean((probTest - varRespuesta)^2)
  print(sprintf("Evaluacion (diferencia de minimos cuadrados) del ML-%i -> %f",indiceModelo, Eval))
  cat("----------------------------------- \n")
}

## ----Evaluacion del primer modelo----------------------------------------
# evaluacion del modelo 1
evaluacionModelo(modeloLineal1, LAozone.TestProcessed, LAozone.Test$ozone, 1)

# Representamos el ajuste del modelo 1 frente a los datos del test
plot(LAozone.TestProcessed$temp,LAozone.TestProcessed$ozone, main = "Ozono vs Temp")
abline(modeloLineal1$coefficients, col=2 )


## ----Evaluacion de los demas modelos-------------------------------------

#Procedemos a evaluar los otros 4 modelos lineales aprendidos
evaluacionModelo(modeloLineal2, LAozone.TestProcessed, LAozone.Test$ozone,2)
evaluacionModelo(modeloLineal3, LAozone.TestProcessed, LAozone.Test$ozone,3)
evaluacionModelo(modeloLineal4, LAozone.TestProcessed, LAozone.Test$ozone,4)
evaluacionModelo(modeloLineal5, LAozone.TestProcessed, LAozone.Test$ozone,5)


## ----Transformaciones no lineales y evaluaciones-------------------------

# modelo con transformacion no lineal 1
modeloLinealMejora1 = lm(ozone ~ humidity * ibh * temp, data = Laozone.TrainProcessed)
evaluacionModelo(modeloLinealMejora1, LAozone.TestProcessed, LAozone.Test$ozone,1)

# modelo con transformacion no lineal 2
modeloLinealMejora2 =  lm(ozone ~  humidity * temp * ibh * ibt,
                          data = Laozone.TrainProcessed)
evaluacionModelo(modeloLinealMejora2, LAozone.TestProcessed, LAozone.Test$ozone,2)

# modelo con transformacion no lineal 3
modeloLinealMejora3 = lm(ozone ~ humidity * temp * ibh * ibt * vis,
                         data = Laozone.TrainProcessed)
evaluacionModelo(modeloLinealMejora3, LAozone.TestProcessed, LAozone.Test$ozone,3)

# modelo con transformacion no lineal 4
modeloLinealMejora4 = lm(ozone ~ humidity * temp * ibh * I(ibt^2) + I(vis^2),
                         data = Laozone.TrainProcessed)
evaluacionModelo(modeloLinealMejora4, LAozone.TestProcessed, LAozone.Test$ozone,4)

# modelo con transformacion no lineal 5
modeloLinealMejora5 = lm(ozone ~ humidity * temp * ibh + I(ibt^2) + I(vis^2),
                         data = Laozone.TrainProcessed)
evaluacionModelo(modeloLinealMejora5, LAozone.TestProcessed, LAozone.Test$ozone,5)


## ----Media de errores normalizados en intervalo--------------------------
errorNormalizadoIntervalo = function(modeloTrain, datosTest, varRespuesta, indiceModelo, salidaPantalla = TRUE)
{
  # Calculamos la probabilidad del modelo lineal
  probTest = predict(modeloTrain, data.frame(datosTest), type="response")
  # Calculo del Eout con el modelo lineal
  numerador = abs(probTest - varRespuesta)
  denominador = max(varRespuesta) - min(varRespuesta)
  error = numerador/denominador
  error = mean(error)
  error = error*100
  if(salidaPantalla)
  {
    print(sprintf("Eout (media de errores normalizados) del ML-%i -> %f",
                  indiceModelo, error))
  }
  else
  {
    error
  }

}


## ----Evaluacion de los mejores modelos-----------------------------------

# Aplicacion de la metrica explicada anteriormente para explicar nuestro mejor modelo
errorNormalizadoIntervalo(modeloLinealMejora1, LAozone.TestProcessed, LAozone.Test$ozone,1,T)
errorNormalizadoIntervalo(modeloLinealMejora2, LAozone.TestProcessed, LAozone.Test$ozone,2,T)
errorNormalizadoIntervalo(modeloLinealMejora3, LAozone.TestProcessed, LAozone.Test$ozone,3,T)
errorNormalizadoIntervalo(modeloLinealMejora4, LAozone.TestProcessed, LAozone.Test$ozone,4,T)
errorNormalizadoIntervalo(modeloLinealMejora5, LAozone.TestProcessed, LAozone.Test$ozone,5,T)


## ----Ajuste del Eout-----------------------------------------------------
set.seed(14)

ajusteEoutRegresion = function(dataset)
{
  # Utilizamos el 80% de las muestras del conjunto de datos
  # para crear el conjunto Train y las restantes para el Test
  porcentajeMuestrasTrain = 0.8
  train = sample(nrow(dataset), round(nrow(dataset)*porcentajeMuestrasTrain))
  dataset.Train = dataset[train,]
  dataset.Test = dataset[-train,]
  
  # No consideramos el atributo de historial familiar (famHist)
  # al ser una variable binaria
  objetoTrans = preProcess(dataset.Train[,!colnames(dataset.Train)=="ozone"],
                           method = c("BoxCox", "center", "scale"))
  dataset.TrainProcessed = predict(objetoTrans, dataset.Train)
  dataset.TestProcessed = predict(objetoTrans, dataset.Test)
  
  # Aprendemos el mejor modelo de regresion lineal
  modeloLineal = lm(ozone ~ humidity * temp * ibh + I(ibt^2) + I(vis^2), data = Laozone.TrainProcessed)
  
  # Calculamos la probabilidad del modelo lineal
  probTest = predict(modeloLineal, data.frame(dataset.TestProcessed), type="response")
  # Calculo del Eout con el modelo lineal
  errorNormalizadoIntervalo(modeloLineal, dataset.TestProcessed, dataset.TestProcessed$ozone, 1,F)
}

# Llamamos a la funcion para calcular el Eout medio con 100 iteraciones
errorMedio = mean(replicate(100,ajusteEoutRegresion(LAozone)))

print(sprintf("El error medio obtenido con 100 iteraciones es: %f", errorMedio))


