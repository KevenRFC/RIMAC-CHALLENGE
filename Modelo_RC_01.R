
DATA_INICIAL <- read.table(file="C:/Users/Keven/Desktop/RIMAC_CHALLENGE/DS_Generados/BD_ProcDatos_Parte_03.txt",header=TRUE,sep = ",")

dim(DATA_INICIAL)
str(DATA_INICIAL)

DATA_INICIAL$TARGET <- as.factor(DATA_INICIAL$TARGET)
summary(DATA_INICIAL$TARGET)

A = data.frame(colnames(DATA_INICIAL))


LLAVE = c("PERIODO",
         "CUC",
         "P_AUTO")

VARIABLES = c(
              "NSE_RIMAC",
             "NUM_PRODUCTOS",
             "SEGMENTO_INTERNO",
             "OK",
             "CPP",
             "DEFICIENTE",
             "DUDOSO",
             "PERDIDA",
             "DEUDA_HIINI",
             "SALDO_SBS",
             "SALDO_TC_SBS",
             "SALDO_VEH_SBS",
             "SALDO_HIP_SBS",
             "SALDO_PP_SBS",
             "SALDO_MICRO_SBS",
             "SALDO_PEQUENA_SBS",
             "SALDO_OTROS",
             "PRIMA_TOTAL",
             "PRIMA",
             "CANALDES",
             "CLASEVEHDES",
             #"MARCAVEHDES",
             "USOVEHDES",
             "ANOFABRIC",
             "SUMASEG",
             "NRO_SINIESTROS",
             "MTO_SINIESTROS",
             "Flag_Bancari",
             "SEXO",
             "UBICACION",
             "DEPARTAMENTO",
             "NUM_FACTURAS",
             "MONTO_DOLARES",
             "TIPCASO.RN_DESCRIPTOR_VEH_Atenciones",
             "TIPCASO.RN_DESCRIPTOR_VEH_Consulta",
             "TIPCASO.RN_DESCRIPTOR_VEH_Disconformidades",
             "TIPCASO.RN_DESCRIPTOR_VEH_Emergencia",
             "TIPCASO.RN_DESCRIPTOR_VEH_Fidelizacion",
             "TIPCASO.RN_DESCRIPTOR_VEH_Operacion",
             "TIPCASO.RN_DESCRIPTOR_VEH_Operaciones",
             "TIPCASO.RN_DESCRIPTOR_VEH_Reclamo",
             "TIPCASO.RN_DESCRIPTOR_VEH_Relacionamiento",
             "TIPCASO.RN_DESCRIPTOR_VEH_Requerimientos",
             "TIPCASO.RN_DESCRIPTOR_VEH_Solicitud",
             "TIPCASO.RN_DESCRIPTOR_VEH_Sugerencias",
             "TIPCASO.RN_DESCRIPTOR_noveh_Atenciones",
             "TIPCASO.RN_DESCRIPTOR_noveh_Consulta",
             "TIPCASO.RN_DESCRIPTOR_noveh_Disconformidades",
             "TIPCASO.RN_DESCRIPTOR_noveh_Emergencia",
             "TIPCASO.RN_DESCRIPTOR_noveh_Fidelizacion",
             "TIPCASO.RN_DESCRIPTOR_noveh_Operacion",
             "TIPCASO.RN_DESCRIPTOR_noveh_Operaciones",
             "TIPCASO.RN_DESCRIPTOR_noveh_Reclamo",
             "TIPCASO.RN_DESCRIPTOR_noveh_Relacionamiento",
             "TIPCASO.RN_DESCRIPTOR_noveh_Requerimientos",
             "TIPCASO.RN_DESCRIPTOR_noveh_Solicitud",
             "TIPCASO.RN_DESCRIPTOR_noveh_Sugerencias",
             "MONTO_DOLARES_01",
             "NUM_FACTURAS_01",
             "MONTO_DOLARES_02",
             "NUM_FACTURAS_02",
             "MONTO_DOLARES_03",
             "NUM_FACTURAS_03",
             "MONTO_DOLARES_04",
             "NUM_FACTURAS_04",
             "MONTO_DOLARES_05",
             "NUM_FACTURAS_05",
             "MONTO_DOLARES_06",
             "NUM_FACTURAS_06",
             "MONTO_DOLARES_07",
             "NUM_FACTURAS_07",
             "MONTO_DOLARES_08",
             "NUM_FACTURAS_08",
             "MONTO_DOLARES_09",
             "NUM_FACTURAS_09",
             "TARGET")


DATA <- DATA_INICIAL[DATA_INICIAL['PERIODO']<201612,VARIABLES]
DATA_PROD <- DATA_INICIAL[DATA_INICIAL['PERIODO']==201612,]

str(DATA)

library(caTools)

set.seed(0)
muestra <- sample.split(DATA, SplitRatio = 0.10)
trainData <- subset(DATA, muestra == TRUE)
testData <- subset(DATA, muestra == FALSE)

dim(trainData)
dim(testData)



library(randomForest)

set.seed(1)
modelo_3 <- randomForest(TARGET ~ . ,data = trainData,importance=T, ntree=100)

## 1. Interacciones optimas
plot(modelo_3) 

## 2. Importancia de las Variables
varImpPlot(modelo_3, type = 1)
varImpPlot(modelo_3, type = 2)

importancia=data.frame(importance(modelo_3))
library(reshape)
importancia<-sort_df(importancia,vars='MeanDecreaseAccuracy')
importancia

## 3. Predicción Test
prediccion_1_1 <- predict(modelo_3,testData,type="class") 
prediccion_1_2 <- predict(modelo_3,testData,type="prob")
## 4. Matriz de Confusión
MC<-table(testData$TARGET,prediccion_1_1)     
MC
Aciertos <- sum(diag(MC)) / sum(MC) * 100
cat("\nPrecision:",round(Aciertos,2),"%\n")

## 5. Curva ROC
library(ROCR)
predict.rocr  <- prediction(prediccion_1_2[,2],testData$TARGET)
perf.rocr     <- performance(predict.rocr,"tpr","fpr") #True y False postivie.rate

auc <- as.numeric(performance(predict.rocr ,"auc")@y.values)
plot(perf.rocr, col="yellow", lwd=5, main = paste('Area Bajo la Curva =',round(auc,2)))  
abline(a=0, b= 1)

auc

## 6. GINI
GINI<- (2*auc)-1
GINI

## 7. KS
testData$Default2M <- as.factor(testData$TARGET)

library(InformationValue)
ks_stat(actuals=testData$TARGET, predictedScores=prediccion_1_2[,2])


str(DATA_PROD)

prediccion_1_1 <- predict(modelo_3,DATA_PROD,type="class") 
prediccion_1_2 <- predict(modelo_3,DATA_PROD,type="prob")

proba_1 <- prediccion_1_2[,2]

data_summit <- cbind(DATA_PROD[c('ID_UNICO','NUM_FACTURAS','MONTO_DOLARES')],proba_1)

data_summit$FLAG_DEUDOR_MES[data_summit['NUM_FACTURAS'] > 0] <- 1
data_summit$FLAG_DEUDOR_MES[data_summit['NUM_FACTURAS'] == 0] <- 0

table(data_summit$FLAG_DEUDOR_MES)
table(data_summit$FLAG_DEUDOR_MES)

prod <- read.table(file="C:/Users/Keven/Desktop/RIMAC_CHALLENGE/Data/test.csv",header=TRUE,sep = ",")

head(prod)

head(data_summit)

data_final <- merge (data_summit, prod, by = "ID_UNICO")

data_final['PROBA'] <- data_final['proba_1']

summary(data_final['PROBA'])

data_final[c('ID_UNICO','PROBA')]
