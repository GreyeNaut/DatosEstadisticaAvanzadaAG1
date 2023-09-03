#Instalación de paquetes necesarios
install.packages("car")
library(car)
install.packages("glmnet")
library(glmnet)
install.packages("rstudioapi")
library(rstudioapi)
# Establece el directorio de trabajo actual donde se encuentra el script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Carga el archivo CSV en un data frame
Listings <- read.csv("Listings.csv")

# Verifica la estructura del data frame
str(Listings)
head(Listings)

# Obtén el número total de columnas en el data frame
num_columnas <- ncol(Listings)
# Itera a través de las columnas e imprime el encabezado y el número de columna
for (i in 1:num_columnas) {
  cat("Columna", i, ":", colnames(Listings)[i], "\n")
  print(head(Listings[, i]))
  cat("\n")
}

#Selección de variables:
#Creamos nuevo DataFram para el estudio
Listings_dfSV <- Listings

#Vemos el % de Null que en cada columna, que interferería mucho con la creación de la
#Matriz de correlación
na_percentage <- colMeans(is.na(Listings_dfSV)) * 100
na_percentage

#Eliminamos las dos columnas que están completamente vacías
Listings_dfSV <- Listings_dfSV[, !(names(Listings_dfSV) %in% c("bathrooms", "calendar_updated"))]

# Elimina los registros con NA o NaN en cualquier columna restante
Listings_dfSV <- na.omit(Listings_dfSV)

dim(Listings_dfSV)

# Elimina el signo de dólar y convierte price a numérico
Listings_dfSV$price <- as.numeric(gsub("\\$|,", "", Listings_dfSV$price))

#Guardamos las variables sin factorizar
Listings_dfSF <- Listings_dfSV

#Devuelve el nombre de todas las columnas categóricas
categorical_cols <- names(Listings_dfSV)[sapply(Listings_dfSV, is.factor) | sapply(Listings_dfSV, is.character)]
categorical_cols

# Convertir columnas categóricas en valores numéricos
for (col in categorical_cols) {
  Listings_dfSV[[col]] <- as.numeric(factor(Listings_dfSV[[col]]))
}

#Revisar la desviación estándar de todas las variables
std_deviations <- apply(Listings_dfSV, 2, sd)
std_deviations

# Encuentra las columnas con desviación estándar igual a cero
zero_std_columns <- names(std_deviations[std_deviations == 0])
# Elimina las columnas con desviación estándar igual a cero
Listings_dfSV <- Listings_dfSV[, !names(Listings_dfSV) %in% zero_std_columns]

# Calcular la matriz de correlación solo para "price" con todas las demás columnas
correlation_with_price <- cor(Listings_dfSV$price, Listings_dfSV[, -which(names(Listings_dfSV) == "price")])

# Visualizar la matriz de correlación
print(correlation_with_price)
# Obtener los nombres de las columnas correspondientes
column_names <- names(Listings_dfSV)[-which(names(Listings_dfSV) == "price")]
# Filtrar las correlaciones significativas (mayores de 0.4 en valor absoluto) y sus nombres
significant_correlations <- correlation_with_price[abs(correlation_with_price) > 0.05]
significant_column_names <- column_names[abs(correlation_with_price) > 0.05]
# Crear un data frame con las correlaciones y los nombres de las columnas
result_df <- data.frame(Columna = significant_column_names, Correlacion = significant_correlations)
# Visualizar las correlaciones significativas
print(result_df)
# Obtener las columnas significativas
columnas_correlacion <- Listings_dfSV[, c("price", significant_column_names)]
# Calcular la matriz de correlación
matriz_correlacion <- cor(columnas_correlacion)
# Visualizar la matriz de correlación
print(matriz_correlacion)
install.packages("corrplot")
library(corrplot)
# Crea un gráfico de matriz de correlación con corrplot
corrplot(matriz_correlacion, method = "color", type = "upper", tl.cex = 0.7)


#Valores de roomtype y property type
unique_values_property_type <- unique(Listings_dfSF$property_type)
unique_values_room_type <- unique(Listings_dfSF$room_type)
# Imprimir los valores únicos
print(unique_values_property_type)
print(unique_values_room_type)

#Se elimina property type y room type   por la cantidad de valores que tiene y ser dificilmente agrupables
Listings_df <- Listings_dfSF[, c("accommodates", "bedrooms", "beds", "price")]
# Obtener las columnas significativas
columnas_correlacion <- Listings_df[, c("price","accommodates", "bedrooms", "beds", "price")]
# Calcular la matriz de correlación
matriz_correlacion <- cor(columnas_correlacion)
# Visualizar la matriz de correlación
print(matriz_correlacion)
install.packages("corrplot")
library(corrplot)
# Crea un gráfico de matriz de correlación con corrplot
corrplot(matriz_correlacion, method = "color", type = "upper", tl.cex = 0.7)


##################################################################
# Creación del dataframe con las variables seleccionadas
model_data <- Listings_df
# Creación del modelo de regresión lineal múltiple
lm_model <- lm(price ~  accommodates + bedrooms + beds, data = model_data)
# Resumen del modelo
summary(lm_model)
#Estudiamos la colinealidad
library(car)
vif(lm_model)

# Creación del modelo de regresión lineal múltiple
lm_model <- lm(price ~ accommodates + bedrooms, data = model_data)
# Resumen del modelo
summary(lm_model)
#Estudiamos la colinealidad
vif(lm_model)

#Dividir la pantalla en 4
par(mfrow = c(2,2))
#Presenta los gráficos de diagnóstico
plot(lm_model)

# Generar características polinómicas de grado 2 para las variables seleccionadas
model_data$accommodates2 <- poly(model_data$accommodates, 2)
model_data$bedrooms2 <- poly(model_data$bedrooms, 2)
# Crear el modelo polinómico de grado 2
poly_model <- lm(price ~ accommodates2 + bedrooms2, data = model_data)
# Resumen del modelo polinómico
summary(poly_model)

#Ha empeorado los gráficos
plot(poly_model)


# Aplicar una transformación logarítmica a la variable objetivo
model_data$log_price <- log(model_data$price)
# Crear un nuevo modelo de regresión lineal con la variable transformada
lm_model_log1 <- lm(log_price ~ accommodates + bedrooms, data = model_data)
# Resumen del nuevo modelo
summary(lm_model_log1)
#Ha mejorado considerablemente los graficos
plot(lm_model_log1)
vif(lm_model_log1)

# Aplicar una transformación logarítmica a la variable objetivo
model_data$log_price <- log(model_data$price)
# Crear un nuevo modelo de regresión lineal con la variable transformada
lm_model_log2 <- lm(log_price ~ accommodates, data = model_data)
# Resumen del nuevo modelo
summary(lm_model_log2)
#Igual prácticamente
plot(lm_model_log2)





############### Modelo Logístico ################################


Listings_df2 <- Listings_dfSF[, c("room_type","accommodates", "bedrooms", "beds")]
#Sustituir los valores en la columna "room_type"
Listings_df2$room_type <- ifelse(Listings_df2$room_type %in% c("Private room", "Shared room"), "Habitación",
                                 ifelse(Listings_df2$room_type %in% c("Hotel room", "Entire home/apt"), "Entero", Listings_df2$room_type))
# Obtener las columnas significativas
columnas_correlacion2 <- Listings_df2[, c("room_type","accommodates", "bedrooms", "beds")]
# Calcular la matriz de correlación
matriz_correlacion2 <- cor(columnas_correlacion2)
# Visualizar la matriz de correlación
print(matriz_correlacion2)
# Crea un gráfico de matriz de correlación con corrplot
corrplot(matriz_correlacion2, method = "color", type = "upper", tl.cex = 0.7)

Listings_df2 <- Listings_df2[, c("room_type","accommodates", "bedrooms", "beds")]



# Cargar la librería "caTools" para dividir los datos
library(caTools)
# Establecer una semilla para la reproducibilidad
set.seed(123)
# Dividir el dataset en entrenamiento (70%) y prueba (30%)
split <- sample.split(Listings_df2$room_type, SplitRatio = 0.7)
train_data <- subset(Listings_df2, split == TRUE)
test_data <- subset(Listings_df2, split == FALSE)

# Codificar room_type como factor con dos niveles: "Habitación" y "Entero"
train_data$room_type <- factor(train_data$room_type, levels = c("Habitación", "Entero"))

# Crear el modelo de regresión logística
logistic_model <- glm(room_type ~ ., data = train_data, family = "binomial")

summary(logistic_model)

logistic_model <- glm(room_type ~ accommodates + beds, data = train_data, family = "binomial")

summary(logistic_model)

vif(logistic_model)

logistic_model <- glm(room_type ~  beds, data = train_data, family = "binomial")

summary(logistic_model)

vif(logistic_model)

# Ajustar el modelo a los datos de entrenamiento
logistic_model_fit <- step(logistic_model)

# Predecir las clases en el conjunto de prueba
predicted_classes <- predict(logistic_model_fit, newdata = test_data, type = "response")

# Convertir las probabilidades en clases (0 o 1)
predicted_classes <- ifelse(predicted_classes >= 0.5, 1, 0)

# Calcular la matriz de confusión y otras métricas de evaluación
confusion_matrix <- table(test_data$room_type, predicted_classes)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Imprimir la matriz de confusión y métricas de evaluación
print(confusion_matrix)
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))




