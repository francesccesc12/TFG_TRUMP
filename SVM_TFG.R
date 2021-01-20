library(e1071)
library("rgl")
library(ggplot2)

#####


lagpad <- function(x, k) {
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
  }
}

######


load("DJIA.Total.Rdata")

prop.table(table(DJIA.total$binary))


DJIA.total$binary <- as.factor(DJIA.total$binary)
DJIA.total[is.na(DJIA.total$ratio.neg.scaled),]$ratio.neg.scaled <- 0
DJIA.total[is.na(DJIA.total$ratio.pos.scaled),]$ratio.pos.scaled <- 0
DJIA.total[is.na(DJIA.total$index.pol.scaled),]$index.pol.scaled <- 0
DJIA.total[is.na(DJIA.total$index.pol2.scaled),]$index.pol2.scaled <- 0

ggplot(data = DJIA.total, aes(x = lagpad(DJIA.total$return,1), y = lagpad(DJIA.total$return,2), color = binary)) +
  geom_point(size =2.5) +
  theme_bw() +
  theme(legend.position = "right")



#FICAR NOMS I NO LAGPADS!

DJIA.SVM <- DJIA.total[,c("binary","return", "dia")]
DJIA.SVM$lag1 <- lagpad(DJIA.total$return,1) 
DJIA.SVM$lag2 <- lagpad(DJIA.total$return,2) 
DJIA.SVM$lag3 <- lagpad(DJIA.total$return,3) 
DJIA.SVM$return_scaled <- as.vector(scale(DJIA.SVM$return))
DJIA.SVM$scaled_lag1 <- as.vector(scale(DJIA.SVM$lag1))
DJIA.SVM$scaled_lag2 <- as.vector(scale(DJIA.SVM$lag2))
DJIA.SVM$scaled_lag3 <- as.vector(scale(DJIA.SVM$lag3))

DJIA.SVM$ratio_neg_lag3 <-  lagpad(DJIA.total$ratio.neg.scaled,3) 
DJIA.SVM$ratio_neg_lag2 <-  lagpad(DJIA.total$ratio.neg.scaled,2) 
DJIA.SVM$ratio_neg_lag1 <-  lagpad(DJIA.total$ratio.neg.scaled,1) 

DJIA.SVM$ratio_pos_lag3 <-  lagpad(DJIA.total$ratio.pos.scaled,3) 
DJIA.SVM$ratio_pos_lag2 <-  lagpad(DJIA.total$ratio.pos.scaled,2) 
DJIA.SVM$ratio_pos_lag1 <-  lagpad(DJIA.total$ratio.pos.scaled,1) 

DJIA.SVM$index_lag3 <-  lagpad(DJIA.total$index.pol.scaled,3) 
DJIA.SVM$index_lag2 <-  lagpad(DJIA.total$index.pol.scaled,2) 
DJIA.SVM$index_lag1 <-  lagpad(DJIA.total$index.pol.scaled,1) 

DJIA.SVM$index2_lag3 <-  lagpad(DJIA.total$index.pol2.scaled,3) 
DJIA.SVM$index2_lag2 <-  lagpad(DJIA.total$index.pol2.scaled,2) 
DJIA.SVM$index2_lag1 <-  lagpad(DJIA.total$index.pol2.scaled,1) 



DJIA.SVM$ratio_neg_lag8 <-  lagpad(DJIA.total$ratio.neg.scaled,8) 
DJIA.SVM$ratio_pos_lag8 <-  lagpad(DJIA.total$ratio.pos.scaled,8) 
DJIA.SVM$index_lag8 <-  lagpad(DJIA.total$index.pol.scaled,8) 

DJIA.SVM <- DJIA.SVM[-(1:3),] #Elimino NA principi


set.seed(12)
train <-  sample(seq(length(DJIA.SVM$binary)),length(DJIA.SVM$binary)*0.70,replace=FALSE)
learning <- sample(train, length(train)*0.80, replace = FALSE)

set.seed(12)
test <- (1389-418):1386
train <- 1:970
learning <- sample(train, length(train)*0.80, replace = FALSE)






svm_cv <- tune("svm", binary ~ lag1 + lag2 + lag3,data= DJIA.SVM[learning,] ,kernel="linear",scale=TRUE,
               ranges = list(cost = c(0.00001,0.0001,0.001,0.01,0.1,1:10 )),class.weights = c("0" = 1.25, "1" = 1))


svm1 <- svm(binary ~ lag1 + lag2 + lag3,data= DJIA.SVM ,kernel="linear",scale=TRUE,subset=learning, cost = 1,
            class.weights = c("0" = 1.25, "1" = 1))

summary(svm_cv2)

ggplot(data = svm_cv2$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  labs(title = "Error de clasificación vs hiperparámetreo C") +
  theme_bw()


#Resultats predits-observats amb train
t1 <- table(DJIA.SVM$binary[learning],svm1$fitted) 
t1
accuracy_TRAIN <- sum(diag(t1))*100 /(length(learning))
t1
error.rate_TRAIN <- 100-accuracy_TRAIN
precision_TRAIN <- t1[1,1]*100 / (sum(t1[1,1],t1[2,1]))
recall_TRAIN<- t1[1,1]*100 / (sum(t1[1,1],t1[1,2]))
f_TRAIN<- 2*precision_TRAIN*recall_TRAIN /(precision_TRAIN + recall_TRAIN)


#CUADRAR NOMESS

validation <- train[!train %in% learning]

# Predicción para la muestra test
svm.pred <- predict(object = svm1,newdata = DJIA.SVM[validation,])
summary(svm.pred)



#Resultats predits-observats amb test
t2 <- with(DJIA.SVM[validation,],table(svm.pred,binary))
t2 
accuracy_TEST <- sum(diag(t2))*100 / nrow(DJIA.total[validation,])
error.rate_TEST <- 100-accuracy_TEST
precision_TEST <- t2[1,1]*100 / (sum(t2[1,1],t2[2,1]))
recall_TEST <- t2[1,1]*100 / (sum(t2[1,1],t2[1,2]))
f_TEST <- 2*precision_TEST*recall_TEST /(precision_TEST + recall_TEST)




# INCORPORATEEEEEEE SENTIMENAL ANALISSISS!

svm2 <- svm(binary ~lag1 + lag2 + lag3 + ratio_neg_lag1 + ratio_neg_lag2 +  ratio_neg_lag3,data= DJIA.SVM ,kernel="linear",scale=TRUE, subset=learning, cost = 1,
            class.weights = c("0" = 1.25, "1" = 1))

summary(svm2)

svm_cv <- tune("svm", binary ~ lag1 + lag2 + lag3 ,data= DJIA.SVM[learning,] ,kernel="linear",scale=TRUE,
                ranges = list(cost = c(0.00001,0.01,0.1,1:10 )),class.weights = c("0" = 1.25, "1" = 1))

summary(svm_cv)

svm1 <- svm_cv$best.model

svm_cv2 <- tune("svm", binary ~ lag1 + lag2 + lag3 + ratio_neg_lag3,data= DJIA.SVM[learning,] ,kernel="linear",scale=TRUE,
               ranges = list(cost = c(0.00001,0.01,0.1,1:10 )),class.weights = c("0" = 1.25, "1" = 1))

summary(svm_cv2)

svm2 <-svm(binary ~ lag1 + lag2 + lag3 + ratio_neg_lag3,data= DJIA.SVM ,kernel="linear",scale=TRUE,
            subset = learning,class.weights = c("0" = 1.25, "1" = 1), cost = 2)


#Resultats predits-observats amb train
t1 <- table(DJIA.SVM$binary[learning],svm2$fitted) #
t1
accuracy2_TRAIN <- sum(diag(t1))*100 /(length(learning))
t1

error.rate2_TRAIN <- 100-accuracy2_TRAIN
precision2_TRAIN <- t1[1,1]*100 / (sum(t1[1,1],t1[2,1]))
recall2_TRAIN<- t1[1,1]*100 / (sum(t1[1,1],t1[1,2]))
f2_TRAIN <- 2*precision2_TRAIN*recall2_TRAIN /(precision2_TRAIN + recall2_TRAIN)


#CUADRAR NOMESS

validation <- train[!train %in% learning]

# Predicción para la muestra test
svm.pred <- predict(object = svm2,newdata = DJIA.SVM[validation,])
summary(svm.pred)



#Resultats predits-observats amb test
t2 <- with(DJIA.SVM[validation,],table(svm.pred,binary))
t2 
accuracy2_TEST <- sum(diag(t2))*100 / nrow(DJIA.total[validation,])
error.rate2_TEST <- 100-accuracy2_TEST
precision2_TEST <- t2[1,1]*100 / (sum(t2[1,1],t2[2,1]))
recall2_TEST <- t2[1,1]*100 / (sum(t2[1,1],t2[1,2]))
f2_TEST <- 2*precision2_TEST*recall2_TEST /(precision2_TEST + recall2_TEST)


#LINEAAAAAAAL 


# DIBUIXA HIPERPLANO
#ESCALO LES VARIABLES 

# Para extraer la ecuación del hiperplano y del margen es necesario aplicar 
# algebra lineal (sobre ESCALADOS!)

#modelo solo con 2

svm_graf2 <- svm(binary ~ scaled_lag1 + scaled_lag2, data= DJIA.SVM ,kernel="linear",scale=FALSE, cost = 10,
                 class.weights = c("0" = 1.27, "1" = 1))

beta <- drop(t(svm_graf2$coefs) %*% as.matrix(DJIA.SVM[,c("scaled_lag1","scaled_lag2")])[svm_graf2$index,])
beta0 <- svm_graf2$rho

rango_X1 <- range(DJIA.SVM$scaled_lag1)
rango_X2 <- range(DJIA.SVM$scaled_lag2)


# Interpolación de puntos
new_x1 <- seq(from = rango_X1[1], to = rango_X1[2], length = 100)
new_x2 <- seq(from = rango_X2[1], to = rango_X2[2], length = 100)
nuevos_puntos <- expand.grid(scaled_lag1 = new_x1, scaled_lag2 = new_x2)

# Predicción según el modelo
predicciones <- predict(object = svm_graf2, newdata = nuevos_puntos)
table(predicciones)

# Se almacenan los puntos predichos para dar color a las regiones
color_regiones <- data.frame(nuevos_puntos, binary = predicciones)


ggplot() +
  # Representación de las 2 regiones empleando los puntos y coloreándolos
  # según la clase predicha por el modelo
  geom_point(data = color_regiones, aes(x = scaled_lag1, y = scaled_lag2, color = binary),
             size = 0.5) +
  # Se añaden las observaciones
  geom_point(data = DJIA.SVM, aes(x = scaled_lag1, y = scaled_lag2, color = as.factor(binary)),
             size = 6) +
  # # Se identifican aquellas observaciones que son vectores soporte del modelo
  geom_point(data = DJIA.SVM[svm_graf2$index, ],
             aes(x = scaled_lag1, y = scaled_lag2, color = as.factor(binary)),
             shape = 21, colour = "black", alpha= 0.001,
             size = 6) +
  # Se añaden las rectas del hiperplano y los márgenes
  geom_abline(intercept = beta0/beta[2], slope = -beta[1]/beta[2]) +
  geom_abline(intercept = (beta0 - 1)/beta[2], slope = -beta[1]/beta[2],
              linetype = "dashed") +    
  geom_abline(intercept = (beta0 + 1)/beta[2], slope = -beta[1]/beta[2],
              linetype = "dashed") +
  theme_bw() +
  theme(legend.position = "right")

# RADIAAAAAL 

# Se interpolar puntos dentro del rango de los dos predictores X1 y X2.
# Estos nuevos puntos se emplean para predecir la variable respuesta acorde
# al modelo y así colorear las regiones que separa el hiperplano.


svm_graf2 <- svm(binary ~ scaled_lag1 + scaled_lag2, data= DJIA.SVM ,kernel="radial",scale=FALSE, cost = 10,
                 class.weights = c("0" = 1.27, "1" = 1))


rango_X1 <- range(DJIA.SVM$scaled_lag1)
rango_X2 <- range(DJIA.SVM$scaled_lag2)


# Interpolación de puntos
new_x1 <- seq(from = rango_X1[1], to = rango_X1[2], length = 100)
new_x2 <- seq(from = rango_X2[1], to = rango_X2[2], length = 100)
nuevos_puntos <- expand.grid(scaled_lag1 = new_x1, scaled_lag2 = new_x2)

# Predicción según el modelo
predicciones <- predict(object = svm_graf2, newdata = nuevos_puntos)
table(predicciones)

# Se almacenan los puntos predichos para dar color a las regiones
color_regiones <- data.frame(nuevos_puntos, binary = predicciones)


ggplot() +
  # Representación de las 2 regiones empleando los puntos y coloreándolos
  # según la clase predicha por el modelo
  geom_point(data = color_regiones, aes(x = scaled_lag1, y = scaled_lag2, color = binary),
             size = 0.5) +
  # Se añaden las observaciones
  geom_point(data = DJIA.SVM, aes(x = scaled_lag1, y = scaled_lag2, color = as.factor(binary)),
             size = 2.5) +
  # Se identifican aquellas observaciones que son vectores soporte
  geom_point(data = DJIA.SVM[svm_graf2$index, ],
             aes(x = scaled_lag1, y = scaled_lag2, color = as.factor(binary)),
             shape = 21, colour = "black",
             size = 2.5, alpha = 0.001) +
  theme_bw() +
  theme(legend.position = "right")

#### RADIAL GENERAL 

svm_cv <- tune("svm", binary ~ lag1 + lag2 + lag3,data= DJIA.SVM[learning,], kernel = 'radial', 
               ranges = list(cost = c(0.01, 0.1, 1,  10,20,30),
                             gamma = c(0.5, 1, 2, 5, 10)))

svm1 <- svm(binary ~ lag1 + lag2 + lag3,data= DJIA.SVM ,kernel="radial",scale=TRUE,subset=learning, cost = 10, gamma = 2,
            class.weights = c("0" = 1.25, "1" = 1))



summary(svm_cv)

ggplot(data = svm_cv2$performances, aes(x = cost, y = error, color = as.factor(gamma)))+
  geom_line() +
  geom_point() +
  labs(title = "Error de clasificación vs hiperparámetros C y gamma", color = "gamma") +
  theme_bw() +
  theme(legend.position = "bottom")

summary(svm_cv)

svm2 <- svm(binary ~ lag1 + lag2 + lag3+  ratio_neg_lag3, DJIA.SVM ,kernel="radial",scale=TRUE,subset=learning, cost = 1, gamma = 0.5,
            class.weights = c("0" = 1.25, "1" = 1))

svm_cv2 <- tune("svm", binary ~ lag1 + lag2 + lag3 + ratio_neg_lag3 + ratio_neg_lag2 + ratio_neg_lag1,data= DJIA.SVM[learning,], kernel = 'radial', 
               ranges = list(cost = c(0.01, 0.1, 1,  10, 20),
                             gamma = c(0.5, 1, 2, 5, 10))) # 0.41

svm_cv2 <- tune("svm", binary ~ lag1 + lag2 + lag3 + ratio_neg_lag3,data= DJIA.SVM[learning,], kernel = 'radial', 
                ranges = list(cost = c(0.01, 0.1, 1,  10, 20),
                              gamma = c(0.5, 1, 2, 5, 10))) #0.38!

svm_cv2 <- tune("svm", binary ~ lag1 + lag2 + lag3 + ratio_neg_lag3 + ratio_neg_lag2 + ratio_neg_lag1 + ratio_pos_lag1 + ratio_pos_lag2 + ratio_pos_lag3 + index2_lag1 + index2_lag2 + index2_lag3 + index_lag1 + index_lag2 + index_lag3,data= DJIA.SVM[learning,], kernel = 'radial', 
                ranges = list(cost = c(0.01, 0.1, 1,  10, 20),
                              gamma = c(0.5, 1, 2, 5, 10))) #0.42

svm_cv2 <- tune("svm", binary ~ lag1 + lag2 + lag3 + ratio_neg_lag3 + ratio_neg_lag2 + ratio_neg_lag1  + index2_lag1 + index2_lag2 + index2_lag3 + index_lag1 + index_lag2 + index_lag3,data= DJIA.SVM[learning,], kernel = 'radial', 
                ranges = list(cost = c(0.01, 0.1, 1,  10, 20),
                              gamma = c(0.5, 1, 2, 5, 10))) #0.41

svm_cv2 <- tune("svm", binary ~ lag1 + lag2 + lag3 + ratio_neg_lag3 + ratio_neg_lag2 + ratio_neg_lag1 + ratio_pos_lag1 + ratio_pos_lag2 + ratio_pos_lag3 + index_lag1 + index_lag2 + index_lag3,data= DJIA.SVM[learning,], kernel = 'radial', 
                ranges = list(cost = c(0.01, 0.1, 1,  10, 20),
                              gamma = c(0.5, 1, 2, 5, 10))) #0.42

svm_cv2 <- tune("svm", binary ~ lag1 + lag2 + lag3 + ratio_neg_lag3 + ratio_neg_lag2 + ratio_neg_lag1 + ratio_pos_lag1 + ratio_pos_lag2 + ratio_pos_lag3 + index2_lag1 + index2_lag2 + index2_lag3 ,data= DJIA.SVM[learning,], kernel = 'radial', 
                ranges = list(cost = c(0.01, 0.1, 1,  10, 20),
                              gamma = c(0.5, 1, 2, 5, 10))) #0.42







summary(svm_cv2)

# Predicción para la muestra test
svm.pred <- predict(object = svm1,newdata = DJIA.SVM[test,])
summary(svm.pred)



#Resultats predits-observats amb test
t2 <- with(DJIA.SVM[test,],table(svm.pred,binary))
t2 
accuracy2_TEST <- sum(diag(t2))*100 / nrow(DJIA.total[test,])
error.rate2_TEST <- 100-accuracy2_TEST
precision2_TEST <- t2[1,1]*100 / (sum(t2[1,1],t2[2,1]))
recall2_TEST <- t2[1,1]*100 / (sum(t2[1,1],t2[1,2]))
f2_TEST <- 2*precision2_TEST*recall2_TEST /(precision2_TEST + recall2_TEST)

