# DJIA 


library(quantmod) #indicesbursailtes
library(lmtest) #grangertest
library(tseries) #adf test
library(vars) #var model
library(e1071)


#CARGAMOS INDICES DE SENTIMIENTO
load("index.pol.scales.RData")
load("ratio.neg.scales.Rdata")
load("ratio.pos.scales.Rdata")
load("index.pol2.scaled.RData")
load("prueba2.Rdata")
load("DJIA.total.RData")

df.DJIA <- DJIA.total
df.DJIA$return.scaled <- scale(df.DJIA$return)

#CARGAMOS INFO DIFERENTES INDICES BURSATILES: 
DJIA <- getSymbols("^DJI", src = "yahoo",from = "2017-01-16", to = "2020-11-07")
summary(DJI)

#Nos quedamos con el close price de todas 
DJIA.c <- DJI[,4]

DJIA.return <-  ROC(DJIA.c$DJI.Close,n=1,type="discrete") 

# añadimos valores del fin de semana por interpolación: 
DJIA.x <- merge(DJIA.c, zoo(,seq(start(DJIA.c),end(DJIA.c),by="day")), all=TRUE)
DJIA.x$inter <- ifelse(is.na(DJIA.x), 1, 0)
DJIA.x <- na.approx(DJIA.x)


# Calculamos los log-returns (Rendimientos)
DJIA.x$return <- ROC(DJIA.x$DJI.Close,n=1,type="discrete") 
DJIA.x <- DJIA.x[- which(is.na(DJIA.x$return)),]

#graficos 


plot(as.vector(VIX.return$VIX.Close), type = "l", xaxt = "n", font.axis =2, font.lab = 2, col = "steelblue", ylab = "Rendimientos.VIX", xlab = "", las = 2,  ylim = c(-1,1.5))
plot(as.vector(VIX.c$VIX.Close), type = "l", xaxt = "n", font.axis =2, font.lab = 2, col = "steelblue", ylab = "VIX.Close", xlab = "", las = 2, ylim = c(0,100))

times <- seq(from = as.Date("2017-01-01"), to = as.Date("2020-11-06"), by = "quarter")
grid(ny = NULL, nx = 17)
axis(1, c(1,at = seq(from = 60,to = 940, by = 940/15),970), labels = c(as.Date("2017-01-17"),times[2:16],as.Date("2020-11-06")), cex.axis = 0.7, font = 2, las = 2)

axis(1, c(1,at = seq(from = 60,to = 1389, by = 1389/15),1405), labels = c(as.Date("2017-01-18"),times[2:16],as.Date("2020-11-06")), cex.axis = 0.7, font = 2, las = 2)


# Escalamos también las variables economicas: 
DJIA.x$return.scaled <- scale(DJIA.x$return)

DJIA.x <- as.data.frame(DJIA.x)
DJIA.x$dia <- as.Date(rownames(DJIA.x))

DJIA.total <- merge(DJIA.x, prueba2, by.x = "dia", by.y = "anyo_mes", all.x = TRUE)

# Variable binaria 1 si la bolsa sube 0 si baja 
DJIA.total$binary <- ifelse( DJIA.total$return > 0, 1, 0)

save(DJIA.total, file = "DJIA.total.Rdata")

df.DJIA <- DJIA.total

df.DJIA$return.scaled <- scale(df.DJIA$return)

# alguns grafics 

plot(scale(DJIA.c))

#####################


plot(rollmean(df.DJIA$return.scaled, k = 30, fill=NA), type = "l", ylim = c(-1.5,1.5), xaxt ="n", ylab = "DJIA y pol.index2 escalados",xlab = "", font.lab = 2)
lines(rollmean(df.DJIA$index.pol2.scaled, k =30 , fill = NA, na.rm = TRUE), col = "green", lwd = 2)
grid(ny=7,nx=16)
abline(h = (0-mean(df.DJIA$index.pol2, na.rm = T))/sd(df.DJIA$index.pol2, na.rm = T), col = "red", lty = 3)
axis(1, at = seq(1,1389,90), labels = strftime(df.DJIA$dia[seq(1,1389,90)],"%Y-%m") , cex.axis=0.60, srt=45, col.ticks = "grey", las=2)
legend(x = c(100,500), y= c(-1.2, -0.8),legend = c("DJIA 30-días media móvil", "Índice de polaridad_2 30-días media móvil "), fill = c("black", "green"), cex = 0.6)

#######################

plot(rollmean(df.DJIA$return.scaled, k = 30, fill=NA), type = "l", xaxt ="n",ylim = c(-1.5, 1), xlab = "", ylab = "DJIA y pol.index1 escalados", font.lab = 2)
lines(rollmean(df.DJIA$index.pol.scaled, k =30 , fill = NA, na.rm = TRUE), col = "green", lwd = 2)
grid(ny=7,nx=16)
abline(h = (0-mean(df.DJIA$index.pol, na.rm = T))/sd(df.DJIA$index.pol, na.rm = T), col = "red", lty = 3)
axis(1, at = seq(1,1389,90), labels = strftime(df.DJIA$dia[seq(1,1389,90)],"%Y-%m") , cex.axis=0.60, srt=45, col.ticks = "grey", las=2)
legend(x = c(100,550), y= c(-1.2, -0.8),legend = c("DJIA 30-días media móvil", "Índice de polaridad_1 30-días media móvil "), fill = c("black", "green"), cex = 0.7)

#######################

plot(rollmean(df.DJIA$return.scaled, k = 30, fill=NA), type = "l", xaxt ="n",ylim = c(-1.5, 1), xlab = "", ylab = "DJIA y Ratio Negatividad escalados", font.lab = 2)
lines(rollmean(df.DJIA$ratio.neg.scaled, k =30 , fill = NA, na.rm = TRUE), col = "red", lwd = 2)
grid(ny=7,nx=16)
abline(h = (0.5-mean(df.DJIA$ratio_neg, na.rm = T))/sd(df.DJIA$ratio_neg, na.rm = T), col = "red", lty = 3)
axis(1, at = seq(1,1389,90), labels = strftime(df.DJIA$dia[seq(1,1389,90)],"%Y-%m") , cex.axis=0.60, srt=45, col.ticks = "grey", las=2)
legend(x = c(100,550), y= c(-1.2, -0.8), legend = c("DJIA 30-días media móvil", "Ratio de Negatividad 30-días media móvil "), fill = c("black", "red"), cex = 0.7)

########################

plot(rollmean(df.DJIA$return.scaled, k = 30, fill=NA), type = "l", xaxt ="n",ylim = c(-1.5, 1),  xlab = "", ylab = "DJIA y Ratio Positividad escalados", font.lab = 2)
lines(rollmean(df.DJIA$ratio.pos.scaled, k =30 , fill = NA, na.rm = TRUE), col = "blue", lwd = 2)
grid(ny=7,nx=16)
abline(h = (0.5-mean(df.DJIA$ratio_pos, na.rm = T))/sd(df.DJIA$ratio_pos, na.rm = T), col = "red", lty = 3)
axis(1, at = seq(1,1389,90), labels = strftime(df.DJIA$dia[seq(1,1389,90)],"%Y-%m") , cex.axis=0.60, srt=45, col.ticks = "grey", las=2)
legend(x = c(100,550), y= c(-1.2, -0.8),legend = c("DJIA 30-días media móvil", "Ratio Positividad 30-días media móvil "), fill = c("black", "blue"), cex = 0.7)

# grangertest ESCALANT O SENSE ESCALAR? 

pvalor <- function(v1,v2,n){
  v <- vector()
  for (i in 1:n){
    v[i] <- grangertest(x = v1, y = v2, order = i)[2,4]
    
  }
  return(v)
}

pvalor(df.DJIA$index.pol,df.DJIA$return,10) #VAMOOOOOOS POL 2 8 PUES NO...(df mes curt si...)

pvalor(df.DJIA$index.pol2.scaled, df.DJIA$return.scaled, 8)

pvalor(df.DJIA$ratio_pos, df.DJIA$binary,10) # pol 1, 0.1008... (3 day) + ratio_neg 0.09 day 3 + ratio_pos 0.11 day 3


out_granger <- grangertest(x = df.DJIA$index.pol,y  = df.DJIA$return, order = 10)

sink(file = "out_GRANGER.txt")
grangertest(x = df.DJIA$index.pol,y  = df.DJIA$return, order = 10)
sink(file = NULL)


# lm 

lm2 <- lm(df.DJIA$return.scaled ~ lag(df.DJIA$return.scaled) + lag(df.DJIA$return.scaled,  2 ) + lag(df.DJIA$return.scaled, 3) + lag(df.DJIA$return.scaled,  4) + lag(df.DJIA$return.scaled,  5)
          + lag(df.DJIA$return.scaled,  6 ) + lag(df.DJIA$return.scaled,  7) + lag(df.DJIA$return.scaled,  8) +  lag(df.DJIA$index.pol2.scaled,  2) + lag(df.DJIA$index.pol2.scaled,  3) 
          + lag(df.DJIA$index.pol2.scaled,  4) + lag(df.DJIA$index.pol2.scaled,  5) +  lag(df.DJIA$index.pol2.scaled,  6) +  lag(df.DJIA$index.pol2.scaled,  7) +  lag(df.DJIA$index.pol2.scaled,  8))

summary(lm2)


plot(y = df.DJIA$return.scaled, x = lag(df.DJIA$index.pol2.scaled, n = 5),  ylim = c(-12, 12), xlim = c(-4,6), ylab = "DJIA returns escalados", xlab = "Ind. Pol_2 escalado", pch = 16, font.lab = 2)
abline(a = lm2$coefficients[1], lm2$coefficients[13], col = "red", lty = 2)
text(2, -8, paste0("DJIA.returns = ", round(lm2$coefficients[1],3), " ", round(lm2$coefficients[13],3), "*Ind.Pol_2"), col= "red", cex = 0.8)


lm3 <- lm(df.DJIA$binary ~ lag(df.DJIA$binary, n = 1) + lag(df.DJIA$binary, n = 2) + lag(df.DJIA$binary, n = 3) + lag(df.DJIA$ratio_neg, n =1)
          + lag(df.DJIA$ratio_neg, n =2) + lag(df.DJIA$ratio_neg, n =3))

summary(lm3)

plot(y = df.DJIA$binary, x = lag(df.DJIA$ratio_neg, n = 3),ylab = "DJIA binary", xlab = "Ratio_neg", pch = 16, font.lab = 2)
abline(a = lm2$coefficients[1], lm2$coefficients[13], col = "red", lty = 2)
text(2, -8, paste0("DJIA.returns = ", round(lm2$coefficients[1],3), " ", round(lm2$coefficients[13],3), "*Ind.Pol_2"), col= "red", cex = 0.8)

plot(lag(df.DJIA$ratio_neg, n = 3), col=c('red', 'blue')[as.factor(df.DJIA$binary)])

# MODEL LOG? 

# logistic model : 

log_model <- glm(df.DJIA$binary ~  lagpad(df.DJIA$binary,1) + lagpad(df.DJIA$binary,2) + lagpad(df.DJIA$binary,3) + lagpad(df.DJIA$ratio_neg, 1)
                 + lagpad(df.DJIA$ratio_neg, 2) + lagpad(df.DJIA$ratio_neg, 3) ,family=binomial(link='logit'))

summary(log_model)

df <- data.frame("lagpad(df.DJIA$binary,1)" = mean(lagpad(df.DJIA$binary,1),na.rm = T),"lagpad(df.DJIA$binary,2)" = mean(lagpad(df.DJIA$binary,2), na.rm = T) ,"lagpad(df.DJIA$binary,3)" = mean(lagpad(df.DJIA$binary,3), na.rm = T), "lagpad(df.DJIA$ratio_neg, 1)" = mean(lagpad(df.DJIA$ratio_neg, 1), na.rm = T),
                 "lagpad(df.DJIA$ratio_neg, 2)" = mean(lagpad(df.DJIA$ratio_neg, 2), na.rm = T), "lagpad(df.DJIA$ratio_neg, 3))" = 0.2)


names(df) <- c("lagpad(df.DJIA$binary,1)", "lagpad(df.DJIA$binary,2)", "lagpad(df.DJIA$binary,3)", "lagpad(df.DJIA$ratio_neg, 1)", "lagpad(df.DJIA$ratio_neg, 2)", "lagpad(df.DJIA$ratio_neg, 3)")

names(df) <- names(log_model[["model"]])[2:7]


log.odds <- predict(log_model, df)

exp(log.odds)/(1+exp(log.odds))

library(popbio)
logi.hist.plot(lagpad(df.DJIA$ratio_neg,3)[which(!is.na(lagpad(df.DJIA$ratio_neg,3)))], 
               df.DJIA[which(!is.na(lagpad(df.DJIA$ratio_neg,3))),]$binary,boxp=FALSE,type="hist",
               col="steelblue", xlab = "Ratio Negatividad_{t-3}", ylabel = "DJIA binary", ylabel2 = "Frecuencia", counts = TRUE)



############### FINNNN

#DIVIDIM EN ANYS:

df.DJIA <- DJIA.total

df.DJIA_2017 <- df.DJIA[df.DJIA$dia < "2018-01-01",]
df.DJIA_2018 <- df.DJIA[df.DJIA$dia > "2017-12-31" & df.DJIA$dia < "2019-01-01",]
df.DJIA_2019 <- df.DJIA[df.DJIA$dia > "2018-12-31" & df.DJIA$dia < "2020-01-01",]
df.DJIA_2020 <- df.DJIA[df.DJIA$dia > "2019-12-31",]


pvalor(df.DJIA_2017$ratio_neg, df.DJIA_2017$return, 10) #RATIO NEG 8-9 DIES //
pvalor(df.DJIA_2017$ratio_neg, df.DJIA_2017$binary, 10) # NADA

df.DJIA$return.scaled <- scale(df.DJIA$return)

lm2 <- lm(df.DJIA_2017$return ~ lagpad(df.DJIA_2017$return,1) + lagpad(df.DJIA_2017$return,2 ) + lagpad(df.DJIA_2017$return, 3) + lagpad(df.DJIA_2017$return,  4) + lagpad(df.DJIA_2017$return,  5)
          + lagpad(df.DJIA_2017$return,  6 ) + lagpad(df.DJIA_2017$return,  7) + lagpad(df.DJIA_2017$return,  8) + lagpad(df.DJIA_2017$return,  9) + lagpad(df.DJIA_2017$return,  10) +  lagpad(df.DJIA_2017$ratio_neg,  1) +lagpad(df.DJIA_2017$ratio_neg,  2) + lagpad(df.DJIA_2017$ratio_neg,  3) 
          + lagpad(df.DJIA_2017$ratio_neg,  4) + lagpad(df.DJIA_2017$ratio_neg,  5) +  lagpad(df.DJIA_2017$ratio_neg,  6) +  lagpad(df.DJIA_2017$ratio_neg,  7) +  lagpad(df.DJIA_2017$ratio_neg,  8) + lagpad(df.DJIA_2017$ratio_neg,  9) + lagpad(df.DJIA_2017$ratio_neg,  10))

summary(lm2)

plot(df.DJIA_2017$return.scaled ~ lagpad(df.DJIA_2017$ratio.neg.scaled,  8))

sink(file = "output2.txt")
summary(lm2)
sink(file = NULL)

pvalor(df.DJIA_2018$index.pol2, df.DJIA_2018$return, 10) #NADA
pvalor(df.DJIA_2018$index.pol2, df.DJIA_2018$binary, 10) #NADA

pvalor(df.DJIA_2019$index.pol2, df.DJIA_2019$return, 10) #NADA
pvalor(df.DJIA_2019$index.pol2, df.DJIA_2019$binary, 10) #NADA

pvalor(df.DJIA_2020$index.pol.scaled, df.DJIA_2020$return.scaled, 10) #NADA
pvalor(df.DJIA_2020$index.pol2, df.DJIA_2020$binary, 10) #NADA

#DOBLE ESCALA

source("https://gist.githubusercontent.com/ellisp/4002241def4e2b360189e58c3f461b4a/raw/9ab547bff18f73e783aaf30a7e4851c9a2f95b80/dualplot.R")     

dualplot(y2 = rollmean(df.DJIA$index.pol,k = 30, fill= NA, na.rm= TRUE) , y1 =  rollmean(df.DJIA$return,k=30, fill =NA), x1 = seq(from= as.Date("2017-01-18"), to = as.Date("2020-11-06"), by = 1 ), ylim1 = c(-0.015,0.01), ylim2 = c(-0.2,0.6)
         ,nxbreaks = 30, yleg1 = "DJIA 30-días media móvil", yleg2 =  "Índice de polaridad_1 30-días media móvil", ylab1 = "", ylab2 = "")



dualplot(y2 = rollmean(df.DJIA$index.pol,k = 30, fill= NA, na.rm= TRUE) , y1 =  rollmean(df.DJIA$DJI.Close,k=30, fill =NA), x1 = seq(from= as.Date("2017-01-18"), to = as.Date("2020-11-06"), by = 1 ), ylim1 = c(20000,30000), ylim2 = c(-0.2,0.6)
         ,nxbreaks = 30, yleg1 = "DJIA-Índice 30-días media móvil", yleg2 =  "Índice de polaridad_1 30-días media móvil", ylab1 = "", ylab2 = "")

dualplot(y2 = df.DJIA$index.pol, y1 =  df.DJIA$return, x1 = seq(from= as.Date("2017-01-18"), to = as.Date("2020-11-06"), by = 1 ), ylim1 = c(-0.15,0.15), ylim2 = c(-1,1)
         ,nxbreaks = 30, yleg1 = "DJIA", yleg2 =  "Índice de polaridad_1 ", ylab1 = "", ylab2 = "")


# MAX I MINS DIFERENTS PER LA INTERPOLACIÓ! CORRETGIR GRÀFICS INICIALS!

#ASSUNCIONES

jarque.bera.test(df.DJIA$ratio.neg.scaled[!is.na(df.DJIA$ratio.neg.scaled)])
hist(df.DJIA$return.scaled)

