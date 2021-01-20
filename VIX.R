#VIX

library(quantmod) #indicesbursailtes
library(lmtest) #grangertest
library(tseries) #adf test
library(vars) #var model


#CARGAMOS INDICES DE SENTIMIENTO
load("index.pol.scales.RData")
load("ratio.neg.scales.Rdata")
load("ratio.pos.scales.Rdata")
load("index.pol2.scaled.RData")
load("prueba2.Rdata")
load("VIX.total.Rdata")

#CARGAMOS INFO DIFERENTES INDICES BURSATILES: 
Vix <- getSymbols("^VIX", src = "yahoo", from = "2017-01-16", to = "2020-11-07") #NO INCLOU ELS EXTREMS

#Nos quedamos con el close price de todas 
VIX.c <- VIX[,4]

# añadimos valores del fin de semana por interpolación: 
VIX.x <- merge(VIX.c, zoo(,seq(start(VIX.c),end(VIX.c),by="day")), all=TRUE)
VIX.x$inter <- ifelse(is.na(VIX.x), 1, 0)
VIX.x <- na.approx(VIX.x)


# Calculamos los log-returns (Rendimientos)
VIX.x$return <- ROC(VIX.x$VIX.Close,n=1,type="discrete") 
VIX.x <- VIX.x[- which(is.na(VIX.x$return)),]

VIX.return <- ROC(VIX.c$VIX.Close,n=1,type="discrete") 

# Escalamos también las variables economicas: 
VIX.x$return.scaled <- scale(VIX.x$return)

VIX.x <- as.data.frame(VIX.x)
VIX.x$dia <- as.Date(rownames(VIX.x))

VIX.total <- merge(VIX.x, prueba2, by.x = "dia", by.y = "anyo_mes", all.x = TRUE)

# Variable binaria 1 si la bolsa sube 0 si baja 
VIX.total$binary <- ifelse( VIX.total$return > 0, 1, 0)

VIX.total$return_scaled <- scale(VIX.total$return)

save(VIX.total, file = "VIX.total.Rdata")

df.VIX <- VIX.total

# alguns grafics 

plot(scale(VIX.c))

#####################

plot(rollmean(df.VIX$return.scaled, k = 10, fill=NA), type = "l", xaxt ="n", ylab = "VIX y pol.index2 escalados", font.lab = 2)
lines(rollmean(df.VIX$index.pol2.scaled, k =30 , fill = NA, na.rm = TRUE), col = "green", lwd = 2)
grid(ny=7,nx=16)
axis(1, at = seq(1,1389,90), labels = strftime(df.VIX$dia[seq(1,1389,90)],"%Y-%m") , cex.axis=0.60, srt=45, col.ticks = "grey", las=2)
legend(x = 410, y = 2.1,legend = c("VIX 10-días media móvil", "Índice de polaridad_2 30-días media móvil "), fill = c("black", "green"), cex = 0.7)

#######################

plot(rollmean(df.VIX$return.scaled, k = 10, fill=NA), type = "l", xaxt ="n", ylab = "VIX y pol.index1 escalados", font.lab = 2)
lines(rollmean(df.VIX$index.pol.scaled, k =30 , fill = NA, na.rm = TRUE), col = "green", lwd = 2)
grid(ny=7,nx=16)
axis(1, at = seq(1,1389,90), labels = strftime(df.VIX$dia[seq(1,1389,90)],"%Y-%m") , cex.axis=0.60, srt=45, col.ticks = "grey", las=2)
legend(x = 410, y = 2.1,legend = c("VIX 10-días media móvil", "Índice de polaridad_1 30-días media móvil "), fill = c("black", "green"), cex = 0.7)

#######################

plot(rollmean(df.VIX$return.scaled, k = 10, fill=NA), type = "l", xaxt ="n", ylab = "VIX y Ratio Negatividad escalados", font.lab = 2)
lines(rollmean(df.VIX$ratio.neg.scaled, k =30 , fill = NA, na.rm = TRUE), col = "red", lwd = 2)
grid(ny=7,nx=16)
axis(1, at = seq(1,1389,90), labels = strftime(df.VIX$dia[seq(1,1389,90)],"%Y-%m") , cex.axis=0.60, srt=45, col.ticks = "grey", las=2)
legend(x = 410, y = 2.1,legend = c("VIX 10-días media móvil", "Ratio de Negatividad 30-días media móvil "), fill = c("black", "red"), cex = 0.7)

########################

plot(rollmean(df.VIX$return.scaled, k = 10, fill=NA), type = "l", xaxt ="n", ylab = "VIX y Ratio Positividad escalados", font.lab = 2)
lines(rollmean(df.VIX$ratio.pos.scaled, k =30 , fill = NA, na.rm = TRUE), col = "blue", lwd = 2)
grid(ny=7,nx=16)
axis(1, at = seq(1,1389,90), labels = strftime(df.VIX$dia[seq(1,1389,90)],"%Y-%m") , cex.axis=0.60, srt=45, col.ticks = "grey", las=2)
legend(x = 410, y = 2.1,legend = c("VIX 10-días media móvil", "Ratio Positividad 30-días media móvil "), fill = c("black", "blue"), cex = 0.7)

# grangertest ESCALANT O SENSE ESCALAR? 

# BAYESIAN APPROACH TO GRANGER CAUSALITY?

pvalor <- function(v1,v2,n){
  v <- vector()
for (i in 1:n){
  v[i] <- grangertest(x = v1, y= v2, order = i)[2,4]
  
}
  return(v)
}

pvalor(df.VIX$ratio_pos, df.VIX$return,10)
pvalor( df.VIX$index.pol2,df.VIX$binary,10) #ABSOLUTAMENTE NADA

# CORRECIÓN BONFERRONI? 

grangertest(df.VIX$index.pol.scaled, df.VIX$return.scaled, order = 8) #SIGNIFICATIU AL 10!
grangertest(df.VIX$index.pol.scaled, df.VIX$return.scaled, order = 6)
grangertest(df.VIX$index.pol, df.VIX$return, order = 6)


grangertest(df.VIX$index.pol2.scaled, df.VIX$return.scaled, order = 6)
grangertest(df.VIX$index.pol2, df.VIX$return, order = 6)

grangertest(df.VIX$ratio.neg.scaled, df.VIX$return.scaled, order = 8)

grangertest(df.VIX$ratio.pos.scaled, df.VIX$return.scaled, order = 6)


# lm 

lm2 <- lm(df.VIX$return.scaled ~ lag(df.VIX$return.scaled) + lag(df.VIX$return.scaled, n = 2 ) + lag(df.VIX$return.scaled, n =3) + lag(df.VIX$return.scaled, n = 4) + lag(df.VIX$return.scaled, n = 5)
         + lag(df.VIX$return.scaled, n = 6 ) + lag(df.VIX$index.pol2.scaled, n = 2) + lag(df.VIX$index.pol2.scaled, n = 3) 
         + lag(df.VIX$index.pol2.scaled, n = 4) + lag(df.VIX$index.pol2.scaled, n = 5) +  lag(df.VIX$index.pol2.scaled, n = 6))

summary(lm2)


plot(y = df.VIX$return.scaled, x = lag(df.VIX$index.pol2.scaled, n = 6), ylim = c(-4,8), xlim = c(-4,10), ylab = "VIX returns escalados", xlab = "Ind. Pol_2 escalado", pch = 16, font.lab = 2)
abline(a = lm2$coefficients[1], lm2$coefficients[12], col = "red", lty = 2)
text(6, 4, paste0("VIX.returns = ", round(lm2$coefficients[1],3), " + ", round(lm2$coefficients[12],3), "*Ind.Pol_2"), col= "red", cex = 0.8)

######

df.VIX_2017 <- df.VIX[df.VIX$dia < "2018-01-01",]
df.VIX_2018 <- df.VIX[df.VIX$dia > "2017-12-31" & df.VIX$dia < "2019-01-01",]
df.VIX_2019 <- df.VIX[df.VIX$dia > "2018-12-31" & df.VIX$dia < "2020-01-01",]
df.VIX_2020 <- df.VIX[df.VIX$dia > "2019-12-31",]


pvalor(df.VIX_2017$ratio_pos, df.VIX_2017$return, 10) # NADA
pvalor(df.VIX_2017$ratio_pos, df.VIX_2017$binary, 10) # NADA


pvalor(df.VIX_2018$index.pol2, df.VIX_2018$return, 10) #NADA
pvalor(df.VIX_2018$index.pol2, df.VIX_2018$binary, 14) #

pvalor(df.VIX_2019$index.pol2, df.VIX_2019$return, 10) #NADA
pvalor(df.VIX_2019$index.pol2, df.VIX_2019$binary, 14) #NADA

pvalor(df.VIX_2020$ratio_neg, df.VIX_2020$return, 10) #NADA
pvalor(df.VIX_2020$index.pol2, df.VIX_2020$binary, 10) #NADA

#desviacions

load("volatilidad_tuits.Rdata")

volatilidad <- prueba2_volatilidad[4:1365,]

VIX.total <- merge(VIX.total, volatilidad, by.x = "dia", by.y = "anyo_mes", all.x =TRUE)


plot(volatilidad[!is.na(volatilidad$sentimiento_promedio_sd),]$sentimiento_promedio_sd,  
     df.VIX[df.VIX$dia %in% volatilidad[!is.na(volatilidad$sentimiento_promedio_sd),]$anyo_mes,]$return)



pvalor(df.VIX$sentimiento_promedio_sd, df.VIX$return, 10)
pvalor(df.VIX_2017$sentimiento_promedio_sd, df.VIX_2017$return, 10)
pvalor(df.VIX_2018$sentimiento_promedio_sd, df.VIX_2018$return, 10)
pvalor(df.VIX_2019$sentimiento_promedio_sd, df.VIX_2019$return, 10)
pvalor(df.VIX_2020$sentimiento_promedio_sd, df.VIX_2020$return, 10)



pvalor(df.VIX$sentimiento_promedio_sd, df.VIX$binary, 10)

pvalor(df.VIX$sentimiento_promedio_mean, df.VIX$binary, 10)


pvalor(df.VIX_2017$sentimiento_promedio_mean, df.VIX_2017$return, 10)
pvalor(df.VIX_2018$sentimiento_promedio_mean, df.VIX_2018$return, 10)
pvalor(df.VIX_2019$sentimiento_promedio_mean, df.VIX_2019$return, 10)
pvalor(df.VIX_2020$sentimiento_promedio_mean, df.VIX_2020$return, 10)

