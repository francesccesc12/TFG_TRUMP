#SP 500

library(quantmod) #indicesbursailtes
library(lmtest) #grangertest
library(tseries) #adf test
library(vars) #var model


#CARGAMOS INDICES DE SENTIMIENTO
load("index.pol.scales.RData")
load("ratio.neg.scales.Rdata")
load("ratio.pos.scales.Rdata")
load("index.pol.scales.RData")
load("prueba2.Rdata")
load("SP.total.Rdata")


#CARGAMOS INFO DIFERENTES INDICES BURSATILES: 
SP <- getSymbols("^GSPC", src = "yahoo", from = "2017-01-16", to = "2020-11-07") #NO INCLOU ELS EXTREMS

#Nos quedamos con el close price de todas 
SP.c <- GSPC[,4]

# añadimos valores del fin de semana por interpolación: 
SP.x <- merge(SP.c, zoo(,seq(start(SP.c),end(SP.c),by="day")), all=TRUE)
SP.x$inter <- ifelse(is.na(SP.x), 1, 0)
SP.x <- na.approx(SP.x)


# Calculamos los log-returns (Rendimientos)
SP.x$return <- ROC(SP.x$GSPC.Close,n=1,type="discrete") 
SP.x <- SP.x[- which(is.na(SP.x$return)),]


SP.returns <- ROC(SP.c$GSPC.Close,n=1,type="discrete") 

# Escalamos también las variables economicas: 
SP.x$return.scaled <- scale(SP.x$return)

SP.x <- as.data.frame(SP.x)
SP.x$dia <- as.Date(rownames(SP.x))

SP.total <- merge(SP.x, prueba2, by.x = "dia", by.y = "anyo_mes", all.x = TRUE)

# Variable binaria 1 si la bolsa sube 0 si baja 
SP.total$binary <- ifelse( SP.total$return > 0, 1, 0)

SP.total$return.scaled <- scale(SP.total$return)

save(SP.total, file = "SP.total.Rdata")

df.SP <- SP.total

# alguns grafics 

plot(scale(SP.c))

#####################

plot(rollmean(df.SP$return.scaled, k = 10, fill=NA), type = "l", xaxt ="n", ylab = "SP y pol.index2 escalados",xlab = "", font.lab = 2)
lines(rollmean(df.SP$index.pol2.scaled, k =30 , fill = NA, na.rm = TRUE), col = "green", lwd = 2)
grid(ny=7,nx=16)
axis(1, at = seq(1,1389,90), labels = strftime(df.SP$dia[seq(1,1389,90)],"%Y-%m") , cex.axis=0.60, srt=45, col.ticks = "grey", las=2)
legend(x = 150, y = -1.5,legend = c("SP 10-días media móvil", "Índice de polaridad_2 30-días media móvil "), fill = c("black", "green"), cex = 0.7)

#######################

plot(rollmean(df.SP$return.scaled, k = 10, fill=NA), type = "l", xaxt ="n", xlab = "", ylab = "SP y pol.index1 escalados", font.lab = 2)
lines(rollmean(df.SP$index.pol.scaled, k =30 , fill = NA, na.rm = TRUE), col = "green", lwd = 2)
grid(ny=7,nx=16)
axis(1, at = seq(1,1389,90), labels = strftime(df.SP$dia[seq(1,1389,90)],"%Y-%m") , cex.axis=0.60, srt=45, col.ticks = "grey", las=2)
legend(x = 150, y = -1.5,legend = c("SP 10-días media móvil", "Índice de polaridad_1 30-días media móvil "), fill = c("black", "green"), cex = 0.7)

#######################

plot(rollmean(df.SP$return.scaled, k = 10, fill=NA), type = "l", xaxt ="n", xlab = "", ylab = "SP y Ratio Negatividad escalados", font.lab = 2)
lines(rollmean(df.SP$ratio.neg.scaled, k =30 , fill = NA, na.rm = TRUE), col = "red", lwd = 2)
grid(ny=7,nx=16)
axis(1, at = seq(1,1389,90), labels = strftime(df.SP$dia[seq(1,1389,90)],"%Y-%m") , cex.axis=0.60, srt=45, col.ticks = "grey", las=2)
legend(x =  150, y = -1.5,legend = c("SP 10-días media móvil", "Ratio de Negatividad 30-días media móvil "), fill = c("black", "red"), cex = 0.7)

########################

plot(rollmean(df.SP$return.scaled, k = 10, fill=NA), type = "l", xaxt ="n", xlab = "", ylab = "SP y Ratio Positividad escalados", font.lab = 2)
lines(rollmean(df.SP$ratio.pos.scaled, k =30 , fill = NA, na.rm = TRUE), col = "blue", lwd = 2)
grid(ny=7,nx=16)
axis(1, at = seq(1,1389,90), labels = strftime(df.SP$dia[seq(1,1389,90)],"%Y-%m") , cex.axis=0.60, srt=45, col.ticks = "grey", las=2)
legend(x =  150, y = -1.5,legend = c("SP 10-días media móvil", "Ratio Positividad 30-días media móvil "), fill = c("black", "blue"), cex = 0.7)

# grangertest ESCALANT O SENSE ESCALAR? 

pvalor <- function(v1,v2,n){
  v <- vector()
  for (i in 1:n){
    v[i] <- grangertest(x=v1, y=v2, order = i)[2,4]
    
  }
  return(v)
}

pvalor( df.SP$index.pol2,df.SP$return,10)

pvalor(df.SP$ratio_pos, df.SP$binary,10) #0.14 amb ind1; 0.12 ratio_neg; [6-8 dia]


df.SP <- SP.total

grangertest(df.SP$index.pol.scaled, df.SP$return.scaled, order = 8) #SIGNIFICATIU AL 10!
grangertest(df.SP$index.pol.scaled, df.SP$return.scaled, order = 6)
grangertest(df.SP$index.pol, df.SP$return, order = 6)


grangertest(df.SP$index.pol2.scaled, df.SP$return.scaled, order = 6)
grangertest(df.SP$index.pol2, df.SP$return, order = 6)

grangertest(df.SP$ratio.neg.scaled, df.SP$return.scaled, order = 8)

grangertest(df.SP$ratio.pos.scaled, df.SP$return.scaled, order = 6)


# lm 

lm2 <- lm(df.SP$return.scaled ~ lag(df.SP$return.scaled) + lag(df.SP$return.scaled, n = 2 ) + lag(df.SP$return.scaled, n =3) + lag(df.SP$return.scaled, n = 4) + lag(df.SP$return.scaled, n = 5)
          + lag(df.SP$return.scaled, n = 6 ) + lag(df.SP$index.pol2.scaled, n = 2) + lag(df.SP$index.pol2.scaled, n = 3) 
          + lag(df.SP$index.pol2.scaled, n = 4) + lag(df.SP$index.pol2.scaled, n = 5) +  lag(df.SP$index.pol2.scaled, n = 6))

summary(lm2)


plot(y = df.SP$return.scaled, x = lag(df.SP$index.pol2.scaled, n = 6), ylim = c(-4,8), xlim = c(-4,10), ylab = "SP returns escalados", xlab = "Ind. Pol_2 escalado", pch = 16, font.lab = 2)
abline(a = lm2$coefficients[1], lm2$coefficients[12], col = "red", lty = 2)
text(6, 4, paste0("SP.returns = ", round(lm2$coefficients[1],3), " + ", round(lm2$coefficients[12],3), "*Ind.Pol_2"), col= "red", cex = 0.8)



############### FINNNN

# Calculamos los log-returns (Rendimientos)
SP.return <- ROC(SP.c,n=1,type="discrete") 
SP.return <- SP.return[c(- which(is.na(SP.return)))]


# Escalamos también las variables economicas: 
SP.return.scaled <- as.data.frame(scale(SP.return))

# Variable binaria 1 si la bolsa sube 0 si baja 
binary.SP <- ifelse( diff(GSPC[,4], lag = 1) > 0, 1, 0)

#Variable dels dies que apareixen a twitter i a serie financera 
#ESTEM PERDENT LA INFO DE TOTS ELS DIES QUE SON FINDE!
dies <- seq.Date(from = as.Date("2017-01-20"), to = as.Date("2020-11-04"), by = 1)
date <- prueba2$anyo_mes

dies_tuits <- dies[dies %in% date]
dies_eco <- dies[as.character(dies) %in%  row.names(SP.return.scaled)]
dies_totals <- dies_tuits[dies_tuits %in% dies_eco]

#contruim SP500 Dataframe
df.SP <- data.frame(dies_totals = dies_totals)
df.SP$sentiment <-index.pol.scaled[which(date %in% dies_totals)]
df.SP$sentiment2 <- index.pol2.scaled[which(date %in% dies_totals)]
df.SP$eco <- SP.return.scaled[which(row.names(SP.return.scaled) %in% as.character(dies_totals)),]
df.SP$binary <- ifelse(df.SP$eco > 0, 1, 0)
df.SP$negative <- ratio.neg.scaled[which(date %in% dies_totals)]
df.SP$positive <- ratio.pos.scaled[which(date %in% dies_totals)]

save(df.SP, file ="df.SP.RData")

# primers grafics 

plot(rollmean(df.SP$eco, k = 30, fill=NA), type = "l", xaxt ="n")
lines(rollmean(df.SP$negative, k = 90, fill = NA), col = "red")
lines(rollmean(df.SP$positive, k = 90, fill = NA), col = "blue")
abline(h=0, lty =2, col = "red")
axis(1, at = seq(from = 1, to = 913, by = 30), labels = dies_totals[seq(from = 1, to = 913, by = 30)], cex.axis=0.60, srt=45, col.ticks = "grey", las=2)


plot(rollmean(df.SP$eco, k = 30, fill = NA), type = "l", xaxt = "n") #MILLOR PINTA
lines(rollmean(df.SP$sentiment, k =90, fill = NA), type = "l", col = "green")
abline(h=0, lty =2, col = "red")
axis(1, at = seq(from = 1, to = 913, by = 30), labels = dies_totals[seq(from = 1, to = 913, by = 30)], cex.axis=0.60, srt=45, col.ticks = "grey", las=2)

plot(rollmean(df.SP$eco, k = 30, fill = NA), type = "l", xaxt = "n")
lines(rollmean(df.SP$sentiment2, k =90, fill = NA), type = "l", col = "green")
abline(h=0, lty =2, col = "red")
axis(1, at = seq(from = 1, to = 913, by = 30), labels = dies_totals[seq(from = 1, to = 913, by = 30)], cex.axis=0.60, srt=45, col.ticks = "grey", las=2)

# PERIODE 2017: 


df.SP.2017 <- df.SP[df.SP$dia < "2018-01-01",]

plot(rollmean(scale(df.SP.2017$eco), k = 10, fill=NA),ylim = c(-1,1), type = "l", xaxt ="n")
lines(rollmean(scale(df.SP.2017$negative), k = 10, fill = NA), col = "red")
lines(rollmean(df.SP.2017$positive, k = 10, fill = NA), col = "blue")
abline(h=0, lty =2, col = "red")
axis(1, at = seq(from = 1, to = 233, by = 30), labels = dies_totals[seq(from = 1, to = 233, by = 30)], cex.axis=0.60, srt=45, col.ticks = "grey", las=2)


grangertest(df.SP.2017$sentiment, df.SP.2017$eco, order = 4)
grangertest(df.SP.2017$sentiment, df.SP.2017$binary, order = 4)

grangertest(df.SP.2017$index.pol2, df.SP.2017$binary, order = 6) #SIGNIFICATIU AL 10!
pvalor(df.SP.2017$index.pol2, df.SP.2017$return, 10)




plot(rollmean(scale(df.SP.2017$eco), k = 30, fill=NA),ylim = c(-1,1), type = "l", xaxt ="n")
lines(rollmean(scale(df.SP.2017$sentiment2), k = 30, fill = NA), col = "green")
abline(h=0, lty =2, col = "red")
axis(1, at = seq(from = 1, to = 233, by = 30), labels = dies_totals[seq(from = 1, to = 233, by = 30)], cex.axis=0.60, srt=45, col.ticks = "grey", las=2)


#ABANS DE LM MIRAR QUE LES DUES SERIES SIGUIN ESTACIONARIES?

lm1 <- lm(df.SP.2017$eco ~ lag(df.SP.2017$eco) + lag(df.SP.2017$eco, n = 2 ) + lag(df.SP.2017$eco, n =3) + lag(df.SP.2017$eco, n = 4) 
          + df.SP.2017$sentiment2 + lag(df.SP.2017$sentiment2, n = 2) + lag(df.SP.2017$sentiment2, n = 3) + lag(df.SP.2017$sentiment2, n = 4))
summary(lm1)

plot(lm1)

# DIA ANTERIOR Y 4 DIES ABANS!

plot(df.SP.2017$eco, lag(df.SP.2017$sentiment2, n = 4), xlab = "eco", ylab = "sent", pch = 16)




# Check Homocedasticiy of the residuals: 

bptest(lm1) #model okey 

# Check for normal residuals 

shapiro.test(resid(lm1))
qqnorm(resid(lm1))


####

#DIVIDIM EN ANYS:

df.SP <- SP.total

df.SP_2017 <- df.SP[df.SP$dia < "2018-01-01",]
df.SP_2018 <- df.SP[df.SP$dia > "2017-12-31" & df.SP$dia < "2019-01-01",]
df.SP_2019 <- df.SP[df.SP$dia > "2018-12-31" & df.SP$dia < "2020-01-01",]
df.SP_2020 <- df.SP[df.SP$dia > "2019-12-31",]


pvalor(df.SP_2017$index.pol2, df.SP_2017$return, 10) 
pvalor(df.SP_2017$index.pol2, df.SP_2017$binary, 10) # NADA


pvalor(df.SP_2018$ratio_pos, df.SP_2018$return, 10) #NADA
pvalor(df.SP_2018$index.pol, df.SP_2018$binary, 14) # INDEX POL 1 DIA 7+10+11// RATIO NEG 7 + 10 + 11 //RATIO POS IGUAL!


logit2 <- glm(df.SP_2018$binary ~ lagpad(df.SP_2018$binary,1) + lagpad(df.SP_2018$binary,2 ) + lagpad(df.SP_2018$binary, 3) + lagpad(df.SP_2018$binary,  4) + lagpad(df.SP_2018$binary,  5)
          + lagpad(df.SP_2018$binary,  6 ) + lagpad(df.SP_2018$binary,  7) + lagpad(df.SP_2018$binary,  8) + lagpad(df.SP_2018$binary,  9) + lagpad(df.SP_2018$binary,  10) +  lagpad(df.SP_2018$ratio_neg,  1) +lagpad(df.SP_2018$ratio_neg,  2) + lagpad(df.SP_2018$ratio_neg,  3) 
          + lagpad(df.SP_2018$ratio_neg,  4) + lagpad(df.SP_2018$ratio_neg,  5) +  lagpad(df.SP_2018$ratio_neg,  6) +  lagpad(df.SP_2018$ratio_neg,  7) +  lagpad(df.SP_2018$ratio_neg,  8) + lagpad(df.SP_2018$ratio_neg,  9) + lagpad(df.SP_2018$ratio_neg,  10), family=binomial(link='logit'))


logit2 <- glm(df.SP_2018$binary ~ lagpad(df.SP_2018$binary,1) + lagpad(df.SP_2018$binary,2 ) + lagpad(df.SP_2018$binary, 3) + lagpad(df.SP_2018$binary,  4) + lagpad(df.SP_2018$binary,  5)
              + lagpad(df.SP_2018$binary,  6 ) + lagpad(df.SP_2018$binary,  7) + lagpad(df.SP_2018$binary,  8) + lagpad(df.SP_2018$binary,  9) + lagpad(df.SP_2018$binary,  10) 
              + lagpad(df.SP_2018$ratio_neg,  7)  + lagpad(df.SP_2018$ratio_neg,  10), family=binomial(link='logit'))


summary(logit2)

grangertest(df.SP_2018$ratio_neg,df.SP_2018$binary, order = 7)

sink(file = "output3.txt")
summary(logit2)
sink(file = NULL)


logit3 <- glm(df.SP_2018$binary ~ lagpad(df.SP_2018$binary,1) + lagpad(df.SP_2018$binary,2 ) + lagpad(df.SP_2018$binary, 3) + lagpad(df.SP_2018$binary,  4) + lagpad(df.SP_2018$binary,  5)
              + lagpad(df.SP_2018$binary,  6 ) + lagpad(df.SP_2018$binary,  7) + lagpad(df.SP_2018$binary,  8) + lagpad(df.SP_2018$binary,  9) + lagpad(df.SP_2018$binary,  10) +  lagpad(df.SP_2018$ratio.pos.scaled,  1) +lagpad(df.SP_2018$ratio.pos.scaled,  2) + lagpad(df.SP_2018$ratio.pos.scaled,  3) 
              + lagpad(df.SP_2018$ratio.pos.scaled,  4) + lagpad(df.SP_2018$ratio.pos.scaled,  5) +  lagpad(df.SP_2018$ratio.pos.scaled,  6) +  lagpad(df.SP_2018$ratio.pos.scaled,  7) +  lagpad(df.SP_2018$ratio.pos.scaled,  8) + lagpad(df.SP_2018$ratio.pos.scaled,  9) + lagpad(df.SP_2018$ratio.pos.scaled,  10), family=binomial(link='logit'))

summary(logit3)

sink(file = "output4.txt")
summary(logit3)
sink(file = NULL)

logit4 <- glm(df.SP_2018$binary ~ lagpad(df.SP_2018$binary,1) + lagpad(df.SP_2018$binary,2 ) + lagpad(df.SP_2018$binary, 3) + lagpad(df.SP_2018$binary,  4) + lagpad(df.SP_2018$binary,  5)
              + lagpad(df.SP_2018$binary,  6 ) + lagpad(df.SP_2018$binary,  7) + lagpad(df.SP_2018$binary,  8) + lagpad(df.SP_2018$binary,  9) + lagpad(df.SP_2018$binary,  10) +  lagpad(df.SP_2018$index.pol.scaled,  1) +lagpad(df.SP_2018$index.pol.scaled,  2) + lagpad(df.SP_2018$index.pol.scaled,  3) 
              + lagpad(df.SP_2018$index.pol.scaled,  4) + lagpad(df.SP_2018$index.pol.scaled,  5) +  lagpad(df.SP_2018$index.pol.scaled,  6) +  lagpad(df.SP_2018$index.pol.scaled,  7) +  lagpad(df.SP_2018$index.pol.scaled,  8) + lagpad(df.SP_2018$index.pol.scaled,  9) + lagpad(df.SP_2018$index.pol.scaled,  10), family=binomial(link='logit'))

summary(logit4)

sink(file = "output5.txt")
summary(logit4)
sink(file = NULL)

pvalor(df.SP_2019$ratio_pos, df.SP_2019$return, 10) #NADA
pvalor(df.SP_2019$ratio_neg, df.SP_2019$binary, 10) #NADA

pvalor(df.SP_2020$ratio_pos, df.SP_2020$return, 10) #NADA
pvalor(df.SP_2020$ratio_neg, df.SP_2020$binary, 10) #NADA
