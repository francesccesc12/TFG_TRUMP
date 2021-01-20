plot(DJIA.c, data.forma.in = "%Y-%m", colorset=c("cadetblue"), cex.axis = 0.6, xlim = c("2017-01-17, 2020-11-06"))

chart.TimeSeries(return,colorset=c("cadetblue"),	main = "Log-Retornos diarios para el índice DJIA")

plot(DJIA.c$DJI.Close, type = "l", xaxt = "n")

DJIA.x <- DJIA.total
options(scipen = 99)

par(mfrow = c(1,2), font.lab = 2)
grid()
boxplot(as.vector(DJIA.x$return), col = "steelblue", ylab = "Rendimientos.DJIA", ylim= c(-0.15, 0.15))
grid()
abline(h = mean(as.vector(DJIA.x$return)), col = "red", lty = 2)
text(0.65, 0.02, paste0("Media = ",round(mean(as.vector(DJIA.x$return)),4)), col = "red")
hist(DJIA.x$return, col = "steelblue", breaks = 25, xlab = "", ylab = "Frecuencia",xlim = c(-0.15, 0.15), font.lab = 2,ylim = c(0,800), main = "")
grid()
abline(v = mean(as.vector(DJIA.x$return)), col = "red", lty = 2)
text(-0.05, 300, paste0("Media = ",round(mean(as.vector(DJIA.x$return)),4)), col = "red")

fig1 <- barplot(100*prop.table(table(Trump$fuente)), xaxt="n", col = "steelblue", ylab = "%", xlab = "Fuente", ylim = c(0,110), las = 2, cex.names = 0.6, font.lab = 2)
text(fig1, 100*prop.table(table(Trump$fuente)), round(100*prop.table(table(Trump$fuente)),2), pos = 3, col = "darkblue", font = 2)
grid()
labs <- paste(names(table(Trump$fuente)))
text(cex=0.6, x=fig1-.25, y=-12, labs, xpd=TRUE, srt=45)


ggplot(Trump, aes(x = as.Date(fecha), fill = "steelblue", width = 0.25)) +
  geom_histogram(position = "identity", bins = 30, show.legend = FALSE, fill = "steelblue", alpha = 0.75) +
  scale_x_date(date_labels = "%m-%y", date_breaks = "1 month") +
  labs(x = "Mes de publicación", y = "Número de tweets") +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=90, hjust=1))

load("tweets14074.Rdata")
Trump <- tweets

par(mfrow = c(1,2), font.lab = 2, cex.axis = 0.8)
boxplot(Trump$rt, col = "steelblue", main = "", ylim = c(0,2000000), las = 2, xlab = "RT", font.lab = 2)
grid()
boxplot(Trump$fav, col = "steelblue", main = "", ylim = c(0,2000000), las = 2, xlab = "FAV", font.lab = 2)
grid()



dev.off()
plot(Trump$rt, Trump$fav, col = "steelblue", pch = 16, font.lab = 2, xlab = "RT", ylab = "FAV", main = "", las = 1, ylim = c(0,1000000), xlim = c(0,400000))
abline(a = coef(lm(Trump$fav ~ Trump$rt))[1], b = coef(lm(Trump$fav ~ Trump$rt))[2], lty = 2, col = "red")
text(250000, 600000, paste0("Modelo Lineal: FAV = ", round( coef(lm(Trump$fav ~ Trump$rt))[1],0), " + RT*",  round( coef(lm(Trump$fav ~ Trump$rt))[2],2)), col = "red")
text(250000,450000, paste0("Cor(FAV,RT) = ", round(cor(Trump$rt, Trump$fav),2)), col = "red")


fig3 <- plot(month[1:46,2],type = "l", ylim = c(0,1000), col = "steelblue", main = "", xaxt = "n", xlab = "Mes de publicación",  ylab = "Número de tuits", font.lab = 2, las = 2)
grid(nx = 10)
#axis(1, at = seq(1,1348, by = 3), labels = data2[seq(1,1348, by = 3)], cex.axis=0.60, srt=45, col.ticks = "grey", las=2)
axis(1, at =seq(1,46),  labels = month[1:46,1], cex.axis=0.60, srt=45, col.ticks = "grey", las=2)
text(month[1:46,2], label = round(month[1:46,2],0), pos = 3, col = "darkblue")

####

vector <- 1:100

proba1 <- vector()
proba2 <- vector()

for (i in 2:1000){
  
  proba1[i-1] <- log(vector[i]/vector[i-1])
  proba2[i-1] <- (vector[i]-vector[i-1])/vector[i-1]
}

plot(proba1, proba2)
abline(a = 0, b = 1)

ROC(vector)
ROC(vector, type = "discrete")
