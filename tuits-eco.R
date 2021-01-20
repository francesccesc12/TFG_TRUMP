#ECO TUITS 

palabras_econ <- c("djia", "dow", "dow jones", "dow jones industrial average", "bearish", "bear market", "best stock", "bullish", "bull market", "finance", "finance news", "financial news", "fiancial market", "long stock", "SP500", "stock", "stock market", "stock decline", "stock fall", "market crash", "stock market news", "market today", "stock price", "to buy", "wall street", "news today", "unemployment", "economy", "trade", "tariff", "jobs", "SP")

load("tweets17074.Rdata")

tweets$text <- tolower(tweets$text)

library(stringr)
economic_tweets<-tweets[str_detect(tweets$text, palabras_econ),]
head(economic_tweets$text)


#SENTIMENT ANALYSIS TUITS ECO! [EN PROCESO]

palabras_econ <- c("djia", "dow", "dow jones", "dow jones industrial average", "bearish", "bear market",
                   "best stock", "bullish", "bull market", "finance", "finance news", "financial news", "fiancial market",
                   "long stock", "SP500", "stock", "stock market", "stock decline", "stock fall", "market crash",
                   "stock market news", "market today", "stock price", "to buy", "wall street", "news today",
                   "unemployment", "economy", "trade", "tariff", "jobs", "SP", "deal", "money", "industry", "companies", 
                   "fed", "dollar", "inflation", "tax","taxes", "oil", "economic", "economy", "business", "subsidies", "inflation", "tariffs")

tweets$text <- tolower(tweets$text)
economic_tweets<-tweets[str_detect(tweets$text, palabras_econ),]

library(xlsx)
write.xlsx(economic_tweets, file = "economic_tweets.xlsx")


#################

library(readxl)
library(lubridate) # working with times and dates

eco_df <- read_excel("economic_tweets.xlsx") #NYEEEEEE

load("DJIA_df.Rdata")

eco_df <- economic_tweets

eco_df <- eco_df[eco_df$created_at < "2020-11-01",]

eco_df[40,]$text 
eco_df[73,]#TWEET ON TARRIFS

#temporal vars

eco_df <- eco_df %>%
  mutate(
         est_timestamp = with_tz(utc_timestamp, tz = "US/Eastern"))

eco_df <- eco_df %>% mutate(anyo = year,
                            mes = month,
                            anyo_mes = ymd(paste(anyo, mes,day,  sep="-"),truncated=2))


article.estimation <- function(num_tweet, eco_df, df){
  
 event_window <- abnormal(40,eco_df, DJIA.x, 6, 6)[[2]]
  
 estimation <- seq(from = eco_df[40,]$anyo_mes - 271, to =  eco_df[40,]$anyo_mes - 21, by = 1)
 
 estimation_window <-  mean(df[df$dia %in% estimation,]$return, na.rm = TRUE)

 t <- event_window  / sd(df[df$dia %in% estimation,]$return, na.rm = TRUE)
}
  
  


abnormal <- function(num_tweet,eco_df,df, days_before, days_after, estimation_window){
  
  if(eco_df[num_tweet,]$hour >= 21 ){ #tanca a les 9 utc (hora anglesa) ---> aquell dia compte 
    
    dates <- seq(from = eco_df[num_tweet,]$anyo_mes - days_before + 1, to = eco_df[num_tweet,]$anyo_mes, by = 1) 
    #dia inclos
    
    efecte <- seq(from = eco_df[num_tweet,]$anyo_mes+1, to = eco_df[num_tweet,]$anyo_mes + days_after, by = 1)
    
    abnormal <- vector()
    caar <- vector()
    
    
    for (i in 1:(days_after + days_before)){
      abnormal[i] <- df[df$dia == eco_df[num_tweet,]$anyo_mes - days_after + i,]$return  - mean(df[df$dia %in% dates | df$dia %in% efecte,]$return, na.rm = TRUE)
      caar[i] <- sum(abnormal)
    }
    
    
    
  }  else {
    
    dates <- seq(from = eco_df[num_tweet,]$anyo_mes - days_before , to = eco_df[num_tweet,]$anyo_mes -1, by = 1)
    #dia NO inclos
    
    efecte <- seq(from = eco_df[num_tweet,]$anyo_mes, to = eco_df[num_tweet,]$anyo_mes + days_after - 1, by = 1)
    
    abnormal <- vector()
    caar <- vector()
    
    for (i in 1:(days_after + days_before)){
      abnormal[i] <- df[df$dia == (eco_df[num_tweet,]$anyo_mes - days_before + i - 1),]$return - mean(df[df$dia %in% dates | df$dia %in% efecte,]$return, na.rm = TRUE)
      caar[i] <- sum(abnormal)
    }
    
    estimation <- seq(from = eco_df[num_tweet,]$anyo_mes - estimation_window - 10, to =  eco_df[num_tweet,]$anyo_mes - 10 - 1, by = 1)
    
    sd1 <- sd(df[df$dia %in% estimation,]$return, na.rm = TRUE)
    
    t <- abnormal  / sd(df[df$dia %in% estimation,]$return, na.rm = TRUE)
    
    t_caar <- caar / (sqrt(estimation_window +1)*sd(df[df$dia %in% estimation,]$return, na.rm = TRUE))
    
    
  } 
  
  return(list(t.test(df[df$dia %in% dates,]$return, df[df$dia %in% efecte,]$return), abnormal, caar, t, t_caar, sd1))
  
}
  imp <- vector()
  for(i in 2:nrow(eco_df)){
  imp[i] <-abnormal(i,eco_df, DJIA.x, 10, 10,250)[[1]][["p.value"]]
  }
  
  imp_t <- vector()
  for(i in 2:nrow(eco_df)){
    imp_t[i] <-max(abs(abnormal(i,eco_df, DJIA.x, 10, 10, 250)[[4]]))
  }
  
  #MIRAR QUE PASA AMB AQUEST TUITS DE CONGRESMAN
  
  eco_df[intersect(which(imp_t > 2),which(imp < 0.05)),]$text
  
  
  #49 I 91! 
  eco_df[which(imp < 0.05),]$text
  
  imp_DJIA <- eco_df[which(imp < 0.05),c("text","est_timestamp")]
  write.xlsx(imp_DJIA, file = "imp_DJIA_2DIES.xlsx")
  
  
  eco_df[91,]$text
  eco_df[91,]$est_timestamp
  eco_df[91,]$hour
  
  
  abnormal(18,eco_df, DJIA.x, 6, 6)
  
  abnormal(91,eco_df, DJIA.x, 10, 10, 250)
  
  #CREC QUE TWITTER HORA DE ESPAÑA
  
  #DIBUIXAR BE SI DIA IN O OUT 
plot(abnormal(49,eco_df, DJIA.x, 10, 10,250) # 0 ABANS D'EFECTES, PER TANT FINS 6
[[2]], type = "l", ylab = "Abnormal Returns", xaxt = "n", xlab = "Event Study", font.lab = 2, col = "steelblue", ylim = c(-0.02,0.03))
grid()
axis(1, at =seq(1,21),  labels = (-days_after): (days_before), cex.axis=0.80, srt=45, col.ticks = "grey", las=1)
abline(h=0, col = "red", lty = 2)
abline(v=11, col = "black", lty = 3, lwd = 2)


plot(abnormal(91,eco_df, DJIA.x, 10, 10, 250) # 0 DINS EFECTES, PER TANT FINS 5
     [[2]], type = "l", ylab = "Abnormal Returns", xaxt = "n", xlab = "Event Study", font.lab = 2, col = "steelblue")
grid()
axis(1, at =seq(1,12),  labels = (-days_after): (days_before-1), cex.axis=0.80, srt=45, col.ticks = "grey", las=1)
abline(h=0, col = "red", lty = 2)
abline(v=7, col = "black", lty = 3, lwd = 2) #cuadrar aixo tmb!

require(png)
img<-readPNG("tuit1.png")
img2 <- readPNG("tuit2.png")

plot(1:10,ty="n")
#specify the position of the image through bottom-left and top-right coords
rasterImage(img,2,2,4,4)

rasterImage(img, 0,0,0,0)

#DIA 0 NO EN EFECTE!
plot(abnormal(49,eco_df, DJIA.x, 10, 10,250)
     [[3]], type = "l", ylab = "Cumulative Abnormal Returns", xaxt = "n", xlab = "Event Study", ylim = c(-0.1, 0.01), font.lab = 2, col = "steelblue")
grid()
axis(1, at =seq(1,21),  labels = (-days_after): days_before, cex.axis=0.80, srt=45, col.ticks = "grey", las=1)
abline(v=11, col = "black", lty = 3, lwd = 2)
rasterImage(img2, 3,-0.027,7,-0.043)


# DIA 0 EN EFECTE!  
plot(abnormal(49,eco_df, DJIA.x, 10, 10,250)
     [[3]], type = "l", ylab = "Cumulative Average Abnormal Returns", xaxt = "n", xlab = "Event Study", font.lab = 2, col = "steelblue")
grid()
axis(1, at =seq(1,12),  labels = (-days_after): (days_before-1), cex.axis=0.80, srt=45, col.ticks = "grey", las=1)
abline(v=7, col = "black", lty = 3, lwd = 2)
rasterImage(img2, 8,-0.009,12,-0.005)

####### 


# FER AMR INTERSECT 49! 

abnormal(49,eco_df, DJIA.x, 10, 10,250) 


options(scipen =999)

pt(abs(abnormal(91,eco_df, DJIA.x, 10, 10,250)[[4]]), 248, lower.tail = F)


sd1 <- abnormal(91,eco_df, DJIA.x, 10, 10,250)[[6]]

sum(abnormal(91,eco_df, DJIA.x, 10, 10,250)[[2]][1:20])

car_91 <- c(0.0659,0.0145,0.0162,0.0143, 0.0085,0.0025,-0.0037,-0.0291,0)

car_49 <- c(-0.0850, 0.0201, 0.0470, 0.0680, 0.0601, 0.0664,0)

car_91[8] / ((8^(1/2))*sd1)

car_t_91 <- c(3.5, 1.72, 1.57, 1.20, 0.63, 0.17,-0.23, -1.72, 0)

car_t_49 <- c(-3.86, 2.04, 3.90, 4.88, 3.86, 3.89, 0)

pt(abs(car_t_91), df = 248, lower.tail = F)



#taula 6.3 

caar1 <- abnormal(49,eco_df, DJIA.x, 6, 6,250)[[3]][6] 
caar2 <- abnormal(49,eco_df, DJIA.x, 6, 6,250)[[3]][8] - abnormal(49,eco_df, DJIA.x, 6, 6,250)[[3]][7]  
caar3 <- abnormal(49,eco_df, DJIA.x, 6, 6,250)[[3]][9] - abnormal(49,eco_df, DJIA.x, 6, 6,250)[[3]][7]  
caar4 <- abnormal(49,eco_df, DJIA.x, 6, 6,250)[[3]][10] - abnormal(49,eco_df, DJIA.x, 6, 6,250)[[3]][7] 
caar5 <- abnormal(49,eco_df, DJIA.x, 6, 6,250)[[3]][11] - abnormal(49,eco_df, DJIA.x, 6, 6,250)[[3]][7]  
caar7 <- caar4 <- abnormal(49,eco_df, DJIA.x, 6, 6,250)[[3]][12] 

estimation <- seq(from = eco_df[49,]$anyo_mes - 271, to =  eco_df[40,]$anyo_mes - 21, by = 1)


denominador <- sqrt(250 +1)*sd(df[df$dia %in% estimation,]$return, na.rm = TRUE)


 caar5 / denominador
