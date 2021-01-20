# load required packages
library(readr) # reading and writing delimited text files
library(dplyr) # SQL-style data processing
library(tidytext) # text analysis in R
library(stringr) # working with text strings
library(lubridate) # working with times and dates
library(jsonlite) # reading and writing JSON
library(tidyr) # data reshaping
library(wordcloud) #wordcloud
library(sentimentr) #sentiment analysis
library(stringr) #tuits eco
library(ggplot2) #grafs

options(scipen=999)

# PART 1: LIMPIEZA + WORDCLOUD

#carguem dades
load("/Users/francescsalvador/Desktop/trump/TrumpTweets.Rdata")

#Filtramos para quitar rt y a partir de que es presidente de EEUU
data <- data[data$created_at > "2017-01-16" & data$created_at < "2020-11-07",]
data <- data %>% dplyr::filter(!substr(text,1,2) == "RT")
tweets <- data

# regex for parsing tweets [Elements que treure del twitts: ex: http, ampersand...]
replace_reg <- "https?://[^\\s]+|&amp;|&lt;|&gt;|\bRT\\b"

# create date elements
tweets <- tweets %>%
  mutate(utc_timestamp = ymd_hms(created_at),
         est_timestamp = with_tz(utc_timestamp, tz = "US/Eastern"),
         year = year(est_timestamp),
         month = month(est_timestamp),
         day = day(est_timestamp),
         hour = hour(utc_timestamp))

# selecciones el tuits que fa ell, no els de campaña (aid/media)
tweets <- tweets %>%
  mutate(source2 = case_when(grepl("Ads|Media",source) ~ "aides",
                             TRUE ~ "trump"))
tweets <- tweets[tweets$source2 == "trump",]

#tuits sense RT ni links etc. 
tweets <- tweets %>%
  filter(is_retweet == FALSE & substring(text,1,2) != "RT" & substring(text,1,4) != "http") %>%
  mutate(text = str_replace_all(text, replace_reg, "")) 

save(tweets, file = "tweets14074.Rdata")

# Tokenización (divisió del tuit en paraules) 
#[S'utlitza token = "tweets", per obtenir format tweets;  per mantenir hashtag # i @ simbols. + Convertir tot a minuscula!]
words <- tweets %>%
  filter(is_retweet == FALSE & substring(text,1,2) != "RT" & substring(text,1,4) != "http") %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "tweets")


# remove stop words #NOMES PER VEURE LES PARAULES MÉS UTILITZADES! 
#PER L'ANÀLISI SENTIMENTAL NO LES TREUREM PERQUE HI HAN NEGACIONS!
words <- words %>%
  anti_join(stop_words, by = "word")


# bivariate, juntem en grups de 2 paraules!
bigrams <- tweets %>% 
  filter(is_retweet == FALSE & substring(text,1,4) != "http" & substring(text,1,2) != "RT") %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) #AQUI ESTA LA CLAU

#Eliminar parells de paraules que tinguin alguna stopword 
#+ eliminar tots els parells de paraules amb alguna paraula sense caracters alfabetics
bigrams <- bigrams %>%
  separate(bigram, into = c("first","second"), sep = " ", remove = FALSE) %>%
  anti_join(stop_words, by = c("first" = "word")) %>%
  anti_join(stop_words, by = c("second" = "word")) %>%
  filter(str_detect(first, "[a-z]") &
           str_detect(second, "[a-z]"))

#Paraules més utilizades
words_count <- words %>%
  group_by( word) %>%
  dplyr::count() %>% filter(n>200)

#bigrams més utilitzats
bigrams_count <- bigrams %>%
  group_by(bigram) %>%
  dplyr::count() %>% filter(n>80)

dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(words = words_count$word, freq =words_count$n, colors = blues9[4:8], random.order = FALSE)



wordcloud(words = bigrams_count$bigram, freq =bigrams_count$n, colors = blues9[4:8], random.order = FALSE)

#palabras que tenir en compte: (no estan diccionari) BORDER, CHINA, WALL, RUSSIA, WITCH HUNT, KOREA, MEXICO 
#AFEGIR AL DICCIONARI 


# PART 2: SENTIMENT ANALYSIS

# Añadimos al diccionario los terminos que ha usado mucho trump y tienen una interpretación diferente por ser él.
mykey <- lexicon::hash_sentiment_jockers_rinker
mykey <- update_key(mykey, x = data.frame(x = c("border", "china", "wall", "russia", "which hunt", 
                                                "korea", "mexico"), y = rep(-1,7), stringsAsFactors = FALSE))

save(mykey, file ="mykey.Rdata")


#sentiment analysis
auto_sent <- sentiment(tweets$text,polarity_dt = mykey,
                       valence_shifters_dt = lexicon::hash_valence_shifters, hyphen = "",
                       amplifier.weight = 0.8, n.before = 5, n.after = 2,
                       question.weight = 1, adversative.weight = 0.25,
                       neutral.nonverb.like = FALSE)

# Juntamos por tuit y sumamos sentimiento de las palabras dentro del mismo tuit, en este caso y al ser dentro del tuit esta bien 
#hecha la suma ya que las palabras dentro del mismo tuit se compensan. 
auto_sent2 <- auto_sent %>% 
  group_by(element_id) %>%
  dplyr::summarise(sentimiento_promedio = sum(sentiment)) #GUAY! CUADRA EL NUM DE TWEETS. 

# Asignamos a cada tuit si es positivo, neutro o negativo [INFORMACIÓN POR TUIT!]
auto_sent2$pol <- ifelse(auto_sent2$sentimiento_promedio > 0, "positivo", "negativo")
auto_sent2[auto_sent2$sentimiento_promedio == 0,]$pol <- "neutral"



table(auto_sent2$pol) #añadir las palabras personalizadas ha aumentado mucho los negativos de 4000 y pico a 5000!
fig1 <- barplot(table((auto_sent2$pol)), col = blues9[c(8,5,1)], ylim = c(0,10000), 
        font.lab = 2, las = 1, ylab = "Número de tuits", xlab = "Polaridad")
grid()
text(fig1, round(table(auto_sent2$pol)), paste0(round(table(auto_sent2$pol)), " (", 
                                                100*round(prop.table(table(auto_sent2$pol)),2), "%)"),
     pos = 3, cex = 1.2, font = 2)

par(mfrow = c(1,2), font.lab = 2)

boxplot(auto_sent2$sentimiento_promedio, col = "steelblue", ylim = c(-4,3),ylab = "Sentimiento Polaridad")
grid()
abline(h= mean(auto_sent2$sentimiento_promedio), col = "red", lty = 2)
text(0.63, 0.5 ,paste("Media:", round(mean(auto_sent2$sentimiento_promedio),2)), col = "red")

fig2 <- hist(auto_sent2$sentimiento_promedio, col = "steelblue", ylim = c(0,10000), las = 1, ylab = "Número de tuits",
      font.lab = 2, main = "", breaks = -4.5:3.5, xlim = c(-4,3), xlab = "")
grid()
abline(v= mean(auto_sent2$sentimiento_promedio), col = "red", lty = 2)
text(-2, 6000 ,paste("Media:", round(mean(auto_sent2$sentimiento_promedio),2)), col = "red")
text(x = fig2$mids, y = fig2$counts, fig2$counts, font = 2, pos = 3, cex = 1.2)

summary(auto_sent2$sentimiento_promedio)

# Afegim la informació del tuit 
auto_sent2$id_str <- tweets$id_str
class_tuits <- left_join(auto_sent2, tweets[,c(2,3,11:14)], by = c("id_str" = "id_str"))


# [QUEREMOS INFORMACIÓN POR DIA!]
prueba1 <- class_tuits %>% mutate(anyo = year,
                                  mes = month,
                                  anyo_mes = ymd(paste(anyo, mes,day,  sep="-"),truncated=2)) %>%
  group_by( anyo_mes, pol) %>%
  dplyr::count(pol) #guayyyy YA TENEMOS PARA CADA DIA CUANTOS POS Y CUANTOS NEG 

prueba2_volatilidad <- class_tuits[,c(2,6:8)] %>% mutate(anyo = year,
                                  mes = month,
                                  anyo_mes = ymd(paste(anyo, mes,day,  sep="-"),truncated=2)) %>%
  group_by( anyo_mes) %>%
  summarise_each(funs(mean, sd))

prueba2_volatilidad <- prueba2_volatilidad[c(1,2,8)]

save(prueba2_volatilidad, file = "volatilidad_tuits.Rdata")

#COLUMNA: POS,NEG,NEU POR DIA
prueba2 <- spread(prueba1, pol, n) 

#Asignamos NA por 0
prueba2[is.na(prueba2$negativo),]$negativo <- 0
prueba2[is.na(prueba2$positivo),]$positivo <- 0
prueba2[is.na(prueba2$neutral),]$neutral <- 0

###################################################

p_neg <- prueba2 %>% group_by(mes) %>%
  dplyr::summarise(neg = sum(negativo)) %>% #SUMEN PER OBTENIR EL DEL MES
  ungroup() 

par(mfrow = c(2,1), mar=c(3, 4, 2, 2))
plot(p_neg$neg,type = "l",col = "steelblue", ylim = c(0,500), main = "",  xlab = "Mes de publicación",  ylab = "Tuits negativos por mes", xaxt = "n", font.lab = 2, las = 2)
grid(ny = 6, nx = 47)
axis(1, at = 1:47, labels =  strftime(p_neg$mes,"%Y-%m"), cex.axis=0.60, srt=45, col.ticks = "grey", las=2)


###################################################

p_pos <- prueba2 %>% group_by(mes) %>%
  dplyr::summarise(pos = sum(positivo)) %>% #SUMEN PER OBTENIR EL DEL MES
  ungroup() 

par(mfrow = c(1,1))
plot(p_pos$pos,type = "l",col = "steelblue", ylim = c(0,500), main = "",  xlab = "",  ylab = "Tuits positivos por mes", xaxt = "n", font.lab = 2, las = 2)
grid(ny = 7, nx = 47)
axis(1, at = 1:47, labels =  strftime(p_pos$mes,"%Y-%m"), cex.axis=0.60, srt=45, col.ticks = "grey", las=2)


###################################################

p_index2 <- prueba2 %>% group_by(mes) %>%
  dplyr::summarise(ind = mean(index.pol2)) %>% #SUMEN PER OBTENIR EL DEL MES
  ungroup() 

par(mfrow = c(1,1))
plot(p_index2$ind,type = "l",col = "steelblue", main = "",  xlab = "",  ylab = "Media del índice de polaridad 2 por mes", xaxt = "n", font.lab = 2, las = 2)
grid(ny = 7, nx = 47)
abline(h = 0, col="red", lty = 2)
axis(1, at = 1:47, labels =  strftime(p_pos$mes,"%Y-%m"), cex.axis=0.60, srt=45, col.ticks = "grey", las=2)

###################################################


p_index1 <- prueba2 %>% group_by(mes) %>%
  dplyr::summarise(ind = mean(index.pol)) %>% #SUMEN PER OBTENIR EL DEL MES
  ungroup() 

par(mfrow = c(1,1))
plot(p_index1$ind,type = "l",col = "steelblue", main = "",  xlab = "",  ylab = "Media del índice de polaridad 1 por mes", xaxt = "n", font.lab = 2, las = 2)
grid(ny = 7, nx = 47)
abline(h = 0, col="red", lty = 2)
axis(1, at = 1:47, labels =  strftime(p_pos$mes,"%Y-%m"), cex.axis=0.60, srt=45, col.ticks = "grey", las=2)

###################################################

par(mfrow = c(2,1), mar=c(3, 4, 2, 2))

p_ratio_neg <- prueba2 %>% group_by(mes) %>%
  dplyr::summarise(ratio.neg = mean(ratio_neg)) %>% #SUMEN PER OBTENIR EL DEL MES
  ungroup() 


plot(p_ratio_neg$ratio.neg,type = "l",col = "steelblue", main = "",  xlab = "",  ylab = "Media ratio negatividad por mes", xaxt = "n", font.lab = 2, las = 2)
grid(ny = 7, nx = 47)
abline(h = 0.5, col="red", lty = 2)
axis(1, at = 1:47, labels =  strftime(p_pos$mes,"%Y-%m"), cex.axis=0.60, srt=45, col.ticks = "grey", las=2)


###################################################

p_ratio_pos <- prueba2 %>% group_by(mes) %>%
  dplyr::summarise(ratio.pos = mean(ratio_pos)) %>% #SUMEN PER OBTENIR EL DEL MES
  ungroup() 


plot(p_ratio_pos$ratio.pos,type = "l",col = "steelblue", main = "",  xlab = "",  ylab = "Media ratio positividad por mes", xaxt = "n", font.lab = 2, las = 2)
grid(ny = 7, nx = 47)
abline(h = 0.5, col="red", lty = 2)
axis(1, at = 1:47, labels =  strftime(p_pos$mes,"%Y-%m"), cex.axis=0.60, srt=45, col.ticks = "grey", las=2)


###################################################

# CREAMOS LOS INDICADORES QUE HABIAMOS DICHO: 

#IND1: INDICE POLARIDAD
#IND2: RATIO NEGATIVOS
#IND3: RATIO POSITIVOS 

prueba2$index.pol <- (prueba2$positivo - prueba2$negativo) / (prueba2$positivo + prueba2$negativo) #IND 1 
prueba2$ratio_neg <- prueba2$negativo / (prueba2$positivo + prueba2$negativo) # RATIO NEG
prueba2$ratio_pos <- prueba2$positivo / (prueba2$positivo + prueba2$negativo)  #RATIO POS 
prueba2$index.pol2 <- (prueba2$positivo - prueba2$negativo) # IND 2

prueba2$index.pol.scaled <- scale(prueba2$index.pol)
prueba2$ratio.neg.scaled <- scale(prueba2$ratio_neg)
prueba2$ratio.pos.scaled <- scale(prueba2$ratio_pos)
prueba2$index.pol2.scaled <- scale(prueba2$index.pol2)

#NaN to 0

prueba2[is.na(prueba2)] <- 0

# ESCALAMOS LOS INDICADOREA (RESTAMOS MEDIA Y ENTRE SD)

index.pol.scaled <- scale(prueba2$index.pol)
ratio.neg.scaled <- scale(prueba2$ratio_neg)
ratio.pos.scaled <- scale(prueba2$ratio_pos)
index.pol2.scaled <- scale(prueba2$index.pol2)


#GUARDAMOS TODO (COMPTE QUE HI HAN NA'S!)

save(index.pol.scaled, file = "index.pol.scales.RData")
save(ratio.neg.scaled, file = "ratio.neg.scales.RData")
save(ratio.pos.scaled, file = "ratio.pos.scales.RData")
save(index.pol2.scaled, file = "index.pol2.scaled.Rdata")
save(prueba2, file ="prueba2.Rdata")
save(class_tuits, file = "class_tuits.Rdata")


# gráficos finales: (EN PRUEBAS)

ggplot(prueba2, aes(x=anyo_mes, y=negativo))+
  geom_line(color="red")+
  theme_minimal()+
  ylab("Frequency of Negative Tweets in Trump's Tweets")+
  xlab("Date")

ggplot(prueba2, aes(x=anyo_mes, y=positivo))+
  geom_line(color="blue")+
  theme_minimal()+
  ylab("Frequency of Positive Tweets in Trump's Tweets")+
  xlab("Date")


ggplot(prueba2, aes(x = anyo_mes, y = index.pol)) +
  geom_point() + 
  geom_smooth() + 
  labs(x = "fecha de publicación") +
  theme_bw() +
  theme(legend.position = "none") #per diaaa


ggplot(prueba2, aes(x = anyo_mes, y = ratio_neg)) +
  geom_point() + 
  geom_smooth() + 
  labs(x = "fecha de publicación") +
  theme_bw() +
  theme(legend.position = "none") #per diaaa

ggplot(prueba2, aes(x = anyo_mes, y = ratio_pos)) +
  geom_point() + 
  geom_smooth() + 
  labs(x = "fecha de publicación") +
  theme_bw() +
  theme(legend.position = "none") #per diaaa


prueba2$mes <- format(as.Date(prueba2$anyo_mes, "%Y-%m-%d"), "%Y-%m")  # PER FER PER MES 
prueba2$mes <- ymd(prueba2$mes, truncated = 2) # date format


prueba2 %>% group_by(mes) %>%
  dplyr::summarise(index.pol.mes = mean(index.pol)) %>% #SUMEN PER OBTENIR EL DEL MES
  ungroup() %>%
  ggplot(aes(x = mes, y = index.pol.mes)) +
  geom_point() + 
  geom_smooth() + 
  labs(x = "fecha de publicación") +
  theme_bw() + 
  theme(legend.position = "none") #por mes con sumaa!

prueba2 %>% group_by(mes) %>%
  dplyr::summarise(ratio.neg.mes = mean(ratio_neg)) %>% #SUMEN PER OBTENIR EL DEL MES
  ungroup() %>%
  ggplot(aes(x = mes, y = ratio.neg.mes)) +
  geom_point() + 
  geom_smooth() + 
  labs(x = "fecha de publicación") +
  theme_bw() + 
  theme(legend.position = "none") #por mes con sumaa!

prueba2 %>% group_by(mes) %>%
  dplyr::summarise(ratio.pos.mes = mean(ratio_pos)) %>% #SUMEN PER OBTENIR EL DEL MES
  ungroup() %>%
  ggplot(aes(x = mes, y = ratio.pos.mes)) +
  geom_point() + 
  geom_smooth() + 
  labs(x = "fecha de publicación") +
  theme_bw() + 
  theme(legend.position = "none") #por mes con sumaa!
