library(gridExtra)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tseries)
library(tidyr)

# Load the data of all the contracts
ancien_client_raw <- read.table("~/ValeurClient/src/mina/nouveau_contrat_ancien_client", sep=';')
nouveau_client_raw <- read.table("~/ValeurClient/src/mina/nouveau_contrat_nouveau_client", sep=';')
names(ancien_client_raw) <- c("mois", "cdSi", "noPse", "dateOuv", "noContrat", "nbCotitulaire", "cdFam", "cdSFam","dateNai", "age", "cdSex", "codemetier", "noStrGtn", "cotMacDo", "ageTranche", "noDep", "departement", "region","numstr", "ville")
names(nouveau_client_raw) <- c("mois", "cdSi", "noPse", "dateOuv", "noContrat", "nbCotitulaire", "cdFam", "cdSFam","dateNai", "age", "cdSex", "codemetier", "noStrGtn", "cotMacDo", "ageTranche", "noDep", "departement", "region","numstr", "ville")

ancien_client <- select(ancien_client_raw, mois, dateOuv, cdSi, noPse, noContrat, cdFam, cdSFam, age, cdSex, nbCotitulaire, ageTranche)
nouveau_client <- select(nouveau_client_raw, mois, dateOuv, cdSi, noPse, noContrat, cdFam, cdSFam, age, cdSex, nbCotitulaire, ageTranche)

ancien_client$mois <- ymd(ancien_client$mois)
nouveau_client$mois <- ymd(nouveau_client$mois)
# # Seperate the contracts by month
# ancien_client$year <- year(ancien_client$mois)
# ancien_client$month <- month(ancien_client$mois)
# ancien_client <- mutate(ancien_client, year_month=paste(year,month, sep='-'))
# ancien_client$year_month <- ymd(paste(ancien_client$year_month, "01", sep="-"))

# Order the new contracts by month
ancien_client <- arrange(ancien_client, mois)
plot(table(ancien_client$mois), type = 'l', main='Nombre de nouveaux contrats anciens clients', xlab='mois', ylab='Nombre contrats')

nouveau_client <- arrange(nouveau_client, mois)
plot(table(nouveau_client$mois), type='l', main='Nombre de nouveaux contrats nouveaux clients', xlab='mois', ylab='Nombre contrats')

# df <- data.frame(tab)
# names(df) <- c("year_month", "nb_new_contracts")
# df <- arrange(df, year_month)
# plot(df)

ancien_client$cdSi <- as.factor(ancien_client$cdSi)
ancien_client$nbCotitulaire <- as.factor(ancien_client$nbCotitulaire)
ancien_client_par_mois <- split(ancien_client, ancien_client$mois)

pdf(file = "~/R/cdSi_distribution.pdf")
for(i in 1:length(ancien_client_par_mois)){
  plot_cdSi <- barplot(xtabs(~ancien_client_par_mois[[i]]$cdSi), col = rainbow(3), space=NULL, cex.names = 0.8, 
                       legend.text = c("CMB", "CMMC", "CMSO"),main = ancien_client_par_mois[[i]]$mois[1])
  text(plot_cdSi, xtabs(~ancien_client_par_mois[[i]]$cdSi), labels=as.character(xtabs(~ancien_client_par_mois[[i]]$cdSi)), xpd=TRUE, pos=3)
  pie(xtabs(~ancien_client_par_mois[[i]]$cdSi),main=ancien_client_par_mois[[i]]$mois[1])
}
dev.off()

pdf(file = "~/R/age_distribution.pdf")
for(i in 1:length(ancien_client_par_mois)){
  opar=par(ps=6)
  hist(ancien_client_par_mois[[i]]$age, col=rainbow(length(levels(ancien_client_par_mois[[i]]$age_Tranche))),
       main = ancien_client_par_mois[[i]]$mois[1], labels = TRUE, xlab = "Age", ylab = "Nombre de client")
}
dev.off()

pdf(file = "~/R/cdSex_distribution.pdf")
for(i in 1:length(ancien_client_par_mois)){
  plot_cdSex <- barplot(xtabs(~ancien_client_par_mois[[i]]$cdSex), col = rainbow(2), space=NULL, cex.names = 0.8, 
                        main = ancien_client_par_mois[[i]]$mois[1])
  text(plot_cdSex, xtabs(~ancien_client_par_mois[[i]]$cdSex), labels=as.character(xtabs(~ancien_client_par_mois[[i]]$cdSex)), xpd=TRUE, pos=3)
  pie(xtabs(~ancien_client_par_mois[[i]]$cdSex),main=ancien_client_par_mois[[i]]$mois[1])
}
dev.off()

pdf(file = "~/R/nbCotitulaire_distribution.pdf")
for(i in 1:length(ancien_client_par_mois)){
  plot_nbCotitulaire <- barplot(xtabs(~ancien_client_par_mois[[i]]$nbCotitulaire), col = rainbow(length(levels(ancien_client$nbCotitulaire))), space=NULL, cex.names = 0.8, 
                        main = ancien_client_par_mois[[i]]$mois[1], xlab = 'Nombre de Cotitulaires', ylab='Nombre de Nouveaux contrats')
  text(plot_nbCotitulaire, xtabs(~ancien_client_par_mois[[i]]$nbCotitulaire), labels=as.character(xtabs(~ancien_client_par_mois[[i]]$nbCotitulaire)), xpd=TRUE, pos=3)
  pie(xtabs(~ancien_client_par_mois[[i]]$nbCotitulaire),main=ancien_client_par_mois[[i]]$mois[1])
}
dev.off()

pdf(file = "~/R/cdFam_distribution.pdf", onefile = TRUE)
for(i in 1:length(ancien_client_par_mois)){
  #  ancien_client_par_mois[[i]] <- arrange(ancien_client_par_mois[[i]],cdSFam)
  p <- ggplot(ancien_client_par_mois[[i]], aes(x=cdFam,fill=cdSFam))+geom_bar()+ylab("Nombre de nouveau contrat")+ggtitle(ancien_client_par_mois[[i]]$mois[1])+ theme_grey(base_size = 8) 
  df <- data.frame(table(ancien_client_par_mois[[i]]$cdFam)) 
  names(df) <- c("cdFam", "nb")
  for(j in 1:3){
    p <- p +annotate("text", x=j, y=df$nb[j]+200, label= as.character(df$nb[j]))
  }
  grid.arrange(p)
}
dev.off()

# Generate a data frame where we have the number of new contracts per month


# Add the year and month column as factor(for boxplot)
df$year_month <- ymd(paste(df$year_month, "01", sep="-"))

df$time <- rownames(df)
df <- separate(df, col=year_month, into=c("year", "month", "day"))
df$day <- NULL
df$year <- as.factor(df$year)
df$month <- as.factor(df$month)

# See the significance of every month by using linaire regression 
lin_reg <- lm(nb_new_contracts~month, data=df)
summary(lin_reg)
# No significant month

# See the seasonality of every month by using the box plot
plot(df$month, df$nb_new_contracts, xlab="mois", ylab="Nombre de nouveaux contrats", main="Saisonnalité")

# Plot the real data by month and the prediction only considering the seasonality
plot(df$time, df$nb_new_contracts, type='l', xlab='Mois', ylab="Nombre de nouveaux contrats", main="Saisonnalité prédiction")
lines(df$time, lin_reg$fitted.values, type='l', col='blue')

# See the seasonality by year
lin_reg_year = lm(nb_new_contracts~year, data=df)
summary(lin_reg_year)

# Box plot to see the trend
plot(df$year, df$nb_new_contracts)
plot(df$year, lin_reg_year$fitted.values)

# Verify the year trend by the real data of June over ten years
df_juin <- filter(df, month=="06")
plot(df$year, lin_reg_year$fitted.values)
lines(df_juin$year, df_juin$nb_new_contracts, col='blue')

# Build a HoltWinters model considering both the trend and seasonality 
df_ts <- ts(df$nb_new_contracts, start=c(2006,1),frequency = 12)
df_hw <- HoltWinters(df_ts)
plot(df_hw)

# Predict the number of new contracts of next year by using this model
df_pred <- predict(df_hw, n.ahead=12)

# Plot the prediction
ts.plot(df_ts, df_pred, lty=1:2)

# Verify the prediction with the first season data of 2016
nouveau_client_verify <- filter(nouveau_client_valid, dateOuv>=ymd("20160101"), dateOuv<ymd("20160401"))

# Obtain the statistics of new contracts by month
nouveau_client_verify$year <- year(nouveau_client_verify$dateOuv)
nouveau_client_verify$month <- month(nouveau_client_verify$dateOuv)
nouveau_client_verify <- mutate(nouveau_client_verify, year_month=paste(year,month, sep='-'))
tab_ver<-table(nouveau_client_verify$year_month)
df_ver <- data.frame(tab_ver)

# Convert data frame to time series
ts_ver <- ts(df_ver$Freq, start = c(2016,1), frequency = 12)
# Add the real data into the previous plot to see the difference
lines(ts_ver, col='blue')


nouveau_client_data <- data.frame(tab)
names(nouveau_client_data) <- c("year_month", "nb")
nouveau_client_data$year_month <- ymd(paste(nouveau_client_data$year_month, "01", sep="-"))
nouveau_client_data <- arrange(nouveau_client_data, year_month)
nouveau_client_data$month <- month(nouveau_client_data$year_month)

#attach(nouveau_client_valid_recent)
#tab <-table(nouveau_client_valid_recent$date)
d.tab <- diff(tab)

plot(tab)
plot(d.tab)

#t <- names(tab)

adf.test(tab)
adf.test(d.tab)

acf(tab)
pacf(tab)

acf(d.tab)
pacf(d.tab)

#d=0
arima(tab, order=c(0,0,1))
arima(tab, order=c(0,0,2))
arima(tab, order=c(1,0,0))
arima(tab, order=c(1,0,1))
arima(tab, order=c(1,0,2))
arima(tab, order=c(2,0,0))
arima(tab, order=c(2,0,1))
arima(tab, order=c(2,0,2))

#d=1
arima(tab, order=c(0,1,1))
arima(tab, order=c(0,1,2))
arima(tab, order=c(1,1,0))
arima(tab, order=c(1,1,1))
arima(tab, order=c(1,1,2))
arima(tab, order=c(2,1,0))
arima(tab, order=c(2,1,1))
arima(tab, order=c(2,1,2))


tab.arima001 <- arima(tab, order=c(0,1,1))
tab.arima100 <- arima(tab, order=c(1,0,0))

tab.pred1 <- predict(tab.arima100, n.ahead=5)
plot(tab, main="Nombre de nouveaux contrats par mois", xlab="mois", ylab="nombre de nouveaux contrats")
lines(tab.pred1$pred, col="red")
lines(tab.pred1$pred+2*tab.pred1$se, col="green")
lines(tab.pred1$pred-2*tab.pred1$se, col="green")

#validation
err <- tab[58:62]-tab.pred1$pred
acf(err)
pacf(err)
hist(err)
