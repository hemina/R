library(lubridate)
library(dplyr)
library(tseries)
library(tidyr)

# Load the data of all the contracts
nouveau_contrat_ancien_client <- read.table("~/ValeurClient/src/mina/nouveau_contrat_ancien_client", sep=';')
names(nouveau_contrat_ancien_client) <- c("mois", "cdSi", "noPse", "dateOuv", "noContrat", "nbClients", "cdFam", "cdSFam","dateNai", "age", "cdSex", "codemetier", "noStrGtn", "cotMacDo", "ageTranche")

# Transform the date variable from string to date type
nouveau_contrat_ancien_client$dateOuv <- ymd(nouveau_contrat_ancien_client$dateOuv)
nouveau_contrat_ancien_client$dateNai <- ymd(nouveau_contrat_ancien_client$dateNai)

############### Pour obtenir la région ################################################
# cdmetier <- read.table("~/ValeurClient/cdmetier.csv", sep=";")
# cdmetier <- cdmetier[complete.cases(cdmetier),] #enlever les NA
# names(cdmetier) <- c("codemetier", "metier")
# 
# nouveau_client$codemetier <- substr(nouveau_client$codemetier,1,1)
# nouveau_client <- merge(nouveau_client,cdmetier, by="codemetier")
#######################################################################################


# Filter the contracts to ensure that the date of birth of the client is before the date of his contract subscription
#nouveau_client_valid <- filter(nouveau_client, dateNai<dateOuv)
# Calculate the age of the client in the day of his contract subscription in order to prepare for the statistics
#nouveau_client_valid$age <- nouveau_client_valid$dateOuv-nouveau_client_valid$dateNai

#nouveau_client_valid_recent <- filter(nouveau_client_valid, dateOuv>ymd("20150301"))
# Filter the new contracts between 20060101 and 20160101
#nouveau_client_valid_recent <- filter(nouveau_client_valid, dateOuv>=ymd("20060101"), dateOuv<ymd("20160101"))
#nouveau_client_valid_recent$date <- nouveau_client_valid_recent$dateOuv-ymd("20150101")

#plot(table(nouveau_client_valid_recent$dateOuv))

nouveau_client_grid <- nouveau_client %>% group_by(mois, metier, cdSFam, age_Tranche)%>% 
  summarise(nb_client=n()) %>% data.frame() %>% arrange(metier, cdSFam, age_Tranche)%>% 
  mutate(category= paste(metier, cdSFam, age_Tranche, sep=" - "), time=mois-as.Date("2015-06-30")) %>%
  select(category, mois, time, nb_client)

# Seperate the contracts by month
nouveau_client_valid_recent$year <- year(nouveau_client_valid_recent$dateOuv)
nouveau_client_valid_recent$month <- month(nouveau_client_valid_recent$dateOuv)
nouveau_client_valid_recent <- mutate(nouveau_client_valid_recent, year_month=paste(year,month, sep='-'))
tab<-table(nouveau_client_valid_recent$year_month)

# Generate a data frame where we have the number of new contracts per month
df <- data.frame(tab)
names(df) <- c("year_month", "nb_new_contracts")

# Add the year and month column as factor(for boxplot)
df$year_month <- ymd(paste(df$year_month, "01", sep="-"))
df <- arrange(df, year_month)
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
