library(lubridate)
library(dplyr)
library(tseries)
library(tidyr)

# Load the data of all the contracts
nouveau_client <- read.table("~/ValeurClient/src/mina/part-r-00000", sep=';')
names(nouveau_client) <- c("dateOuv", "dateNai", "cdSi", "noContrat", "noPse", "cdSex", "cdmetier", "noStrGtn")

# Transform the date variable from string to date type
nouveau_client$dateOuv <- ymd(nouveau_client$dateOuv)
nouveau_client$dateNai <- ymd(nouveau_client$dateNai)

# Filter the contracts to ensure that the date of birth of the client is before the date of his contract subscription
#nouveau_client_valid <- filter(nouveau_client, dateNai<dateOuv)
# Calculate the age of the client in the day of his contract subscription in order to prepare for the statistics
#nouveau_client_valid$age <- nouveau_client_valid$dateOuv-nouveau_client_valid$dateNai

#nouveau_client_valid_recent <- filter(nouveau_client_valid, dateOuv>ymd("20150301"))
# Filter the new contracts between 20060101 and 20160101
date_debut <- ymd("20060101")
date_fin <- ymd("20160301")
nouveau_client_valid_recent <- filter(nouveau_client_valid, dateOuv>=date_debut, dateOuv<date_fin)

# Seperate the contracts by date
tab<-table(nouveau_client_valid_recent$dateOuv)
df_tab_date <- data.frame(tab)
names(df_tab_date) <- c("date", "Nb")

# Complete the days where there is no new contracts by 0
date_init <- date_debut
list_date <- levels(df_tab_date$date)
cond1 <- is.element(as.character(date_init), list_date)

df_tab_date <- spread(df_tab_date, date, Nb)
while(date_init<date_fin){
  if(!cond1){
    df_tab_date[[as.character(date_init)]] <- 0
    print(as.character(date_init))    
  }
  date_init <- date_init+1
  cond1 <- is.element(as.character(date_init), list_date)
}

df_tab_date <- gather(df_tab_date, date, Nb)
df_tab_date$date <- ymd(df_tab_date$date)
df_tab_date <- arrange(df_tab_date,date)
##################################################################################
plot(df_tab_date, type='l')

# Seperate by month
nouveau_client_valid_recent$year <- year(nouveau_client_valid_recent$dateOuv)
nouveau_client_valid_recent$month <- month(nouveau_client_valid_recent$dateOuv)
nouveau_client_valid_recent <- mutate(nouveau_client_valid_recent, year_month=paste(year,month, sep='-'))

tab<-table(nouveau_client_valid_recent$year_month)
d.tab <- diff(tab)

plot(tab, type='l')

adf.test(tab)
adf.test(d.tab)

acf(tab)
pacf(tab)

acf(d.tab)
pacf(d.tab)

tab <- df_tab_date$Nb
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

#validation
err <- tab[253:302]-tab.pred1$pred
acf(err)
pacf(err)
hist(err)
