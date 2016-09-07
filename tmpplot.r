library(lubridate)
library(dplyr)
library(tseries)
library(tidyr)

install_version("RcppArmadillo",repos="http://cran.r-project.org",version="0.6.600.4.0")
install_version("forecast",repos="http://cran.r-project.org",version="7.0")
library(zoo)
library(forecast)

# Load the data of all the contracts
ancien_client <- read.table("~/R/ancien_client/part-r-00000", sep=';')
namesVec<- c("mois", "cdSi", "noCtrScr", "cdFam", "cdSFam", "noPse", "age", "cdSex", "codemetier", "noStrGtn", "cotMacDo", "ageTranche", "noDep", "departement", "region","numstr", "ville")
names(ancien_client) <- namesVec


# Transform the date variable from string to date type
ancien_client$mois <- ymd(ancien_client$mois)

ancien_client_grid <- ancien_client %>% group_by(mois, cdSi, cdSex, cdSFam, ageTranche)%>% 
  summarise(nb=n()) %>% data.frame() %>% arrange(cdSi, cdSex, cdSFam, ageTranche)%>% 
  mutate(category= paste(cdSi, cdSex, cdSFam, ageTranche, sep=" - "), time=mois-as.Date("2015-06-30")) %>%
  select(category, mois, time, nb) %>% arrange(nb)

sp_ancien <- split(ancien_client_grid, ancien_client_grid$category)
nb_category_ancien <- rep(0, times=length(sp_ancien)) # Nombre de categories

for(i in 1:length(sp_ancien)){
  nb_category_ancien[i] <- length(sp_ancien[[i]]$mois)
}

ind <- which(nb_category_ancien>10)

pdf(file = "~/R/test.pdf")
for(i in ind){
  df <- data.frame(mois =sp_ancien[[i]]$mois, nb=sp_ancien[[i]]$nb)
  df <- arrange(df, mois)
  
  # Generer une serie temporelle pour utiliser les modeles, pour pouvoir utiliser le modele HoltWinter, j'ai mis la periode=3, en vrai, periode=12
  ts_df <- ts(df$nb,frequency =3)
  #  plot(decompose(ts_df), main=sp_ancien[[i]]$category[[1]])
  
  ts_df.arima100 <- arima(ts_df, order=c(1,0,0))
  ts_df.pred1 <- predict(ts_df.arima100, n.ahead=3*2)
  ts.plot(ts_df, ts_df.pred1$pred, lty=1:2, col=c("black", "blue"), main=sp_ancien[[i]]$category[[1]])

  qqnorm(ts_df.arima100$residuals, main="verify residuals of arima", col='red')
  qqline(ts_df.arima100$residuals, col='red')
}
dev.off()

pdf(file = "~/R/test.pdf")
for(i in ind){
  df <- data.frame(mois =sp_ancien[[i]]$mois, nb=sp_ancien[[i]]$nb)
  df <- arrange(df, mois)
  
  # Generer une serie temporelle pour utiliser les modeles, pour pouvoir utiliser le modele HoltWinter, j'ai mis la periode=3, en vrai, periode=12
  ts_df <- ts(df$nb,frequency =3)
  #  plot(decompose(ts_df), main=sp_ancien[[i]]$category[[1]])
  
  df_hw <- HoltWinters(ts_df)
  df_pred_hw <- predict(df_hw, n.ahead = 3*2)
  plot(df_hw, df_pred_hw, lty=1:2, main=sp_ancien[[i]]$category[[1]])
  
  df_hww <- forecast.HoltWinters(df_hw, gamma=FALSE)
  
  # qqplot pour verifier les residuals sont de distribution Gaussien
  qqnorm(df_hww$residuals, main="verify residuals of HoltWinters", col='red')
  qqline(df_hww$residuals, col='red')
}
dev.off()

pdf(file = "~/R/test.pdf")
for(i in ind){
  df <- data.frame(mois =sp_ancien[[i]]$mois, nb=sp_ancien[[i]]$nb)
  df <- arrange(df, mois)
  
  # Generer une serie temporelle pour utiliser les modeles, pour pouvoir utiliser le modele HoltWinter, j'ai mis la periode=3, en vrai, periode=12
  ts_df <- ts(df$nb,frequency =3)
  #  plot(decompose(ts_df), main=sp_ancien[[i]]$category[[1]])
  
  plot(stl(ts_df, s.window = "per"))
  
  # qqplot pour verifier les residuals sont de distribution Gaussien
#  qqnorm(df_hww$residuals, main="verify residuals of HoltWinters", col='red')
#  qqline(df_hww$residuals, col='red')
}
dev.off()