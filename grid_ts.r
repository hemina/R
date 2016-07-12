library(gridExtra)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tseries)
library(tidyr)
require(devtools)
#install_version("RcppArmadillo",repos="http://cran.r-project.org",version="0.6.600.4.0")
#install_version("forecast",repos="http://cran.r-project.org",version="7.0")
library(zoo)
library(forecast)
 
# Load the data of all the contracts
ancien_client_raw <- read.table("~/ValeurClient/src/mina/nouveau_contrat_ancien_client", sep=';')
nouveau_client_raw <- read.table("~/ValeurClient/src/mina/nouveau_contrat_nouveau_client", sep=';')
names(ancien_client_raw) <- c("mois", "cdSi", "noPse", "dateOuv", "noContrat", "nbCotitulaire", "cdFam", "cdSFam","dateNai", "age", "cdSex", "codemetier", "noStrGtn", "cotMacDo", "ageTranche", "noDep", "departement", "region","numstr", "ville")
names(nouveau_client_raw) <- c("mois", "cdSi", "noPse", "dateOuv", "noContrat", "nbCotitulaire", "cdFam", "cdSFam","dateNai", "age", "cdSex", "codemetier", "noStrGtn", "cotMacDo", "ageTranche", "noDep", "departement", "region","numstr", "ville")

ancien_client <- select(ancien_client_raw, mois, dateOuv, cdSi, noPse, noContrat, cdFam, cdSFam, age, cdSex, nbCotitulaire, ageTranche)
nouveau_client <- select(nouveau_client_raw, mois, dateOuv, cdSi, noPse, noContrat, cdFam, cdSFam, age, cdSex, nbCotitulaire, ageTranche)

ancien_client$mois <- ymd(ancien_client$mois)
nouveau_client$mois <- ymd(nouveau_client$mois)

ancien_client_grid <- ancien_client %>% group_by(mois, nbCotitulaire, cdSex, cdSFam, ageTranche)%>% 
  summarise(nb=n()) %>% data.frame() %>% arrange(nbCotitulaire, cdSex, cdSFam, ageTranche)%>% 
  mutate(category= paste(nbCotitulaire, cdSex, cdSFam, ageTranche, sep=" - "), time=mois-as.Date("2015-06-30")) %>%
  select(category, mois, time, nb) %>% arrange(nb)

plot(ancien_client_grid$mois, ancien_client_grid$nb)

# Filter les catégories où on a plus que 9 mois de données pour les anciens clients
sp_ancien <- split(ancien_client_grid, ancien_client_grid$category)
nb_category_ancien <- rep(0, times=length(sp_ancien)) # Nombre de categories

for(i in 1:length(sp_ancien)){
  nb_category_ancien[i] <- length(sp_ancien[[i]]$mois)
}

ind <- which(nb_category_ancien>10)

pdf(file = "~/R/predict.pdf")
for(i in ind){
  df <- data.frame(mois =sp_ancien[[i]]$mois, nb=sp_ancien[[i]]$nb)
  df <- arrange(df, mois)

  # Generer une serie temporelle pour utiliser les modeles, pour pouvoir utiliser le modele HoltWinter, j'ai mis la periode=3, en vrai, periode=12
  ts_df <- ts(df$nb,frequency =3)
  plot(decompose(ts_df), main=sp_ancien[[i]]$category[[1]])
  
  ts_df.arima100 <- arima(ts_df, order=c(1,0,0))
  ts_df.pred1 <- predict(ts_df.arima100, n.ahead=3*2)
  ts.plot(ts_df, ts_df.pred1$pred, lty=1:2, col=c("black", "blue"), main=sp_ancien[[i]]$category[[1]])
  
  ts_df.arima001 <- arima(ts_df, order=c(0,0,1))
  ts_df.pred001 <- predict(ts_df.arima001, n.ahead=3*2)
  lines(ts_df.pred001$pred, col="yellow")
  
  lines(stlf(ts_df)$mean, lty=2, col='green')
  
  df_hw <- HoltWinters(ts_df, gamma=FALSE)
  df_pred_hw <- predict(df_hw, n.ahead = 3*2)
  lines(df_pred_hw, col='red', lty=2)

  legend("topright",lty=1, pch=1, col=c("black", "blue", "yellow", "green", "red"), 
         c("data", "AR1", "MA1", "stlf","Holt Winters"))
}
dev.off()

tsmod <- stlm(ts_df, modelfunction=ar)
plot(forecast(tsmod, h=36))

ts.plot(ts_df, main="Nombre de nouveaux contrats par mois", xlab="mois", ylab="nombre de nouveaux contrats", type='l')
lines(ts_df.pred1$pred, col="green")
lines(ts_df.pred1$pred+2*ts_df.pred1$se, col="green")
lines(ts_df.pred1$pred-2*ts_df.pred1$se, col="green")
