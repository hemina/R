library(stringr)
library(dplyr)
library(lubridate)

ancien_client <- read.table("~/R/ancien_client/part-r-00000", sep=';')
nouveau_client <- read.table("~/R/nouveau_client/part-r-00000", sep=';')
namesVec<- c("mois", "cdSi", "noCtrScr", "cdFam", "cdSFam", "noPse", "age", "cdSex", "codemetier", "noStrGtn", "cotMacDo", "age_Tranche", "noDep", "departement", "region","numstr", "ville")
names(ancien_client) <- namesVec
names(nouveau_client) <- namesVec

cdmetier <- read.table("~/cdmetier.csv", sep=";")
cdmetier <- cdmetier[complete.cases(cdmetier),] #enlever les NA
names(cdmetier) <- c("codemetier", "metier")

ancien_client$codemetier <- substr(ancien_client$codemetier,1,1)
ancien_client <- merge(ancien_client,cdmetier, by="codemetier")

nouveau_client$codemetier <- substr(nouveau_client$codemetier,1,1)
nouveau_client <- merge(nouveau_client,cdmetier, by="codemetier")

ancien_client <- select(ancien_client, mois, cdSi, cdSex, metier, cdSFam, age, age_Tranche, region)
ancien_client$cdSi <- as.factor(ancien_client$cdSi)
ancien_client$mois <- ymd(ancien_client$mois)

nouveau_client <- select(nouveau_client, mois, cdSi, cdSex, metier, cdSFam, age, age_Tranche, region)
nouveau_client$cdSi <- as.factor(nouveau_client$cdSi)
nouveau_client$mois <- ymd(nouveau_client$mois)

plot(table(ancien_client$mois), type='l', xlab='mois', ylab='nombres de nouveaux contrats des anciens clients')
plot(table(nouveau_client$mois), type='l', xlab='mois', ylab='nombres de nouveaux contrats des nouveaux clients')
#hist(table(ancien_client$mois))

# ancien_client_grid <- ancien_client %>% group_by(mois, cdSi, cdSex, metier, cdSFam, age_Tranche, region)%>% 
#   summarise(nb_client=n()) %>% data.frame() %>% arrange(cdSi, cdSex, metier, cdSFam, age_Tranche, region)%>% 
#   mutate(category= paste(cdSi, cdSex, metier, cdSFam, age_Tranche, region, sep=" - "), time=mois-as.Date("2015-06-30")) %>%
#   select(category, mois, time, nb_client)

ancien_client_grid <- ancien_client %>% group_by(mois, metier, cdSFam, age_Tranche)%>% 
  summarise(nb_client=n()) %>% data.frame() %>% arrange(metier, cdSFam, age_Tranche)%>% 
  mutate(category= paste(metier, cdSFam, age_Tranche, sep=" - "), time=mois-as.Date("2015-06-30")) %>%
  select(category, mois, time, nb_client)

nouveau_client_grid <- nouveau_client %>% group_by(mois, metier, cdSFam, age_Tranche)%>% 
  summarise(nb_client=n()) %>% data.frame() %>% arrange(metier, cdSFam, age_Tranche)%>% 
  mutate(category= paste(metier, cdSFam, age_Tranche, sep=" - "), time=mois-as.Date("2015-06-30")) %>%
  select(category, mois, time, nb_client)

# Filter les catégories où on a plus que 9 mois de données pour les anciens clients
sp_ancien <- split(ancien_client_grid, ancien_client_grid$category)
nb_category_ancien <- rep(0, times=length(sp_ancien))
result <- data.frame(nb_category_ancien)
for(i in 1:length(sp_ancien)){
  nb_category_ancien[i] <- length(sp_ancien[[i]]$mois)
}

r_sqared_ancien <- rep(0, times=length(sp_ancien))

ind <- which(nb_category_ancien>9)
for(i in ind){
  my_lm <- lm(nb_client~time, data=sp_ancien[[i]])
  r_sqared_ancien[i] <- summary(my_lm)$r.squared
}

pdf(file = "~/R/linear regression ancien client.pdf")
for(i in which(r_sqared_ancien>0.5)){
  plot(sp_ancien[[i]]$time, sp_ancien[[i]]$nb_client, xlab='jours', ylab='nombres de nouveaux contrats des anciens clients', main=sp_ancien[[i]]$category[1])
  abline(lm(nb_client~time, data=sp_ancien[[i]]))
}
dev.off()

# Filter les catégories où on a plus que 9 mois de données pour les nouveaux clients
sp_nouveau <- split(nouveau_client_grid, nouveau_client_grid$category)
nb_category_nouveau <- rep(0, times=length(sp_nouveau))
result <- data.frame(nb_category_nouveau)
for(i in 1:length(sp_nouveau)){
  nb_category_nouveau[i] <- length(sp_nouveau[[i]]$mois)
}

r_sqared_nouveau <- rep(0, times=length(sp_nouveau))

ind <- which(nb_category_nouveau>9)
for(i in ind){
  my_lm <- lm(nb_client~time, data=sp_nouveau[[i]])
  r_sqared_nouveau[i] <- summary(my_lm)$r.squared
}

pdf(file = "~/R/linear regression nouveau client.pdf")
for(i in which(r_sqared_nouveau>0.5)){
  plot(sp_nouveau[[i]]$time, sp_nouveau[[i]]$nb_client, xlab='jours', ylab='nombres de nouveaux contrats des nouveaus clients', main=sp_nouveau[[i]]$category[1])
  abline(lm(nb_client~time, data=sp_nouveau[[i]]))
}
dev.off()
