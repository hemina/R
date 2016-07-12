#install.packages("ggvis")
library(ggvis)

library(ggplot2)

#install.packages("googleVis")
library(googleVis)

ancien_client_sex <- read.table("~/ValeurClient/nouveaux_contrats/ancien_client_sex/part-r-00000", sep=";")
#ancien_client_age <- read.table("~/ValeurClient/nouveaux_contrats/ancien_client_age/part-r-00000", sep=";")
#ancien_client_metier <- read.table("~/ValeurClient/nouveaux_contrats/ancien_client_cdInseeCsp/part-r-00000", sep=";")
ancien_client_cdSi <- read.table("~/ValeurClient/nouveaux_contrats/ancien_client_cdSi/part-r-00000", sep=";")
ancien_client_region <- read.table("~/ValeurClient/nouveaux_contrats/ancien_client_region/part-r-00000", sep=";")
ancien_client_cdSFam <- read.table("~/ValeurClient/nouveaux_contrats/nbContrat_Par_SousFamille/part-r-00000", sep=";")
ancien_client_cdFam <- read.table("~/ValeurClient/nouveaux_contrats/nbContrat_Par_Famille/part-r-00000", sep=";")


ancien_client <- read.table("~/ValeurClient/nouveau_contrat_ancien_client/part-r-00000", sep=';')
nouveau_client <- read.table("~/ValeurClient/nouveau_contrat_nouveau_client/part-r-00000", sep=';')
namesVec<- c("mois", "cdSi", "noCtrScr", "cdFam", "cdSFam", "noPse", "age", "cdSex", "cdInseeCsp", "noStrGtn", "cotMacDo", "age_Tranche", "noDep", "departement", "region","numstr", "ville")
names(ancien_client) <- namesVec
names(nouveau_client) <- namesVec
head(nouveau_client)
head(ancien_client)

nouveau_client$noDep <- NULL
nouveau_client$numstr <- NULL
nouveau_client$ville <- NULL

ancien_client$noDep <- NULL
ancien_client$numstr <- NULL
ancien_client$ville <- NULL

ancien_client_metier <- data.frame(ancien_client$mois,ancien_client$cdInseeCsp)
ancien_client_age <- data.frame(ancien_client$mois,ancien_client$age)

cdmetier <- read.table("~/ValeurClient/cdmetier.csv", sep=";")
cdmetier <- cdmetier[complete.cases(cdmetier),] #enlever les NA

names(ancien_client_age) <- c("mois", "age")
names(ancien_client_sex) <- c("mois", "sex", "nb")
names(ancien_client_metier) <- c("mois", "codemetier")
names(ancien_client_cdSi) <- c("mois", "cdSi", "nb")
names(ancien_client_region) <- c("mois", "region", "nb")
names(ancien_client_cdSFam) <- c("mois", "cdSFam", "nb")
names(ancien_client_cdFam) <- c("mois", "cdFam", "nb")
names(cdmetier) <- c("codemetier", "metier")

ancien_client_metier$codemetier <- substr(ancien_client_metier$codemetier,1,1)
ancien_client_metier <- merge(ancien_client_metier,cdmetier, by="codemetier")


ancien_client_sex_par_mois <- split(ancien_client_sex, ancien_client_sex$mois)
ancien_client_cdSi_par_mois <- split(ancien_client_cdSi, ancien_client_cdSi$mois)
ancien_client_cdSFam_par_mois <- split(ancien_client_cdSFam, ancien_client_cdSFam$mois)
ancien_client_cdFam_par_mois <- split(ancien_client_cdFam, ancien_client_cdFam$mois)
ancien_client_metier_par_mois <- split(ancien_client_metier, ancien_client_metier$mois)
ancien_client_age_par_mois <- split(ancien_client_age, ancien_client_age$mois)
ancien_client_region_par_mois <- split(ancien_client_region, ancien_client_region$mois)

#pie chart cdSex distribution 
pdf(file = "~/R/cdSex_distribution.pdf")
for(i in 1:length(ancien_client_sex_par_mois)){
  pie(ancien_client_sex_par_mois[[i]]$nb, label=ancien_client_sex_par_mois[[i]]$sex, main=ancien_client_sex_par_mois[[i]]$mois[1])
}
dev.off()

pdf(file = "~/R/cdSi_distribution.pdf")
for(i in 1:length(ancien_client_cdSi_par_mois)){
  pie(ancien_client_cdSi_par_mois[[i]]$nb, label=ancien_client_cdSi_par_mois[[i]]$cdSi, main=ancien_client_cdSi_par_mois[[i]]$mois[1])
}
dev.off()

pdf(file = "~/R/region_distribution.pdf")
for(i in 1:length(ancien_client_region_par_mois)){
  pie(ancien_client_region_par_mois[[i]]$nb, label=ancien_client_region_par_mois[[i]]$region, main=ancien_client_region_par_mois[[i]]$mois[1])
  barplot(ancien_client_region_par_mois[[i]]$nb, main=ancien_client_region_par_mois[[i]]$mois[1], legend=ancien_client_region_par_mois[[i]]$region,
          col=gray.colors(length(table(ancien_client_region_par_mois[[i]]$region))))
}
dev.off()

pdf(file = "~/R/cdSFam_distribution.pdf")
for(i in 1:length(ancien_client_cdSFam_par_mois)){
  pie(ancien_client_cdSFam_par_mois[[i]]$nb, label=ancien_client_cdSFam_par_mois[[i]]$cdSFam, main=ancien_client_cdSFam_par_mois[[i]]$mois[1])
  barplot(ancien_client_cdSFam_par_mois[[i]]$nb, main=ancien_client_cdSFam_par_mois[[i]]$mois[1], legend=ancien_client_cdSFam_par_mois[[i]]$cdSFam,
          col=gray.colors(length(table(ancien_client_cdSFam_par_mois[[i]]$cdSFam))))
}
dev.off()

pdf(file = "~/R/cdFam_distribution.pdf")
for(i in 1:length(ancien_client_cdFam_par_mois)){
  pie(ancien_client_cdFam_par_mois[[i]]$nb, label=ancien_client_cdFam_par_mois[[i]]$cdFam, main=ancien_client_cdFam_par_mois[[i]]$mois[1])
  barplot(ancien_client_cdFam_par_mois[[i]]$nb, main=ancien_client_cdFam_par_mois[[i]]$mois[1], legend=ancien_client_cdFam_par_mois[[i]]$cdFam,
          col=gray.colors(length(table(ancien_client_cdFam_par_mois[[i]]$cdFam))))
}
dev.off()

# pdf(file = "~/R/metier_distribution.pdf")
# for(i in 1:length(ancien_client_metier_par_mois)){
#   pie(ancien_client_metier_par_mois[[i]]$nb, label=ancien_client_metier_par_mois[[i]]$metier, main=ancien_client_metier_par_mois[[i]]$mois[1])
# }
# dev.off()

pdf(file = "~/R/age_distribution.pdf")
for(i in 1:length(ancien_client_age_par_mois)){
  hist(ancien_client_age_par_mois[[i]]$age,col = "pink", main=ancien_client_age_par_mois[[i]]$mois[1])
#  ancien_client_age_par_mois[[i]] %>% ggvis(~age, fill:="pink") %>% layer_histograms(width = 5) %>% add_axis("x", title ="Nouveau contrat ancien client age") %>% add_axis("y", title ="Nombre de clients")
}
dev.off()

pdf(file = "~/R/metier_distribution.pdf")
for(i in 1:length(ancien_client_metier_par_mois)){
  counts <- table(ancien_client_metier_par_mois[[i]]$codemetier)
  barplot(counts, main=ancien_client_metier_par_mois[[i]]$mois[1],legend=levels(ancien_client_metier_par_mois[[i]]$metier),
          col=gray.colors(length(table(ancien_client_metier_par_mois[[i]]$codemetier))))
}
dev.off()

pdf(file = "~/R/out1.pdf")
for(i in 1:1){
  ggplot(ancien_client, aes(age_Tranche))+geom_bar()
}
dev.off()

head(ancien_client)
# bp<- ggplot(ancien_client_sex, aes(x="", y=nb, fill=sex))+geom_bar(width = 1, stat = "identity")
# pie <- bp + coord_polar("y", start=0)
# pie
# 
# pie(ancien_client_sex$nb,label=ancien_client_sex$sex)
# 
# ggplot(ancien_client, aes(cdInseeCsp))+geom_bar()
# 

# nouveau_client %>% ggvis(~age, fill:="lightgreen") %>% layer_histograms(width = 5) %>% add_axis("x", title ="Nouveau contrat ancien client age") %>% add_axis("y", title ="Nombre de clients")
# 
# ancien_client %>% ggvis(~region, fill:="pink") %>% layer_bars() %>% add_axis("x", title ="Nouveau contrat ancien client age") %>% add_axis("y", title ="Nombre de clients")
# 
# Pie <- gvisPieChart(ancien_client)
# plot(Pie)