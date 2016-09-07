library(ggplot2)
library(lubridate)
library(gridExtra)
library(dplyr)

ancien_client_raw <- read.table("~/R/ancien_client/part-r-00000", sep=';')
nouveau_client_raw <- read.table("~/R/nouveau_client/part-r-00000", sep=';')
namesVec<- c("mois", "cdSi", "noCtrScr", "cdFam", "cdSFam", "noPse", "age", "cdSex", "codemetier", "noStrGtn", "cotMacDo", "age_Tranche", "noDep", "departement", "region","numstr", "ville")
names(ancien_client_raw) <- namesVec
names(nouveau_client_raw) <- namesVec

ancien_client <- select(ancien_client_raw, mois, cdSi, cdFam, cdSFam, age, cdSex, codemetier, age_Tranche, departement, region)
nouveau_client <- select(nouveau_client_raw, mois, cdSi, cdFam, cdSFam, age, cdSex, codemetier, age_Tranche, departement, region)

ancien_client$mois <- ymd(ancien_client$mois)
nouveau_client$mois <- ymd(nouveau_client$mois)

ancien_client$cdSi <- as.factor(ancien_client$cdSi)
nouveau_client$cdSi <- as.factor(nouveau_client$cdSi)

cdmetier <- read.table("~/cdmetier.csv", sep=";")
cdmetier <- cdmetier[complete.cases(cdmetier),] #enlever les NA
names(cdmetier) <- c("codemetier", "metier")

ancien_client$codemetier <- substr(ancien_client$codemetier,1,1)
ancien_client <- merge(ancien_client,cdmetier, by="codemetier")

nouveau_client$codemetier <- substr(nouveau_client$codemetier,1,1)
nouveau_client <- merge(nouveau_client,cdmetier, by="codemetier")

plot(table(ancien_client$mois), type="b", xlab="Mois", ylab="Nombre de contrats", col='blue')
plot(table(nouveau_client$mois), type="b", xlab="Mois", ylab="Nombre de contrats", col='red')

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
  opar=par(ps=9)
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

pdf(file = "~/R/metier_distribution.pdf")
for(i in 1:length(ancien_client_par_mois)){
#  opar=par(ps=10)
  plot_metier <- barplot(xtabs(~ancien_client_par_mois[[i]]$metier), col = rainbow(length(levels(ancien_client_par_mois[[i]]$metier))), 
                     space=NULL, cex.names = 0.8, legend.text = levels(ancien_client_par_mois[[i]]$metier), 
                     args.legend = list(x="topleft", cex=0.6), main = ancien_client_par_mois[[i]]$mois[1])
#  text(plot_metier, xtabs(~ancien_client_par_mois[[i]]$metier), labels=as.character(xtabs(~ancien_client_par_mois[[i]]$metier)), xpd=TRUE, pos=3)
}
dev.off()

pdf(file = "~/R/region_distribution.pdf")
for(i in 1:length(ancien_client_par_mois)){
  plot_region <- barplot(xtabs(~ancien_client_par_mois[[i]]$region), col = rainbow(length(levels(ancien_client_par_mois[[i]]$region))), 
                         space=NULL, cex.names = 0.8, legend.text = levels(ancien_client_par_mois[[i]]$region), 
                         args.legend = list(x="topleft", cex=0.6), main = ancien_client_par_mois[[i]]$mois[1], xlab = "Region", ylab= "Nombre de nouveaux contrats")
#  text(plot_region, xtabs(~ancien_client_par_mois[[i]]$region), labels=as.character(xtabs(~ancien_client_par_mois[[i]]$region)), xpd=TRUE, pos=3)
}
dev.off()

pdf(file = "~/R/cdFam_distribution.pdf", onefile = TRUE)
for(i in 1:length(ancien_client_par_mois)){
#  ancien_client_par_mois[[i]] <- arrange(ancien_client_par_mois[[i]],cdSFam)
  p <- ggplot(ancien_client_par_mois[[i]], aes(x=cdFam,fill=cdSFam))+geom_bar()+ylab("Nombre de nouveau contrat")+ggtitle(ancien_client_par_mois[[i]]$mois[1])+facet_grid(.~metier)+ theme_grey(base_size = 8) 
  df <- data.frame(table(ancien_client_par_mois[[i]]$cdFam)) 
  names(df) <- c("cdFam", "nb")
#   for(j in 1:3){
#     p <- p +annotate("text", x=j, y=df$nb[j]+100, label= as.character(df$nb[j]))
#   }
  grid.arrange(p)
}
dev.off()
