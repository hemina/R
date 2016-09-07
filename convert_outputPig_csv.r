contrat <- read.table("~/contrat_proba_vivant/part-r-00000", sep=';')
names(contrat) <- c("cdSi", "reseau", "mois", "noClient", "probaVivant","age")
write.table(contrat,"contrat_proba.csv", sep=';', row.names = FALSE)

