noStrGtn <- read.csv("~/ValeurClient/20150601_info_noStrGtn.csv", sep= '\t')
noRegion <- read.csv("~/ValeurClient/num_region.csv", sep= ';')

noRegion$Préfecture <- NULL
head(noRegion)

names(noRegion)[1] <- "GECPOS"
names(noRegion)

noStrGtn$GECPOS <- substr(noStrGtn$GECPOS,1,2)
noStrGtn$NOMSTR <- NULL
noStrGtn$GENOVOI <- NULL
noStrGtn$GETYVOI <- NULL
noStrGtn$GELIBLD <- NULL
noStrGtn$GETYVOI <- NULL
noStrGtn$GELIBVOI <- NULL
noStrGtn$GEBP <- NULL
head(noStrGtn)

tab <- merge(x = noRegion, y = noStrGtn, by = "GECPOS")
names(tab) <- c("noDep", "departement", "region", "noStrGtn", "ville")
str(tab)
tail(tab)
write.table(tab, file = "~/ValeurClient/noGtn_region.csv", sep = ";", row.names = FALSE)
