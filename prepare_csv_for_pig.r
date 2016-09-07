library(lubridate)

taux <- read.table("~/taux_evol_prod.csv", sep=';', header = TRUE)
test <- gather(taux, key = cdSfam, value = taux_evol_prod, C001:C017)
names(test) <- c("mois", "inutil", "cdSFam", "taux_evol_prod")
test$inutil <- NULL
test$mois <- as.Date(test$mois, format="%d/%m/%Y")
test$mois <- strftime(test$mois, format="%Y%m%d")
test$mois <- as.character(test$mois)
write.table(test,"taux_prod.csv", sep=';', row.names = FALSE, col.names = FALSE, quote = FALSE)

taux <- read.table("~/taux_evol_cli.csv", sep=';', header = TRUE)
test <- gather(taux, key = cdSfam, value = taux_cli, C001:C017)
names(test) <- c("mois", "inutil", "cdSFam", "tci")
test$inutil <- NULL
test$mois <- as.Date(test$mois, format="%d/%m/%Y")
test$mois <- strftime(test$mois, format="%Y%m%d")
write.table(test,"taux_cli.csv", sep=';', row.names = FALSE, col.names = FALSE, quote = FALSE)

taux <- read.table("~/taux_evol_tci.csv", sep=';', header = TRUE)
test <- gather(taux, key = cdSfam, value = TCI, C001:C017)
names(test) <- c("mois", "inutil", "cdSFam", "tci")
test$inutil <- NULL
test$mois <- as.Date(test$mois, format="%d/%m/%Y")
test$mois <- strftime(test$mois, format="%Y%m%d")
write.table(test,"tci.csv", sep=';', row.names = FALSE, col.names = FALSE, quote = FALSE)
