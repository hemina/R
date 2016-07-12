ancien_client = read.csv("~/ValeurClient/part-r-00000",sep=';')
View(ancien_client)
colnames(ancien_client) <- c("mois","cdSi","NoCtr","NoPse","age","cdSex","metier","NoStrGstn","credit","PNB","PNB_tous_contrats","cdSFam")
ancien_client_features <- ancien_client
ancien_client_features$mois <- NULL
ancien_client_features$cdSi <- NULL
ancien_client_features$NoCtr <- NULL
ancien_client_features$NoPse <- NULL
ancien_client_features$NoStrGstn <- NULL
ancien_client_features$PNB <- NULL
ancien_client_features$cdSFam <- NULL

result <- kmeans(ancien_client_features,26)
plot(ancien_client[c("age","metier")],col=result$cluster)
plot(ancien_client[c("age","metier")],col=ancien_client$cdSFam)
plot(ancien_client[c("age","cdSFam")],col=ancien_client$cdSFam)

result <- kmeans(ancien_client_features,3)