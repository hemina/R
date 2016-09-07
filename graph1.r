library(tidyr)

influ_1 <- read.csv("~/questionnaire01.csv", header=TRUE, sep=';')
names(influ_1) <- c("factor", "not_imp", "less_imp", "neutral", "imp", "very_imp")


influ_1 <- separate(influ_1, col = not_imp,c("not_imp", "p_not_imp"))
influ_1 <- separate(influ_1, col = less_imp,c("less_imp", "p_less_imp"))
influ_1 <- separate(influ_1, col = neutral,c("neutral", "p_neutral"))
influ_1 <- separate(influ_1, col = imp,c("imp", "p_imp"))
influ_1 <- separate(influ_1, col = very_imp,c("very_imp", "p_very_imp"))

influ_1

influ_2 <- select(influ_1, factor, not_imp, less_imp, neutral, imp, very_imp)
influ_2
influ_2$not_imp <- as.numeric(influ_2$not_imp)
influ_2$less_imp <- as.numeric(influ_2$less_imp)
influ_2$neutral <- as.numeric(influ_2$neutral)
influ_2$imp <- as.numeric(influ_2$imp)
influ_2$very_imp <- as.numeric(influ_2$very_imp)

row.names(influ_2) <- influ_2$factor

influ_3 <- gather(influ_2, importance, number, not_imp:very_imp)

# set the order of importance
influ_3$factor <- factor(influ_3$factor, order = TRUE, levels=c("stong cast", "director", "genre/plot", "film trailers", "first film's appeal", "promotions", "price and time", "word of mouth"))
influ_3$importance <- factor(influ_3$importance, order = TRUE, levels=c("not_imp", "less_imp", "neutral", "imp", "very_imp"))
influ_3 <- arrange(influ_3, factor, importance)
influ_3

#ggplot(data = influ_3, aes(x=factor, y=number, fill=importance))+geom_bar(stat="identity")+facet_grid(.~factor)+ theme_grey(base_size = 15)
ggplot(data = influ_3, aes(x=factor, y=number, alpha=importance))+geom_bar(stat="identity", width=.5, fill="#56B1F7")+theme_grey(base_size = 14)+ggtitle("Main Facors Attrcted the Audience in Watching Finding Mr. Right 2: Book of Love")


