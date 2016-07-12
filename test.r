library(dplyr)
library(lubridate)
library(weights)

############################################ import data #####################################################
dataset <- read.table("~/data_processing/dataset.txt", sep="^", header = FALSE)
names(dataset) <- c("travel_date","origin", "destination", "booking_date", "party_size")


############################################ clean data ########################################################
#select the bookings for which the travel and booking dates are given and reasonable
dataset <- mutate_each(dataset, funs(ymd), travel_date, booking_date)

date_begin <- ymd("2008-01-01")
date_end <- ymd("2010-12-31")

valid_date <- filter(dataset,(travel_date>=date_begin)&(travel_date<=date_end)&(booking_date>=date_begin)&(booking_date<=date_end))


#select the bookings for which the departure and arrival airports are given and made of three letters
valid_origin <- valid_date[grep(pattern = "[A-Z]{3}", x=valid_date$origin),]
valid_destination <- valid_origin[grep(pattern = "[A-Z]{3}", x=valid_origin$destination),]

#select the bookings valid and pertain to 1 or 2 passengers
valid_passenger_dataset <- filter(valid_destination, party_size==1|party_size==2 )



############################################# Q&A ###########################################################
#find how many bookings are valid and pertain to 1 or 2 passengers
str(valid_passenger_dataset)
#20554840 bookings

#find how many passengers in total do these bookings (valid and for 1 or 2 passengers) represent
sum(valid_passenger_dataset$party_size)
#30423000 passengers

#calculate the advance purchase per booking
valid_passenger_dataset$advance_purchase <- valid_passenger_dataset$travel_date-valid_passenger_dataset$booking_date

#calculate the standard deviation of the advance purchase per booking 
sd(valid_passenger_dataset$advance_purchase)
#93.67485 days

#calculate the mean advance purchase per pax
weighted.mean(valid_passenger_dataset$advance_purchase,valid_passenger_dataset$party_size)
#121.2632 days


############################################# visualization ##################################################
#draw the (sample) cumulative distribution function (cdf) of advance purchase per booking
df <- data.frame(table(valid_passenger_dataset$advance_purchase))
names(df) <- c("advance_purchase", "count")
df$cumsum <- cumsum(df$count)
plot(x=df$advance_purchase, y=df$cumsum/1000000, type="l", xlab="Advance purchase(days)", ylab="Number of bookings(Millions)", main="Cumulative distribution function (cdf) of advance purchase per booking")
#from this graph, we can see that there are some negative advance purchase existing in our dataset which could be errors

#draw the (sample) probability mass function of advance purchase with a histogram
hist(as.numeric(valid_passenger_dataset$advance_purchase), probability=TRUE, right = FALSE, xlab="Advance purchase(days)", ylab = "Probability", main = "Probability mass function of advance purchase")

#draw the (sample) probability mass function of advance purchase weighted by number of passengers per booking
wtd.hist(as.numeric(valid_passenger_dataset$advance_purchase), probability=TRUE, right = FALSE, weight = valid_passenger_dataset$party_size, xlab="Advance purchase(days)", ylab = "Probability", main = "Probability mass function of party size weighted advance purchase")
