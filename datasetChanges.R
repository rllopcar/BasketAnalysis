bakery <- read.csv("BreadBasket_DMS.csv") 

library(plyr)

refactor <- revalue(bakery$Item, c("Ella's Kitchen Pouches"="Ellas Kitchen Pouches", "Valentine's card"="Valentines card",
                       "Hearty & Seasonal"="HeartyAndSeasonal", "My-5 Fruit Shoot"="My5 Fruit Shoot",
                       "Tacos/Fajita"="TacosOrFajita"))

bakery[,"Item"] <- refactor


write.csv(bakery, "breadBasket.csv", row.names = FALSE)
