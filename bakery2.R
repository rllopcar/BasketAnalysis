# Loading libraries
if (!require(tibble)) install.packages("tibble")
library(tibble)
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if (!require(lubridate)) install.packages("lubridate")
library(lubridate)
if (!require(viridis)) install.packages("viridis")
library(viridis)
if (!require(ggthemes)) install.packages("ggthemes")
library(ggthemes)
if (!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)
if (!require(ggridges)) install.packages("ggridges")
library(ggridges)
if (!require(arules)) install.packages("arules")
library(arules)
if (!require(arulesViz)) install.packages("arulesViz")
library(arulesViz)
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
if (!require(knitr)) install.packages("knitr")
library(knitr)

# Set working directory
setwd("/Users/franlosada/Documents/EIT 1st year/Cognitive Systems/Bakery/BasketAnalysis")

bakery <- read.csv("breadBasket.csv") %>%
  mutate(Date=as.Date(Date),
         Time=hms(Time)
  )

###### Analysis of the items popularity

## We use a transaction object
trans <- read.transactions("breadBasket.csv", format="single", cols=c(3,4), sep=",", rm.duplicates=TRUE)

# Absolute Item Frequency Plot
itemFrequencyPlot(trans, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")


################ Analysis taking into account the Date and Time
# We will analyize how Date and Time influence the transactions

bakery <- read.csv("breadBasket.csv") %>%
  mutate(Date=as.Date(Date),
         Time=hms(Time)
  )

# En total, tanto para month, weekday y hour, suman 9531 transactions

# Transactions per month
bakery %>%
  mutate(Month=as.factor(month(Date))) %>%
  group_by(Month) %>%
  summarise(Transactions=n_distinct(Transaction)) %>%
  ggplot(aes(x=Month, y=Transactions)) +
  geom_bar(stat="identity", fill="mistyrose2", show.legend=FALSE) +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per month") +
  theme_bw()

# Transactions per weekday
bakery %>%
  mutate(WeekDay=as.factor(weekdays(as.Date(Date)))) %>%
  group_by(WeekDay) %>%
  summarise(Transactions=n_distinct(Transaction)) %>%
  ggplot(aes(x=WeekDay, y=Transactions)) +
  geom_bar(stat="identity", fill="peachpuff2", show.legend=FALSE) +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per weekday") +
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  theme_bw()

# How many items to people purchase on average
itemAvg.1 <- bakery %>% 
  mutate(Day = wday(Date,label=T)) %>% 
  group_by(Day) %>% 
  summarise(Count= n()) 

itemAvg.2 <- bakery %>% 
  mutate(wday=wday(Date,label=T)) %>% 
  group_by(wday,Transaction) %>% 
  summarise(n_distinct(Transaction)) %>% 
  summarise(Count=n())

itemAvg.3 <- data.frame(itemAvg.1, # Days, total items
                   itemAvg.2[2], # unique transactions
                   itemAvg.1[2]/itemAvg.2[2])  # items per unique transaction
colnames(itemAvg.3) <- c("Day","Line","Unique","Items.Trans")

ggplot(itemAvg.3,aes(x=Day,y=Items.Trans,fill=Day))+
  theme_fivethirtyeight()+
  geom_bar(stat="identity", fill="peachpuff2")+
  ggtitle("Items purchased on average per weekday")+
  theme(legend.position="none")+
  geom_text(aes(label=round(Items.Trans,1)), vjust=2)


# Transactions per hour
bakery %>%
  mutate(Hour=as.factor(hour(Time))) %>%
  group_by(Hour) %>%
  summarise(Transactions=n_distinct(Transaction)) %>%
  ggplot(aes(x=Hour, y=Transactions)) +
  geom_bar(stat="identity", fill="steelblue1", show.legend=FALSE) +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per hour") +
  theme_bw()

# Total items per transaction per hour
itemTransHour.1 <- bakery %>% 
  mutate(Hour = as.factor(hour(Time)))%>% 
  group_by(Hour) %>% 
  summarise(Count= n()) 

itemTransHour.2 <- bakery %>% 
  mutate(Hour = as.factor(hour(Time)))%>% 
  group_by(Hour,Transaction) %>% 
  summarise(n_distinct(Transaction)) %>% 
  summarise(Count=n())

itemTransHour.3 <- data.frame(itemTransHour.1, # Days, total items
                   itemTransHour.2[2], # unique transactions
                   itemTransHour.1[2]/itemTransHour.2[2])  # items per unique transaction
colnames(itemTransHour.3) <- c("Hour","Line","Unique","Items.Trans")

itemTransHour <- 
  ggplot(itemTransHour.3,aes(x=Hour,y=Items.Trans,fill=Hour))+
  theme_fivethirtyeight()+
  geom_bar(stat="identity")+
  ggtitle("Total items per transaction per hour")+
  theme(legend.position="none")+
  geom_text(aes(label=round(Items.Trans,1)), vjust=2)
itemTransHour

# Tema horas --> 

# Vamos a basarnos en las conclusiones extraidas de:
#   -las transacciones por hora
#   -Elementos comprados por transacción 


# 1- Desde que abre hasta las 10 (las 10 sin incluir) -- Desayunos rapidos, pocas transac y pocos items por transac (1.6-2)
# 2- Desde las 10 hasta la 1 (la 1 sin incluir) -- Desayunos, muchas transacciones y mas items por trans (2.2)
# 3- Desde la 1 hasta las 3 (las 3 sin incluir) -- Comidas, numero transacciones intermedio y y mas intems por trans (2.4)
# 4- A partir de las 3 -- Por la tarde, desplome exponencciañ de transacciones por hora y a partir de las 6 tambien desciende los items por trans (1.6-1.5)

############ Analysis taking into account the Transactions (Columnas 3 y 4)

# Read the data
trans <- read.transactions("BreadBasket_DMS.csv", format="single", cols=c(3,4), sep=",", rm.duplicates=TRUE)

bakery2 <- read.csv("breadBasket.csv") 

trans2 <- read.transactions("breadBasket.csv", format="single", cols=c(3,4), sep=",", rm.duplicates=TRUE)

trans2@itemInfo[32,1]
trans@itemInfo[31,1]

##### Building a products data frame with the associate prices and waste

products = data.frame(levels(bakery$Item))
colnames(products) = c("products")
products$price = NULL
prices = c(3, 12, 4, 5, 14.5, 1, 0.80, 0.90, 1, 9, 2, 1.10, 3, 3, 3.20, 10, 0.50, 0.40, 12.5, 0.30, 1, 1.70, 
           1.20, 3, 2, 1.20, 3, 1.50, 0.20, 0.80, 1.40, 2.50, 1.7, 3, 0.50, 1, 6.80, 3.90, 4, 2.80, 2, 1, 1.5, 
           2, 0.60, 2.60, 4.80, 1.80, 3, 0.50, 2.10, 3.5, 1.20, 1.10, 1, 1.1, 0.80, 0.40, 1.20, 1.50, 1.8, 0.05, 
           2, 0.20, 3, 1.20, 1.2, 0.60, 1, 1, 3.20, 0.70, 6.50, 3, 1.50, 2.30, 0.70, 2.50, 1, 13, 0.4, 2, 2.5, 
           1.20, 1, 2.3, 1.3, 1,20, 0.80, 11, 1, 12, 8, 2.50)
summary(prices)
products$prices = prices 

waste = c(0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 
          1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0,
          1, 1, 1, 0, 1, 0, 0, 1, 1, 1)
products$waste = waste
