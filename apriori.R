# Set working directory
setwd("/Users/franlosada/Documents/EIT 1st year/Cognitive Systems/Bakery/")

# Read the data
earlyMorning <- read.transactions("earlyMorning.csv", format="single", cols=c(3,4), sep=",", rm.duplicates=TRUE)
lateMorning <- read.transactions("lateMorning.csv", format="single", cols=c(3,4), sep=",", rm.duplicates=TRUE)
lunch <- read.transactions("lunch.csv", format="single", cols=c(3,4), sep=",", rm.duplicates=TRUE)
afternoon <- read.transactions("afternoon.csv", format="single", cols=c(3,4), sep=",", rm.duplicates=TRUE)
saturday <- read.transactions("saturday.csv", format="single", cols=c(3,4), sep=",", rm.duplicates=TRUE)
sunday <- read.transactions("sunday.csv", format="single", cols=c(3,4), sep=",", rm.duplicates=TRUE)
weekDays <- read.transactions("weekDays.csv", format="single", cols=c(3,4), sep=",", rm.duplicates=TRUE)

saturday_df <- read.csv("saturday.csv") %>%
  mutate(Date=as.Date(Date),
         Time=hms(Time)
  )

summary(saturday)

# Support and confidence values
supportLevels <- c(0.1, 0.05, 0.01, 0.005)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Empty integers 
earlyMorning_sup10 <- integer(length=9)
earlyMorning_sup5 <- integer(length=9)
earlyMorning_sup1 <- integer(length=9)
earlyMorning_sup0.5 <- integer(length=9)

# Empty integers 
lateMorning_sup10 <- integer(length=9)
lateMorning_sup5 <- integer(length=9)
lateMorning_sup1 <- integer(length=9)
lateMorning_sup0.5 <- integer(length=9)

# Empty integers 
lunch_sup10 <- integer(length=9)
lunch_sup5 <- integer(length=9)
lunch_sup1 <- integer(length=9)
lunch_sup0.5 <- integer(length=9)

# Empty integers 
saturday_sup10 <- integer(length=9)
saturday_sup5 <- integer(length=9)
saturday_sup1 <- integer(length=9)
saturday_sup0.5 <- integer(length=9)

# Empty integers 
sunday_sup10 <- integer(length=9)
sunday_sup5 <- integer(length=9)
sunday_sup1 <- integer(length=9)
sunday_sup0.5 <- integer(length=9)

# Empty integers 
weekDays_sup10 <- integer(length=9)
weekDays_sup5 <- integer(length=9)
weekDays_sup1 <- integer(length=9)
weekDays_sup0.5 <- integer(length=9)

# Empty integers 
afternoon_sup10 <- integer(length=9)
afternoon_sup5 <- integer(length=9)
afternoon_sup1 <- integer(length=9)
afternoon_sup0.5 <- integer(length=9)

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  
  earlyMorning_sup10[i] <- length(apriori(earlyMorning, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules")))
  
  lateMorning_sup10[i] <- length(apriori(lateMorning, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules")))
  
  lunch_sup10[i] <- length(apriori(lunch, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules")))
  
  saturday_sup10[i] <- length(apriori(saturday, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules")))
  
  sunday_sup10[i] <- length(apriori(sunday, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules")))
  
  weekDays_sup10[i] <- length(apriori(weekDays, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules")))
  
  afternoon_sup10[i] <- length(apriori(afternoon, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)) {
  
  earlyMorning_sup5[i] <- length(apriori(earlyMorning, parameter=list(sup=supportLevels[2], 
                                                        conf=confidenceLevels[i], target="rules")))
  
  lateMorning_sup5[i] <- length(apriori(lateMorning, parameter=list(sup=supportLevels[2], 
                                                        conf=confidenceLevels[i], target="rules")))
  
  lunch_sup5[i] <- length(apriori(lunch, parameter=list(sup=supportLevels[2], 
                                                        conf=confidenceLevels[i], target="rules")))
  
  saturday_sup5[i] <- length(apriori(saturday, parameter=list(sup=supportLevels[2], 
                                                        conf=confidenceLevels[i], target="rules")))
  
  sunday_sup5[i] <- length(apriori(sunday, parameter=list(sup=supportLevels[2], 
                                                        conf=confidenceLevels[i], target="rules")))
  
  weekDays_sup5[i] <- length(apriori(weekDays, parameter=list(sup=supportLevels[2], 
                                                        conf=confidenceLevels[i], target="rules")))
  
  afternoon_sup5[i] <- length(apriori(afternoon, parameter=list(sup=supportLevels[2], 
                                                        conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)) {
  
  earlyMorning_sup1[i] <- length(apriori(earlyMorning, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules")))
  
  lateMorning_sup1[i] <- length(apriori(lateMorning, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules")))
  
  lunch_sup1[i] <- length(apriori(lunch, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules")))
  
  saturday_sup1[i] <- length(apriori(saturday, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules")))
  
  sunday_sup1[i] <- length(apriori(sunday, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules")))
  
  weekDays_sup1[i] <- length(apriori(weekDays, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules")))
  
  afternoon_sup1[i] <- length(apriori(afternoon, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)) {
  
  earlyMorning_sup0.5[i] <- length(apriori(earlyMorning, parameter=list(sup=supportLevels[4], 
                                                          conf=confidenceLevels[i], target="rules")))
  
  lateMorning_sup0.5[i] <- length(apriori(lateMorning, parameter=list(sup=supportLevels[4], 
                                                          conf=confidenceLevels[i], target="rules")))
  
  lunch_sup0.5[i] <- length(apriori(lunch, parameter=list(sup=supportLevels[4], 
                                                          conf=confidenceLevels[i], target="rules")))
  
  saturday_sup0.5[i] <- length(apriori(saturday, parameter=list(sup=supportLevels[4], 
                                                          conf=confidenceLevels[i], target="rules")))
  
  sunday_sup0.5[i] <- length(apriori(sunday, parameter=list(sup=supportLevels[4], 
                                                          conf=confidenceLevels[i], target="rules")))
  
  weekDays_sup0.5[i] <- length(apriori(weekDays, parameter=list(sup=supportLevels[4], 
                                                          conf=confidenceLevels[i], target="rules")))
  
  afternoon_sup0.5[i] <- length(apriori(afternoon, parameter=list(sup=supportLevels[4], 
                                                          conf=confidenceLevels[i], target="rules")))
  
}

### EarlyMorning
num_earlyMorning <- data.frame(earlyMorning_sup10, earlyMorning_sup5, earlyMorning_sup1, earlyMorning_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data=num_earlyMorning, aes(x=confidenceLevels)) +
  
  # Plot line and points (support level of 10%)
  geom_line(aes(y=earlyMorning_sup10, colour="Support level of 10%")) + 
  geom_point(aes(y=earlyMorning_sup10, colour="Support level of 10%")) +
  
  # Plot line and points (support level of 5%)
  geom_line(aes(y=earlyMorning_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=earlyMorning_sup5, colour="Support level of 5%")) +
  
  # Plot line and points (support level of 1%)
  geom_line(aes(y=earlyMorning_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y=earlyMorning_sup1, colour="Support level of 1%")) +
  
  # Plot line and points (support level of 0.5%)
  geom_line(aes(y=earlyMorning_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=earlyMorning_sup0.5, colour="Support level of 0.5%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Early Morning: Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())




### LateMorning
num_lateMorning <- data.frame(lateMorning_sup10, lateMorning_sup5, lateMorning_sup1, lateMorning_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data=num_lateMorning, aes(x=confidenceLevels)) +
  
  # Plot line and points (support level of 10%)
  geom_line(aes(y=lateMorning_sup10, colour="Support level of 10%")) + 
  geom_point(aes(y=lateMorning_sup10, colour="Support level of 10%")) +
  
  # Plot line and points (support level of 5%)
  geom_line(aes(y=lateMorning_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=lateMorning_sup5, colour="Support level of 5%")) +
  
  # Plot line and points (support level of 1%)
  geom_line(aes(y=lateMorning_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y=lateMorning_sup1, colour="Support level of 1%")) +
  
  # Plot line and points (support level of 0.5%)
  geom_line(aes(y=lateMorning_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=lateMorning_sup0.5, colour="Support level of 0.5%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Late Morning: Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())




# Lunch
num_lunch <- data.frame(lunch_sup10, lunch_sup5, lunch_sup1, lunch_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data=num_lunch, aes(x=confidenceLevels)) +
  
  # Plot line and points (support level of 10%)
  geom_line(aes(y=lunch_sup10, colour="Support level of 10%")) + 
  geom_point(aes(y=lunch_sup10, colour="Support level of 10%")) +
  
  # Plot line and points (support level of 5%)
  geom_line(aes(y=lunch_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=lunch_sup5, colour="Support level of 5%")) +
  
  # Plot line and points (support level of 1%)
  geom_line(aes(y=lunch_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y=lunch_sup1, colour="Support level of 1%")) +
  
  # Plot line and points (support level of 0.5%)
  geom_line(aes(y=lunch_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=lunch_sup0.5, colour="Support level of 0.5%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Lunch: Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())



# Afternoon
num_afternoon <- data.frame(afternoon_sup10, afternoon_sup5, afternoon_sup1, afternoon_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data=num_afternoon, aes(x=confidenceLevels)) +
  
  # Plot line and points (support level of 10%)
  geom_line(aes(y=afternoon_sup10, colour="Support level of 10%")) + 
  geom_point(aes(y=afternoon_sup10, colour="Support level of 10%")) +
  
  # Plot line and points (support level of 5%)
  geom_line(aes(y=afternoon_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=afternoon_sup5, colour="Support level of 5%")) +
  
  # Plot line and points (support level of 1%)
  geom_line(aes(y=afternoon_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y=afternoon_sup1, colour="Support level of 1%")) +
  
  # Plot line and points (support level of 0.5%)
  geom_line(aes(y=afternoon_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=afternoon_sup0.5, colour="Support level of 0.5%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Dinner: Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())



# WeekDays
num_weekDays <- data.frame(weekDays_sup10, weekDays_sup5, weekDays_sup1, weekDays_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data=num_weekDays, aes(x=confidenceLevels)) +
  
  # Plot line and points (support level of 10%)
  geom_line(aes(y=weekDays_sup10, colour="Support level of 10%")) + 
  geom_point(aes(y=weekDays_sup10, colour="Support level of 10%")) +
  
  # Plot line and points (support level of 5%)
  geom_line(aes(y=weekDays_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=weekDays_sup5, colour="Support level of 5%")) +
  
  # Plot line and points (support level of 1%)
  geom_line(aes(y=weekDays_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y=weekDays_sup1, colour="Support level of 1%")) +
  
  # Plot line and points (support level of 0.5%)
  geom_line(aes(y=weekDays_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=weekDays_sup0.5, colour="Support level of 0.5%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="WeekDays: Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())




# Saturday
num_saturday <- data.frame(saturday_sup10, saturday_sup5, saturday_sup1, saturday_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data=num_saturday, aes(x=confidenceLevels)) +
  
  # Plot line and points (support level of 10%)
  geom_line(aes(y=saturday_sup10, colour="Support level of 10%")) + 
  geom_point(aes(y=saturday_sup10, colour="Support level of 10%")) +
  
  # Plot line and points (support level of 5%)
  geom_line(aes(y=saturday_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=saturday_sup5, colour="Support level of 5%")) +
  
  # Plot line and points (support level of 1%)
  geom_line(aes(y=saturday_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y=saturday_sup1, colour="Support level of 1%")) +
  
  # Plot line and points (support level of 0.5%)
  geom_line(aes(y=saturday_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=saturday_sup0.5, colour="Support level of 0.5%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Saturday: Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())



# Sunday
num_sunday <- data.frame(sunday_sup10, sunday_sup5, sunday_sup1, sunday_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data=num_sunday, aes(x=confidenceLevels)) +
  
  # Plot line and points (support level of 10%)
  geom_line(aes(y=sunday_sup10, colour="Support level of 10%")) + 
  geom_point(aes(y=sunday_sup10, colour="Support level of 10%")) +
  
  # Plot line and points (support level of 5%)
  geom_line(aes(y=sunday_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=sunday_sup5, colour="Support level of 5%")) +
  
  # Plot line and points (support level of 1%)
  geom_line(aes(y=sunday_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y=sunday_sup1, colour="Support level of 1%")) +
  
  # Plot line and points (support level of 0.5%)
  geom_line(aes(y=sunday_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=sunday_sup0.5, colour="Support level of 0.5%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Sunday: Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())


##### APRIORI EXECUTION

### earlyMorning

# Absolute Item Frequency Plot
itemFrequencyPlot(earlyMorning, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")

# Apriori algorithm execution with a support level of 1% and a confidence level of 50%
earlyMorning_sup5_conf50 <- apriori(earlyMorning, parameter=list(sup=0.005,
                                                                 conf=0.3, 
                                                                 target="rules"))

# Association rules
inspect(head(earlyMorning_sup5_conf50, by="lift", decreasing=TRUE, n=30))

sort(earlyMorning_sup5_conf50, by="lift", decreasing=TRUE)

rules <- apriori(earlyMorning, parameter = list(support=0.005, confidence = 0.2),
                 appearance = list(rhs = "Medialuna"))
inspect(head(rules))

### lateMorning

# Apriori algorithm execution with a support level of 1% and a confidence level of 50%
lateMorning_sup5_conf50 <- apriori(lateMorning, parameter=list(sup= 0.01, 
                                                              conf= 0.25, 
                                                              target="rules"))
inspect(lateMorning_sup5_conf50)
# Association rules
inspect(head(lateMorning_sup5_conf50, by="lift", decreasing=TRUE, n=30))

### lunch

# Absolute Item Frequency Plot
itemFrequencyPlot(earlyMorning, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")

# Apriori algorithm execution with a support level of 1% and a confidence level of 50%
lunch_sup5_conf50 <- apriori(lunch, parameter=list(sup=supportLevels[3], 
                                                               conf=confidenceLevels[5], target="rules"))

# Association rules
inspect(lunch_sup5_conf50)

### afternoon

# Apriori algorithm execution with a support level of 1% and a confidence level of 50%
afternoon_sup5_conf50 <- apriori(afternoon, parameter=list(sup=supportLevels[3], 
                                                   conf=confidenceLevels[7], target="rules"))

# Association rules
inspect(afternoon_sup5_conf50)

### saturday

# {} => {beer}
# These rules mean that no matter what other items are involved the item in the RHS will appear 
# with the probability given by the rule's confidence (which equals the support).

# Apriori algorithm execution with a support level of 1% and a confidence level of 50%
saturday_sup5_conf50 <- apriori(saturday, parameter=list(sup=supportLevels[3], 
                                                           conf=confidenceLevels[7], target="rules"))

# Association rules
inspect(saturday_sup5_conf50)


### sunday

# Absolute Item Frequency Plot
itemFrequencyPlot(sunday, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")

# Apriori algorithm execution with a support level of 1% and a confidence level of 50%
sunday_sup5_conf50 <- apriori(sunday, parameter=list(sup=supportLevels[3], 
                                                           conf=confidenceLevels[7], target="rules"))

# Association rules
inspect(sunday_sup5_conf50)

### weekDays

# Apriori algorithm execution with a support level of 1% and a confidence level of 50%
weekDays_sup5_conf50 <- apriori(weekDays, parameter=list(sup=supportLevels[3], 
                                                           conf=confidenceLevels[7], target="rules"))

# Association rules
inspect(weekDays_sup5_conf50)


