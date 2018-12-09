# Set working directory
setwd("/Users/franlosada/Documents/EIT 1st year/Cognitive Systems/Bakery/")

complete <- read.csv("breadBasket.csv") 

complete <-  mutate(complete, Date=as.Date(Date),
             Time=hms(Time)
             )

str(complete)

# ### Theory
# # Menos que una fecha
# menorQueDia <- complete[ complete[,"Date"] < as.Date("2016-11-02") , ]
# # Entre 2 fechas
# entreDias <- complete[ as.Date("2016-10-30") < complete[,"Date"] & complete[,"Date"] < as.Date("2016-11-02") , ]
# # Menos que una hora
# menorQueHora <- complete[ complete[,"Time"] < hms("10H 0M 0S") , ]
# # Entre dos horas
# menorQueHora <- complete[ complete[,"Time"] > hms("10H 0M 0S") & complete[,"Time"] < hms("13H 0M 0S") , ]
# # Un dia de la semana en concreto:
# diaSemana <- complete[ wday(complete[,"Date"]) == "6" , ]


# Hands on

# Segments by hours 

# 1- Desde que abre hasta las 10 (las 10 sin incluir) -- Desayunos rapidos, pocas transac y pocos items por transac (1.6-2)
earlyMorning <- complete[ complete[,"Time"] < hms("10H 0M 0S") , ]

# 2- Desde las 10 hasta la 1 (la 1 sin incluir) -- Desayunos, muchas transacciones y mas items por trans (2.2)
lateMorning <- complete[ complete[,"Time"] > hms("10H 0M 0S") & complete[,"Time"] < hms("13H 0M 0S") , ]

# 3- Desde la 1 hasta las 3 (las 3 sin incluir) -- Comidas, numero transacciones intermedio y y mas intems por trans (2.4)
lunch <- complete[ complete[,"Time"] > hms("13H 0M 0S") & complete[,"Time"] < hms("15H 0M 0S") , ]

# 4- A partir de las 3 -- Por la tarde, desplome exponencial de transacciones por hora y a partir de las 6 tambien desciende los items por trans (1.6-1.5)
afternoon <- complete[ complete[,"Time"] > hms("15H 0M 0S") , ]

# Segments by weekdays
weekDays <- complete[ wday(complete[,"Date"]) == "1" | wday(complete[,"Date"]) == "2" | wday(complete[,"Date"]) == "3" | wday(complete[,"Date"]) == "4" | wday(complete[,"Date"]) == "5", ]

# Segments by Saturdays
saturday <- complete[ wday(complete[,"Date"]) == "6" , ]

# Segments by Sundays
sunday <- complete[ wday(complete[,"Date"]) == "7" , ]

write.csv(earlyMorning, "earlyMorning.csv", row.names = FALSE)
write.csv(lateMorning, "lateMorning.csv", row.names = FALSE)
write.csv(lunch, "lunch.csv", row.names = FALSE)
write.csv(afternoon, "afternoon.csv", row.names = FALSE)

write.csv(weekDays, "weekDays.csv", row.names = FALSE)
write.csv(saturday, "saturday.csv", row.names = FALSE)
write.csv(sunday, "sunday.csv", row.names = FALSE)

# Minimum value for Time
min <- complete[ complete[,"Time"] < hms("8H 20M 0S") , ]

