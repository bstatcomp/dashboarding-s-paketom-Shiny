library(ggplot2)
library(lubridate)
library(tidyr)


###################### PLOT 1 - PR ##################################
df <- data.frame(
    dan = seq(as.Date("2021-11-02"), as.Date("2021-11-26"), by="days"),
    klici = (sample(seq(10), 25, replace = T) + 0.4*seq(50)+10),
    mail = (sample(seq(10), 25, replace = T)+ 0.7*seq(50)+20))

df[(weekdays(df$dan) %in% c("Sunday", "Saturday")), 2:3] <- 0
df <- pivot_longer(df, names_to = "Tip", 
                   values_to = "Stevilo",
                   -dan)

ggplot(df, aes(x = dan, y = Stevilo, color = Tip)) + 
    geom_line(size = 1.2) + ylab("Stevilo kontaktov") + 
    ggtitle("Stevilo kontaktov s strani strank:")


df <- data.frame(
    dan = seq(as.Date("2021-11-02"), as.Date("2021-11-26"), by="days"),
    klici = (sample(seq(10), 25, replace = T) + 0.4*seq(25)+10),
    mail = (sample(seq(10), 25, replace = T)+ 0.7*seq(25)+20))

df[(weekdays(df$dan) %in% c("Sunday", "Saturday")), 2:3] <- 0
df <- pivot_longer(df, names_to = "Tip", 
                   values_to = "Stevilo",
                   -dan)

ggplot(df, aes(x = dan, y = Stevilo, color = Tip)) + 
    geom_line(size = 1.2) + ylab("Stevilo kontaktov") + 
    ggtitle("Stevilo kontaktov s strani strank:")

###################### PLOT 2 - Marketing ##################################

df2 <- data.frame(
    dan = seq(as.Date("2021-11-02"), as.Date("2021-11-26"), by="days"),
    odzivi = 200 + (cumsum(sample(seq(10), 25, replace = T) + exp(seq(25)*0.01))),
    pogodbe = 100 +(cumsum(sample(seq(5), 25, replace = T))))

df2 <- pivot_longer(df2, names_to = "Tip", 
                   values_to = "Stevilo",
                   -dan)

ggplot(df2, aes(x = dan, y = Stevilo, color = Tip)) + 
    geom_line(size = 1.2) + ylab("Stevilo") + 
    ggtitle("Odzivi na oglasno kampanjo in pogodbe s strankami")


###################### PLOT 2 - Racunovodstvo ##################################

df3 <- data.frame(
    oddelek = c("Marketing", "Analitika", "Stiki z javnostjo"),
    prihodki = rnorm(3, mean = 1000000, sd = 100000),
    odhodki = rnorm(3, mean = 800000, sd = 100000)
)

df3 <- pivot_longer(df3, names_to = "Tip", values_to = "Evri", - oddelek)

ggplot(data  = df3, aes(x = oddelek, y = Evri, fill = Tip )) +
    geom_bar(stat = 'identity', 
             position = position_dodge(width = 0.6),  
             width = 0.5) + 
    ggtitle("Pregled prihodkov in odhodkov za vsak oddelek", subtitle = "Mesec: september")
