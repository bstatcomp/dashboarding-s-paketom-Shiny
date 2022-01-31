library(tidyverse)
library(lubridate)

data <- read.table(
    "marketing_data.csv",
    header = 1,
    sep = ',',
    dec = '.',
    row.names = 1
)
# https://www.kaggle.com/jackdaoud/marketing-dat

# Preprocesiranje
today_date <- as.Date("2014-08-05")

data$Income <-
    as.numeric(gsub(
        pattern = "[\\$,]",
        replacement = "",
        x = data$Income
    ))
data <- data[-which(is.na(data$Income)), ]

data$Dt_Customer <-
    as.Date(data$Dt_Customer, format = c("%m/%d/%Y"))

bad_year <- year(data$Dt_Customer) < 200

data$Dt_Customer[bad_year] <-
    data$Dt_Customer[bad_year] %m+% years(2000)

data$Dt_Customer <-
    as.numeric(today_date - as.Date(data$Dt_Customer)) / 365.25

data <-
    data[-which(data$Marital_Status %in% c("YOLO", "Absurd", "Alone")),]

rm_outliers <- function(x) {
    # function returns good rows
    qq <- quantile(x, c(0.25, 0.75))
    return(
        which(
            (x > (qq[1] - 1.5 * IQR(x)))
            &
            (x < (qq[2] + 1.5 * IQR(x)))
        )
    )
}

for (cc in c("Income", "Year_Birth")) {
    a <- data[, cc]
    good_rows <- rm_outliers(a)
    data <- data[good_rows,]
}

boxplot(data$Income)

data$Education[data$Education == "2n Cycle"] <- "Master"
data$Education <- droplevels(data$Education)
levels(data$Education) <- c("Osnovni", "Dodiplomski", "Magisterij", "Doktorat")

data$Marital_Status <- droplevels(data$Marital_Status)
levels(data$Marital_Status) <- c("Ločeni", "Poročeni", "Samski", "V paru", "Odoveli") 

colnames(data) <- c(
    "Leto_rojstva",
    "Izobrazba",
    "Stan",
    "Letni_prihodek",
    "Št._otrok",
    "Št._najstnikov",
    "Leta_članstva",
    "Zadnji_nakup",
    "Količina_vina",
    "Količina_sadja",
    "Količina_mesa",
    "Količina_rib",
    "Količina_sladkarij",
    "Količina_zlata",
    "Nakupi_akcija",
    "Nakupi_splet",
    "Nakupi_katalog",
    "Nakupi_trgovina",
    "Št._obiski_splet",
    "Kampanja_3",
    "Kampanja_4",
    "Kampanja_5",
    "Kampanja_1",
    "Kampanja_2",
    "Zadnja_kampanja",
    "Pritožbe",
    "Država"
)

data$`Št._drugih_družinskih_članov` <- data$Št._otrok + data$Št._najstnikov

demo <- data[, c("Država",
                 "Leto_rojstva",
                 "Izobrazba",
                 "Stan",
                 "Št._otrok",
                 "Št._najstnikov")]

historical <- data[, c("Leta_članstva",
                       "Zadnji_nakup",
                       "Št._obiski_splet",
                       "Pritožbe")]

priljubljeni_tipi <- data[, c(
    "Količina_vina",
    "Količina_sadja",
    "Količina_mesa",
    "Količina_rib",
    "Količina_sladkarij",
    "Količina_zlata"
)]

priljubljeni_načini <- data[, c("Nakupi_akcija",
                                "Nakupi_splet",
                                "Nakupi_katalog",
                                "Nakupi_trgovina")]

kampanje <- data[, c("Kampanja_1",
                     "Kampanja_2",
                     "Kampanja_3",
                     "Kampanja_4",
                     "Kampanja_5",
                     "Zadnja_kampanja")]


# input <- list(stranke_podmn = NULL)
# 
# library(ggplot2)
# library(gridExtra)
# 
# 
# demo2 <- pivot_longer(demo, cols = c(3,4), names_to = "Podatki", values_to = "vrednosti")
# 
# xs <- split(demo2, f = demo2$Podatki)
# p1 <- ggplot(xs$Izobrazba) + 
#         geom_bar(aes(x = Podatki,
#                      fill = vrednosti,), 
#              group = "Izobrazba",
#              stat = "count", 
#              position = "dodge", 
#              show.legend = T)+
#     facet_wrap(~Podatki, nrow =  1, scales = "free_x")
# 
# p2 <- p1 %+% xs$Stan
# 
# grid.arrange(p1, p2, ncol = 1)    
# 
# 
# 

library(tidyverse)

products <- read.csv("production_products.csv", 
                     header = T, encoding = "UTF8")

orig <- c(0.2992573, 0.3511039, 0.3389699, 0.1345064,
          0.3506594, 0.1442746, 0.4112109, 0.08782747, 0.3390097)
products$prod_cost <- products$list_price * (1 - orig[products$brand_id])

brands <- read.csv("bike_sotres/production_brands.csv", 
                     header = T, encoding = "UTF8")

categories <- read.csv("bike_sotres/production_categories.csv", 
                     header = T, encoding = "UTF8")

stocks<- read.csv("bike_sotres/production_stocks.csv", 
                     header = T, encoding = "UTF8")

production <- products %>% 
    inner_join(brands, by = "brand_id") %>%
    inner_join(categories, by = "category_id") %>%
    inner_join(stocks, by = "product_id")  %>%
    inner_join(stores, by = "store_id") 

write_csv(production, "bike_sotres/production.csv")


order_items <- read.csv("bike_sotres/sales_order_items.csv", 
                     header = T, encoding = "UTF8")

orders <- read.csv("bike_sotres/sales_orders.csv", 
                   header = T, encoding = "UTF8")

customers <- read.csv("bike_sotres/sales_customers.csv", 
                       header = T, encoding = "UTF8")

stores<- read.csv("bike_sotres/sales_stores.csv", 
                  header = T, encoding = "UTF8")

staff<- read.csv("bike_sotres/sales_staffs.csv", 
                  header = T, encoding = "UTF8")

sales <- order_items %>% 
    inner_join(orders, by = "order_id", suffix = c("_item", "_order")) %>%
    inner_join(customers, by = "customer_id", suffix = c("", "_cust")) %>%
    inner_join(stores, by = "store_id", suffix = c("", "_store")) %>%
    inner_join(staff, by = "staff_id", suffix = c("", "_staff") )

sales$prod_cost <- products$prod_cost[sales$product_id]

write_csv(sales, "bike_sotres/sales.csv")



# suplementary code...

tmp <- production %>% select(product_name, quantity) %>% group_by(product_name) %>% 
    summarise(quantity = sum(quantity)) %>% arrange(desc(quantity))

library(zoo)
library(lubridate)
library(scales)

sales %>% mutate(
    order_date = ymd(order_date),
    weeks = cut.Date(order_date, "weeks"),
    revenue = list_price * quantity * (1 - discount)) %>%
    group_by(weeks) %>% summarise(revenue =  sum(revenue)) %>%
    mutate(weeks = ymd(weeks)) %>%
    ggplot(., aes(x = weeks, y = revenue)) + 
    geom_line(group = 1) +
    geom_point()+
    scale_x_date(breaks = date_breaks("1 year"))
