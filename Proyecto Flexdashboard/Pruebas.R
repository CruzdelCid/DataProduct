library(flexdashboard)
library(readr)
library(dplyr)
library(highcharter) #Para graficar
library(leaflet)
library(crosstalk)
library(formattable)
library(ggplot2)
library(lubridate)
library(DT)
library(tm)
library(wordcloud)
library(stringr)

tienda <- read_csv("~/DataProduct/Proyecto Flexdashboard/tienda.csv")
names(tienda) <- gsub(" ","_",names(tienda))
head(tienda)


metricas <- tienda %>%
  summarise(
    tot_envios = n(),
    tot_sales = sum(Sales), 
    tot_quantity = sum(Quantity),
    tot_discount = sum(Discount), 
    tot_profit = sum(Profit), 
    tot_products = n_distinct(Product_ID)
  )

metricas


# Producto más rentable
rentable <- tienda %>% 
  select(Product_Name, Product_ID, Profit) %>%
  group_by(Product_Name, Product_ID) %>%
  summarise(profit = sum(Profit)) %>%
  arrange(desc(profit)) %>% 
  head(1)


rentable

rrentable <- rentable$profit  / metricas$tot_profit
rrentable <- round(rrentable, 2) * 100
gauge(rrentable, min=0, max=100, symbol = '%', 
      gaugeSectors(success = c(80, 100),
                   warning = c(40, 79), 
                   danger = c(0,39)))



# Producto menos rentable 
norentable <- tienda %>% 
  select(Product_Name, Product_ID, Profit) %>%
  group_by(Product_Name, Product_ID) %>%
  summarise(profit = sum(Profit)) %>%
  arrange(profit) %>% 
  head(1)

norentable


rnorentable <- norentable$profit  / metricas$tot_profit
rnorentable <- round(rnorentable, 2) * 100 *-1
gauge(rnorentable, min=0, max=100, symbol = '%', 
      gaugeSectors(success = c(80, 100),
                   warning = c(40, 79), 
                   danger = c(0,39)))


# Cantidad de productos no rentables
rentables <- tienda %>% 
  select(Product_Name, Product_ID, Profit) %>%
  group_by(Product_ID) %>%
  summarise(profit = sum(Profit)) %>%
  filter(profit >= 0)

rentables



#####SEGMENTOS #####
# Compras
tienda %>%
  group_by(Segment) %>% 
  summarise(Compras = n()) %>% 
  arrange(desc(Compras)) %>% 
  hchart("column",hcaes(x = Segment, y = Compras))

# Categorías
tienda %>%
  select(Segment, Category) %>% 
  group_by(Segment) %>% 
  summarise(Categorias = n_distinct(Category))

# Profit 
tienda %>%
  group_by(Segment) %>% 
  summarise(Income = sum(Sales), 
            Profit = sum(Profit), 
            ratio = (Profit / Income) * 100) %>% 
  arrange(desc(Compras)) %>% 
  hchart("column",hcaes(x = Segment, y = ratio))

