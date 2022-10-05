library(plumber)
library(dplyr)
library(rpart)
library(readr)
library(lubridate)
library(jsonlite)

fit <- readRDS("modelo_final.rds")

data_fecha <- read_csv("s&p.csv")

data_fecha <- data_fecha %>% 
  mutate(Fecha = dmy(Fecha))

users <- data.frame(
  uid=c(12,13,14,15),
  username=c("kim", "john","stuart","juan")
)

#* @apiTitle Modelo del Titanic
#* @apiDescription Este api nos servira para predicir
#* si un pasajero del titanic sobrevive o no


#* Log some information about the incoming request
#* @filter logger
function(req){
  boole <- length(req$args)
  
  if (boole != 0){
    y <-list('req'= req$args,'query'=req$QUERY_STRING,'user'=req$HTTP_USER_AGENT)
    
    archivo <- toJSON(y, auto_unbox = TRUE)
    
    wd <- getwd()
    
    d <- Sys.time()
    
    dir <- paste0(wd,"/logs","/year=", year(d), "/month=", month(d), "/day=", day(d))
    
    dir.create(dir, recursive = TRUE)
    
    write(archivo, file = paste0(dir,"/",as.integer(Sys.time()),".json"), append = TRUE)
  } 
  
  cat("REQUEST\n")
  plumber::forward()
}


#* Prediccion de sobrevivencia de un pasajero
#* @param Pclass clase en el que viajabe el pasajero
#* @param Sex Sexo del pasajero
#* @param Age edad del pasajero
#* @param SibSp numero de hermanos
#* @param Parch numero de parientes
#* @param Fare precio del boleto
#* @param Embarked puerto del que embarco
#* @post /titanic
function(Pclass, Sex, Age, SibSp, Parch, Fare, Embarked){
  features <- data_frame(Pclass = as.integer(Pclass),
                         Sex,
                         Age=as.integer(Age),
                         SibSp= as.integer(SibSp),
                         Parch = as.integer(Parch),
                         Fare = as.numeric(Fare),
                         Embarked)
  out <- predict(fit,features,type = "class")
  as.character(out)
}

#* Lookup a user
#* @get /users/<id>
function(id){
  subset(users, uid %in% id)
}


#* @get /user/<from>/connect/<to> 
function(from,to){
  from <- ymd(from)
  to <- ymd(to)
  
  subset(data_fecha, Fecha >= from & Fecha <= to)
}

#* @get /typeint/<id:int>
function(id){
  list(
    id = id,
    type = typeof(id)
  )
}

#* @get /typebool/<id:bool>
function(id){
  list(
    id = id,
    type = typeof(id)
  )
}

#* @get /typedouble/<id:double>
function(id){
  list(
    id = id,
    type = typeof(id)
  )
}
