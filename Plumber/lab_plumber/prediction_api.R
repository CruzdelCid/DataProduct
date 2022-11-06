
library(plumber)
library(dplyr)
library(rpart)
library(readr)
library(lubridate)

fit <- readRDS(gzcon(url("https://github.com/CruzdelCid/DataProduct/blob/main/Plumber/lab_plumber/modelo_final.rds?raw=true")))
data_fecha <- read_delim("s&p.csv")

data_fecha$Fecha <- as.Date(data_fecha$Fecha, format="%d/%m/%y")

users <- data.frame(
  uid=c(12,13),
  username=c("kim", "john")
)



#* @apiTitle Modelo del Titanic
#* @apiDescription Este api nos servira para predicir
#* si un pasajero del titanic sobrevive o no


#* Log some information about the incoming request
#* @filter logger
function(req){
  requesee <- req
  print(requesee)
  cat(as.character(Sys.time()), "-",
      req$REQUEST_METHOD, req$PATH_INFO, "-",
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
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



#* @get /tint/<id:int>
function(id){
  list(
    id = id,
    type = typeof(id)
  )
}

#* @get /tbool/<id:bool>
function(id){
  list(
    id = id,
    type = typeof(id)
  )
}

#* @get /tdouble/<id:double>
function(id){
  list(
    id = id,
    type = typeof(id)
  )
}
