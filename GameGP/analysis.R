library("readr", lib.loc="~/R/win-library/3.3")
library("data.table")
library(plyr)

setwd("D:/projects/Data Mining/GameGP")

# collecting data
# read the data form csv
data <- read_csv("data/rawdata.csv", col_types = cols(Jour_PR = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
saveRDS(data, "data/GameGP.rds")

dataAll <- readRDS("data/GamePro.rds")

# === functions ======
HNMining.setnames<-function(df){
  setnames(df, "Seniorid","Userid")
  setnames(df, "Jour_PR","Date")
}

HNMining.reduce<- function(df){
  data3 = data.frame(df$Userid, df$Gameid, df$Date)
  colnames(data3) <- c("Userid","Gameid","Date")
  data3 = data3[complete.cases(data3), ]
  return(data3)
}

HNMining.normalizeUser<-function(df){
  #setnames(df, "Seniorid","Userid")
  setnames(df, "Sexe","Sex")
  data3 = data.frame(df$Userid, df$Birthday, df$Sex)
  colnames(data3) <- c("Userid","Birthday","Sex")
  data3 = data3[complete.cases(data3), ]
  
  data3 = data3[userData$Birthday < as.POSIXct("1967-01-01", "UTC"),]
  data3 = data3[userData$Sex != 3,] # remove the obvious inconsistence of sex
  
  return(data3)
}

# ====================

# === main ====
HNMining.setnames(dataAll)
dataStat = HNMining.reduce(dataAll)
saveRDS(dataStat,"data/dataStat.rds")

userData <- read_csv("data/user.csv", col_types = cols(Jour_PR = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
saveRDS(userData, "data/user.rds")
# ==============
# cache
userData = readRDS("data/user.rds")
data3 = readRDS("data/dataStat.rds")

userData = HNMining.normalizeUser(userData)
userData[userData$Birthday < as.POSIXct("1967-01-01", "UTC")]
data2014 = data3[data3$Date > as.POSIXct("2014-01-01", "UTC"), ]

inner = join(userData, data2014, type = "inner")

setnames(data.frame(vUserid_200d),"Userid")
