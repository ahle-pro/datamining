library("readr", lib.loc="~/R/win-library/3.3")
library("data.table")

setwd("D:/projects/Data Mining/GameGP")

# collecting data
# read the data form csv
data <- read_csv("data/rawdata.csv", col_types = cols(Jour_PR = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
saveRDS(data, "data/GameGP.rds")

dataAll <- readRDS("data/GamePro.rds")

# === functions ======
HNMining.setnames<-function(df){
}

HNMining.reduce<- function(df){
}

HNMining.normalizeUser<-function(df){
}

# === main =====
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
inner = join(userData, data2014, type = "inner")
setnames(data.frame(vUserid_200d),"Userid")
