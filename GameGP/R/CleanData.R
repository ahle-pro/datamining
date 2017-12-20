library("data.table")

setwd("D:/projects/Data Mining")

# collecting data
# read the data form csv
data <- read_csv("data/gp/rawdata.csv", col_types = cols(Jour_PR = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
saveRDS(data, "data/gp/GameGP.rds")

dataSrc <- readRDS("data/GamePro.rds")


dataAll <- readRDS("data/GamePro.rds")

HNMining.setnames<-function(df){
  setnames(df, "Seniorid","Userid")
  setnames(df, "Jour_PR","Date")
}

HNMining.reduce<- function(df){
  data3 = data.frame(df$Userid, df$Gameid, df$Date, df$Programid)
  colnames(data3) <- c("Userid","Gameid","Date","Programid")
  data3 = data3[complete.cases(data3), ]
  return(data3)
}

HNMining.clean<-function(df){
  HNMining.setnames(df)
  dataStat = HNMining.reduce(df)
  
  # save the work
  saveRDS(dataStat,"data/dataStat.rds")
}