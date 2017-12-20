library("data.table")

# ==== functions =====
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
  return(dataStat)
}
# ==== end functions ====

HNMining.main<-function(){
  setwd("D:/projects/Data Mining")
  
  dataAll <- readRDS("data/GamePro.rds")
  
  dataStat = HNMining.clean(dataAll)
  
  saveRDS(dataStat,"data/dataStat.rds")
}