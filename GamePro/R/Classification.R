library("readr", lib.loc="~/R/win-library/3.3")
library("data.table")
library(dplyr)

HNMining.getPrograms_X<-function(df, program=-1, isPlot=FALSE){
  
  # create a table
  table = data.table(df)# table1: table with less columns from source data
  table <- table[,.(.N), by=Programid] # do on all row, produce the count, and group by the first columns
  #tUserFreq <- tUserFreq[order(N)]# table2: data groupeby users
  if(isPlot){
    plot(tbPrograms, main="No. of exercises on each program dist.",sub=subtitle, xlab="Programid", ylab="No. of exercises")
  }
  
  return(table)
}


HNMining.tabularizePrograms<-function(table){
  # declare variables
  ret <- data.frame("Name"=character(), "Value" = character(), stringsAsFactors = FALSE)
  
  # count No. of the programs
  ret[nrow(ret)+1,] <- c("No. of programs", nrow(table))
  
  #sort
  table = table[order(-N)]
  
  # output
  plot.new()
  grid.draw(grid.table(ret))

  #output
  tb_head = head(table)
  colnames(tb_head)[2] <- "No. of exercises"
  
  plot.new()
  grid.draw(grid.table(tb_head))
}

HNMining.classify <- function(data, save = FALSE){
  
  dataP4 = data[data$Programid == 4, ]
  dataP13 = data[data$Programid == 13, ]
  dataP14 = data[data$Programid == 14, ]
  
  if(save){
    saveRDS(dataP4, "data/dataP4.rds")
    saveRDS(dataP13, "data/dataP13.rds")
    saveRDS(dataP14, "data/dataP14.rds")
  }
}

HNMining.main<-function(){
  # global variables
  env <- new.env(parent=emptyenv())
  env$subtitle = "N = 5.8M obs., db = GamePro"
  setwd("D:/projects/Data Mining")
  
  dataSrc <- readRDS("data/dataStat.rds")
  
  pdf("report/program_classif.pdf")
  
  tbPrograms = HNMining.getPrograms_X(dataSrc)
  
  HNMining.tabularizePrograms(tbPrograms)
  
  dev.off()
}

