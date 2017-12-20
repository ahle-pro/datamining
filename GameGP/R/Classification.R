library("readr", lib.loc="~/R/win-library/3.3")
library("data.table")
library(dplyr)
setwd("D:/projects/Data Mining")

dataSrc <- readRDS("data/dataStat.rds")

pdf("report/program_classif.pdf")
getPrograms_X<-function(df){
  # create a table
  table = data.table(df)# table1: table with less columns from source data
  table <- table[,.(.N), by=Programid] # do on all row, produce the count, and group by the first columns
  #tUserFreq <- tUserFreq[order(N)]# table2: data groupeby users
  plot(tbPrograms, main="No. of exercises on each program dist.",sub="N = 5.8M obs., db = GamePro", xlab="Programid", ylab="No. of exercises")
  
  return(table)
}

tbPrograms = getPrograms_X(dataSrc)

tabular1<-function(table){
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

tabular1(tbPrograms)

dev.off()

build_save_data <- function(){
  
  dataP4 = dataSrc[dataSrc$Programid == 4, ]
  
  saveRDS(dataP4, "data/dataP4.rds")
  
  dataP13 = dataSrc[dataSrc$Programid == 13, ]
  
  saveRDS(dataP13, "data/dataP13.rds")
  
  dataP14 = dataSrc[dataSrc$Programid == 14, ]
  
  saveRDS(dataP14, "data/dataP14.rds")
}
#build_save_data()



