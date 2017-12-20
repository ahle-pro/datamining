# install packages
#install.packages("gridExtra")

# library: always run this
library("readr", lib.loc="~/R/win-library/3.3")
library("data.table")
library(dplyr)
library(utils)
library(gridExtra)
library(grid)

setwd("D:/projects/Data Mining/GameGP")

# count the number of users
summary1<-function(df){
  # declare variables
  ret <- data.frame("Name"=character(), "Value" = character(), stringsAsFactors = FALSE)
  
  # count No. of the users
  nUsers <- length(unique(df$Userid))
  ret[nrow(ret)+1,] <- c("No. of users", prettyNum(nUsers,big.mark=",",scientific=FALSE))
  
  # count No. of the games
  nGames <- length(unique(df$Gameid))
  ret[nrow(ret)+1,] <- c("No. of games", nGames)
  
  # count No. of the exercises
  nMatches <- nrow(df)
  ret[nrow(ret)+1,] <- c("No. of exercises", prettyNum(nMatches,big.mark=",",scientific=FALSE))
  
  # Period
  dates <- range(df$Date,na.rm=TRUE)
  ret[nrow(ret)+1,] <- c("First date", format(dates[1], "%Y-%m-%d"))
  ret[nrow(ret)+1,] <- c("Last date", format(dates[2], "%Y-%m-%d"))
  
  # output
  plot.new()
  grid.draw(grid.table(ret))
}

# get a table grouped by the user
getUser_Matches<-function(df){
  # create a table
  table = data.table(df)# table1: table with less columns from source data
  tUserFreq <- table[,.(.N), by=Userid] # do on all row, produce the count, and group by the first columns
  #tUserFreq <- tUserFreq[order(N)]# table2: data groupeby users
  return(tUserFreq)
}

transpose1<-function(table){
  
  ma = max(table[["N"]])
  mi = min(table[["N"]])
  
  v_nUsers <- vector(mode = "numeric", length = ma) # number of users on a frequence
  
  v_t <- 1:ma
  for(i in 1:ma){
    nUsers <- table[N==i, .N]
    v_nUsers[i] <- nUsers
  }
  # output
  #plot(v_nUsers, main="No. of users on the same no. of training days dist.",sub = subtitle, xlab="No. of training days", ylab="No. of Users")
  return(v_nUsers)
}

getUser_Days<-function(df){

  table = data.table("Userid" = df$Userid, "Date" = df$Date)# transform
  table = unique(table)# filter duplicate
  table1 = table[,.(N = .N,MIN = min (Date, na.rm=TRUE), MAX = max (Date, na.rm=TRUE)), by=Userid]
  table1 = table1[, duration := difftime(MAX, MIN, units = "days")]
  
  #transpose1(table1)
  
  return(table1)
}

# draw a bar for stats
barUsers_Days <-function(vNoDays){
  # Bar:
  max <-length(vNoDays)
  xseq = c(0,1,10,30,60,90,150,200,250,300)
  xtext = c(">0",">1",">10",">30",">60",">90",">150",">200",">250",">300")
  v_nUsers = rep(0,length(xseq))  
  for(i in 1:max){
    for(j in 1:length(xseq)){
      if(vNoDays[i]>xseq[j])
        v_nUsers[j] = v_nUsers[j]+1
    }
  }
  
  xx <- barplot(v_nUsers, names.arg = xtext, main="No. of users by minimum training days dist.", sub= subtitle, xlab="No. of training days", ylab="No. of users")
  text(x = xx, y = v_nUsers, label = v_nUsers, pos = 3, cex = 0.8, col = "blue")
}

getRatioMatchDays<-function(vNoMatches, vNoDays){
  # Question: no. match by day
  vRatioMatchDays <- vNoMatches/vNoDays
  return(vRatioMatchDays)
}

barUsers_Ratio <-function(vRatioMatchDays){
  xseq = c(0,1,6,11,16)
  xtext = c("0-1","1-6","6-11","11-16",">16")
  #xseq = floor(seq(min(outcome),300, length.out = 10))
  ind = findInterval(vRatioMatchDays,xseq)
  ind = sort(ind)
  fTable = table(ind)
  fNames = as.numeric(names(fTable))
  ybar = 1:length(xseq)
  
  for(i in 1:length(xseq)){
    ybar[i] = 0
    if(i %in% fNames){
      ybar[i] <- fTable[match(i, fNames)]
    }
  }
  
  xx <- barplot(ybar, names.arg = xtext, main="No. of users grouped by no. exercises per day dist.", sub=subtitle, xlab="No. of exercises by day", ylab="No. of users")
  text(x = xx, y = ybar, label = ybar, pos = 3, cex = 0.8, col = "blue")
}

barUsers_Period<-function(vUser_Duration){
  xseq = seq(0,365*9,by = 365)
  xtext = c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9",">9")
  ind = findInterval(vUser_Duration,xseq)
  ind = sort(ind)
  fTable = table(ind)
  fNames = as.numeric(names(fTable))
  ybar = 1:length(xseq)
  
  for(i in 1:length(xseq)){
    ybar[i] = 0
    if(i %in% fNames){
      ybar[i] <- fTable[match(i, fNames)]
    }
  }
  
  xx <- barplot(ybar, names.arg = xtext, main="No. of users grouped by usage period dist.", sub=subtitle, xlab="Usage period (year), 1 year = 365 days", ylab="No. of users")
  text(x = xx, y = ybar, label = ybar, pos = 3, cex = 0.8, col = "blue")
}

# reduce the users playing more than 200 days
getUsers_200days <-function(tbUser_Days, tbAll){
  vUsers200 <- tbUser_Days[N>200]$Userid
  tb200d <- tbAll[Userid %in% vUsers200]
  return(tb200d)
}

# ==== main =====
main<-function(){
  # choose a data for analyse
  dataSrc <- readRDS("data/dataStat.rds")
  
  # these parameters should change everytime
  subtitle = "N = 31M obs., db = Game Public"
  pdf("report/GamePublicStats.pdf")
  
  # main process
  summary1(dataSrc)
  tbUser_Matches <-getUser_Matches(dataSrc)
  tbUser_Days<-getUser_Days(dataSrc)
  barUsers_Days(tbUser_Days$N)
  
  vRatioMatchDays<-getRatioMatchDays(tbUser_Matches$N, tbUser_Days$N)
  barUsers_Ratio(vRatioMatchDays)
  dev.off() 
  
  # 200 days
  data200d <- getUsers_200days(tbUser_Days, data.table(dataSrc))
  #saveRDS(data200d, "data/data200d.rds")
  
  subtitle = "N = 16.8M obs., db = Game Public, Users > 200 days"
  pdf("report/200dStats.pdf")
  summary1(data200d)
  tbUser_Matches <-getUser_Matches(data200d)
  tbUser_Days<-getUser_Days(data200d)
  vRatioMatchDays<-getRatioMatchDays(tbUser_Matches$N, tbUser_Days$N)
  barUsers_Ratio(vRatioMatchDays)
  barUsers_Period(tbUser_Days$duration)
  
  dev.off() 
}
# ===============