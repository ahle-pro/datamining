# install packages
#install.packages("gridExtra")

# library: always run this
library("readr", lib.loc="~/R/win-library/3.3")
library("data.table")
library(dplyr)
library(utils)
library(gridExtra)
library(grid)

# ==== functions =====
# count the number of users
HNMining.summaryData<-function(df){
  # declare variables
  ret <- data.frame("Name"=character(), "Value" = character(), stringsAsFactors = FALSE)
  
  # count No. of the users
  nUsers <- length(unique(df$Userid))
  ret[nrow(ret)+1,] <- c("No. of users", nUsers)
  
  # count No. of the games
  nGames <- length(unique(df$Gameid))
  ret[nrow(ret)+1,] <- c("No. of games", nGames)
  
  # count No. of the exercises
  nMatches <- nrow(df)
  ret[nrow(ret)+1,] <- c("No. of exercises", nMatches)
  
  # Period
  dates <- range(df$Date,na.rm=TRUE)
  ret[nrow(ret)+1,] <- c("First date", format(dates[1], "%Y-%m-%d"))
  ret[nrow(ret)+1,] <- c("Last date", format(dates[2], "%Y-%m-%d"))
  
  # output
  plot.new()
  grid.draw(grid.table(ret))
}


HNMining.getUser_X<-function(df){
  # create a table
  table = data.table(df)# table1: table with less columns from source data
  tUserFreq <- table[,.(.N), by=Userid] # do on all row, produce the count, and group by the first columns
  #tUserFreq <- tUserFreq[order(N)]# table2: data groupeby users
  return(tUserFreq)
}
# classify the users based on No. days of training
HNMining.classifyNoDays<-function(vNoDays){
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
  return(v_nUsers)
}

# draw a bar for stats
HNMining.barUsers_Days <-function(v_nUsers, draw=FALSE){
  xtext = c(">0",">1",">10",">30",">60",">90",">150",">200",">250",">300")
  xx <- barplot(v_nUsers, names.arg = xtext, main="No. of users by minimum training days dist.", sub= subtitle, xlab="No. of training days", ylab="No. of users")
  text(x = xx, y = v_nUsers, label = v_nUsers, pos = 3, cex = 0.8, col = "blue")
  
  return(xx)
}


HNMining.getUser_Days<-function(df, draw=FALSE){

  table = data.table("Userid" = df$Userid, "Date" = df$Date)# transform
  table = unique(table)# filter duplicate
  table1 = table[,.(N = .N,MIN = min (Date, na.rm=TRUE), MAX = max (Date, na.rm=TRUE)), by=Userid]
  table1 = table1[, duration := difftime(MAX, MIN, units = "days")]
  
  return(table1)
}



HNMining.getUser_Matches<-function(tUserX, draw=FALSE){
  vMatches = tUserX$N
  if(draw){
    vPlot = sort(copy(vMatches))
    plot(vPlot, main="No. of users on the same no. of exercises dist.",sub=subtitle, xlab="No. of exercises", ylab="No. of users ")
  }
  return(vMatches)
}

HNMining.getRatioMatchDays<-function(vNoMatches, vNoDays){
  
  # Question: no. match by day
  vRatioMatchDays <- vNoMatches/vNoDays
  return(vRatioMatchDays)
}
# draw a bar from matches / days ratio
HNMining.barUsers_Ratio <-function(vRatioMatchDays, draw=FALSE, save=FALSE){
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
  
  if(draw){
    xx <- barplot(ybar, names.arg = xtext, main="No. of users grouped by no. exercises per day dist.", sub=subtitle, xlab="No. of exercises by day", ylab="No. of users")
    text(x = xx, y = ybar, label = ybar, pos = 3, cex = 0.8, col = "blue")
  }
  
  return(ybar)
}

HNMining.statProgram<-function(df, programId=-1, draw = FALSE){
  setwd("D:/projects/Data Mining/GamePro")
  
  if(draw){
    subtitle = "N = 5.2M obs., db = GamePro"
    pdf(paste("report/Programs/Program",programId,".pdf"))
  }
  
  HNMining.summaryData(df)
  tUserX <-HNMining.getUser_X(df)
  vNoDays<-HNMining.getUser_Days(df)
  vNoMatches<-HNMining.getUser_Matches(tUserX)
  vRatioMatchDays<-HNMining.getRatioMatchDays(vNoMatches, vNoDays$N)
  ybar = HNMining.barUsers_Ratio(vRatioMatchDays)
  
  if(draw){
    dev.off()  
  }
  
  return(ybar)
}

# ==== end functions ======

HNMining.main<-function(){
  
  dataP4 <- readRDS("data/dataP4.rds")
  dataP13 <- readRDS("data/dataP13.rds")
  dataP14 <- readRDS("data/dataP14.rds")
  
  #HNMining.summaryData(data3)
  # build 3 vectors
  plot3Programs <- function(dataP4, dataP13, dataP14) {
    
    p4 = HNMining.statProgram(dataP4,"4")
    p13 = HNMining.statProgram(dataP13,"13")
    p14 = HNMining.statProgram(dataP14,"14")
    # save
    dataMatchRatio = cbind(p4,p13,p14)
    # draw
    xtext = c("0-1","1-6","6-11","11-16",">16")
    ybar = rbind(p4,p13,p14)
    xx <- barplot(ybar,col=c("darkblue","red","yellow"),beside = TRUE, names.arg = xtext, legend.text = c("PRESCO","Brain Injury","Psychiatry"), main="No. of users grouped by no. exercises per day dist.", sub="N=5.7M obs., GamePro", xlab="No. of exercises by day", ylab="No. of users")
    text(x = xx, y = ybar, label = ybar, pos = 3, cex = 0.8, col = "blue")
  }
  HNMining.getNoDays3<-function(list){
    p4 = list$p4$N
    p13 = list$p13$N
    p14 = list$p14$N
    
    return(list("p4"=p4,"p13"=p13,"p14"=p14))
  }
  
  HNMining.classifyNoDays3<-function(list){
    p4 = list$p4$N
    p13 = list$p13$N
    p14 = list$p14$N
    
    cp4 = HNMining.classifyNoDays(p4)
    cp13 = HNMining.classifyNoDays(p13)
    cp14 = HNMining.classifyNoDays(p14)
    
    return(list("p4"=cp4,"p13"=cp13,"p14"=cp14))
  }
  
  HNMining.getPercent <- function(vNoDays, draw=FALSE) {
    max = max(vNoDays)
    percent = maply(vNoDays, function(x) x/max)
    return(percent)
  }
  
  HNMining.getPercent3 <- function(list, draw=FALSE){
    p4 = list$p4
    p13 = list$p13
    p14 = list$p14
    
    pp4 = HNMining.getPercent(p4)
    pp13 = HNMining.getPercent(p13)
    pp14 = HNMining.getPercent(p14)
    
    return(list("p4"=pp4,"p13"=pp13,"p14"=pp14))
  }
  # plot 3 Programs in percents
  HNMining.plot3ProgramsPer<-function(list){
      
    p4 = list$p4
    p13 = list$p13
    p14 = list$p14
    # save
    #data = cbind(p4,p13,p14)
    # draw
    xtext = c(">0",">1",">10",">30",">60",">90",">150",">200",">250",">300")
    ybar = rbind(p4,p13,p14)
    xx <-
      barplot(
        ybar,
        col = c("darkblue", "red", "yellow"),
        beside = TRUE,
        names.arg = xtext,
        legend.text = c("PRESCO", "Brain Injury", "Psychiatry"),
        main = "Change of no. of users dist. by training days ",
        sub = "N=5.7M obs., GamePro",
        xlab = "No. of minimum training day",
        ylab = "No. of users (%)"
      )
    #text(x = xx, y = ybar, label = percent(ybar), pos = 3, cex = 0.8, col = "blue")
  }
  
  HNMining.getDiff <- function(vPer) {
    return(abs(diff(vPer)))
  }
  
  HNMining.getDiff3 <- function(listPer, draw=FALSE){
    p4 = listPer$p4
    p13 = listPer$p13
    p14 = listPer$p14
    
    pp4 = HNMining.getDiff(p4)
    pp13 = HNMining.getDiff(p13)
    pp14 = HNMining.getDiff(p14)
    
    return(list("p4"=pp4,"p13"=pp13,"p14"=pp14))
  }
  
  # plot 3 Programs in percents
  HNMining.plot3ProgramsDiff<-function(list){
    
    p4 = list$p4
    p13 = list$p13
    p14 = list$p14
    # save
    #data = cbind(p4,p13,p14)
    # draw
    xtext = c("0-1","1-10","10-30","30-60","60-90","90-150","150-200","200-250","250-300")
    ybar = rbind(p4,p13,p14)
    xx <-
      barplot(
        ybar,
        col = c("darkblue", "red", "yellow"),
        beside = TRUE,
        names.arg = xtext,
        legend.text = c("PRESCO", "Brain Injury", "Psychiatry"),
        main = "Percentage of users dist. by training days ",
        sub = "N=5.7M obs., GamePro",
        xlab = "No. of training days",
        ylab = "No. of users (%)"
      )
    #text(x = xx, y = ybar, label = percent(ybar), pos = 3, cex = 0.8, col = "blue")
  }
}