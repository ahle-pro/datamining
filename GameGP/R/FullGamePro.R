# install packages
#install.packages("gridExtra")

# library: always run this
library("readr", lib.loc="~/R/win-library/3.3")
library("data.table")
library(dplyr)
library(utils)
library(gridExtra)
library(grid)
setwd("D:/projects/Data Mining")


# collecting data
# read the data form csv
# data <- read_csv("data/fullGamePro.csv", col_types = cols(Jour_PR = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
# saveRDS("data/GamePro.rds")
dataSrc <- readRDS("data/GamePro.rds")

# extract the column vectors
df_senior <- dataSrc[2] # senior is a list
df_gameId <- dataSrc[3] # gameId is a 
df_jour <- dataSrc["Jour_PR"]

data3 = data.frame(df_senior, df_gameId, df_jour)

# count the number of users
df_u_senior <- unique(df_senior) # df u senior
cat("The unique user number is ", nrow(df_u_senior))
v_senior = c(t(df_senior)) # vector of users

nbUsers = nrow(df_u_senior)
#max(df_u_senior)
#min(df_u_senior)
#plot(df_senior, main="The distribution of users ", xlab="User Id")
#hist(v_senior, xlab = "user group")

# statistics on gameid
u_gameId <- unique(df_gameId)
nbGame = nrow(u_gameId)
#max(u_gameId)
#minx(u_gameId)

#v_gameId = c(t(gameId)) # vector of matches
#plot(v_gameId, main="The distribution of played games ", xlab="Game Id")

# create a table
tbAll = data.table(data3)# table1: table with less columns from source data
tUserFreq <- tbAll[,.(.N), by=Seniorid] # do on all row, produce the count, and group by the first columns
tUserFreq <- tUserFreq[order(N)]# table2: data groupeby users

max <-max(tUserFreq$N) # max of the matches by user
v_nUsers <- vector(mode = "numeric", length = max) # number of users on a frequence
v_freq <- 1:max
for(i in 1:max){
  nUsers <- table2[N==i, .N]
  v_nUsers[i] <- nUsers
}

# show the users on each frequence
plot(v_nUsers, main="The users on each frequence",sub="N = 5.8M obs., db = GamePro", xlab="Frequence", ylab="Users Per Frequence")

# print the matches on user
v_freq_user = sort(table2$N)
plot(v_freq_user, main="The matches on each user", xlab="User", ylab="Matches Per User")
median1 <- round(median(1:length(v_freq_user)))
points(median1, v_freq_user[median1], col="blue", pch=19, cex=4)

# Jour
tbUserDays = data.table(data.frame(df_senior, df_jour))# transform
tbUserDays = unique(tbUserDays)# filter duplicate
tbUser_Days = tbUserDays[,.(N = .N,MIN = min (Jour_PR, na.rm=TRUE), MAX = max (Jour_PR, na.rm=TRUE)), by=Seniorid]
tbUser_Days = tbUser_Days[, duration := difftime(MAX, MIN, units = "days")]

# Initial Time vs. Last Time
pdf("report/GamePro_cmp_first_last_user.pdf")
# Start time
plot(sort(tbUser_Days$MIN), main="First match per user",sub="N = 5.8M obs., db = GamePro", xlab = "User (ith)", ylab = "First match (Date)")
# End time
plot(sort(tbUser_Days$MAX), main="Last match per user",sub="N = 5.8M obs., db = GamePro", xlab = "User (ith)", ylab = "Last match (Date)")

dev.off()

pdf("report/GamePro_cmp_period_days_user.pdf")
old.par <- par(mfrow=c(1, 2))
# Playing days
plot(sort(tbUser_Days$N), main="Playing days per user",sub="N = 5.8M obs., db = GamePro", xlab = "User (ith)", ylab = "Playing days (days)")
# Playing period
plot(sort(tbUser_Days$duration), main="Playing period per user",sub="N = 5.8M obs., db = GamePro", xlab = "User (ith)", ylab = "Playing period (days)")
par(old.par)

dev.off()

# The statistics on user days
# use join
max <-max(tbUser_Days$N) # max of days by user
v_nDays <- 1:max
for(i in 1:max){
  nUsers <- tbUser_Days[N==i, .N]
  v_nDays[i] <- nUsers
}
# show the users on each number of days
plot(v_nDays, main="The users on each number of days",sub="N = 5.8M obs., db = GamePro", xlab="Number of days", ylab="Users")

# Remark 1: We need only first 100 jours
plot(v_nDays[1:90], main="The users on each number of days (< 90 days)",sub="N = 5.8M obs., db = GamePro", xlab="Number of days", ylab="Users")

# Bar:
max <-length(tbUser_Days$N)
xseq = c(0,1,30,60,90,150,300)
xtext = c(">0",">1",">30",">60",">90",">150",">300")
v_nUsers = rep(0,length(xseq))  
for(i in 1:max){
  for(j in 1:length(xseq)){
    if(tbUser_Days$N[i]>xseq[j])
      v_nUsers[j] = v_nUsers[j]+1
  }  
}

xx <- barplot(v_nUsers, names.arg = xtext, main="No. of users by minimum playing days distr.", sub="N = 5.8M obs., db = GamePro", xlab="No. of playing days", ylab="No. of users")
text(x = xx, y = v_nUsers, label = v_nUsers, pos = 3, cex = 0.8, col = "blue")



# Remark 2: view the table 
len <-length(v_nDays)
tbSumNoUser = matrix(c(sum(v_nDays[1:len]),sum(v_nDays[30:len]),sum(v_nDays[60:len]), sum(v_nDays[90:len]), sum(v_nDays[150:len]),sum(v_nDays[300:len])), ncol=1)
colnames(tbSumNoUser) <- c("No. of users")
rownames(tbSumNoUser) <- c(" > 1 jour", " > 30 jours","> 60 jours","> 90 jours", "> 150 jours", "> 300 jours")


pdf("report/GamePro_getNoUserByDays.pdf")
plot.new()
grid.table(tbSumNoUser)
title("The classification of No. of users per playing days")
summary(tbSumNoUser)
dev.off()

# Classification of observations based on the user
# data$Seniorid <-factor(data$Seniorid)

# ==== Task3: 90j ====
# 

vUsers90 <- tbUser_Days[N>90]$Seniorid
tb90d <- tbAll[Seniorid %in% vUsers90]
#saveRDS(tb90d,"data/tb90d.rds")
tb90d<-readRDS("data/tb90d.rds")

tmp<-copy(tb90d)
tbUserDays = unique(tmp[, Gameid:=NULL])
tbUser_Days = tbUserDays[,.(N = .N,MIN = min (Jour_PR, na.rm=TRUE), MAX = max (Jour_PR, na.rm=TRUE)), by=Seniorid]
tbUser_Days = tbUser_Days[, duration := difftime(MAX, MIN, units = "days")]
rm(tmp)



# Question: How long they played ?
plot(sort(tbUser_Days$duration), main="Playing period by user ",sub="N = 1.7M obs., User >90 days, db = GamePro", ylab="Playing period by user", xlab="User(each), from low to high")

# Question: no. match by user
tmp<-copy(tb90d)
tbUserMatchs = tmp[, Gameid:=NULL]
tbUser_Matchs = tbUserMatchs[,.N , by=Seniorid]
rm(tmp)
plot(sort(tbUser_Matchs$N), main="No. of matches ",sub="N = 1.7M obs., User >90 days, db = GamePro", ylab="No. of matches by user", xlab="User(each), from low to high frequence")

# Question: How often they play ?
vRatioDaysByMonth <- tbUser_Days$N/as.numeric(tbUser_Days$duration)*30
plot(sort(vRatioDaysByMonth), main="No. of days by month (30 days) ",sub="N = 1.7M obs., User >90 days, db = GamePro", ylab="No. of days by month", xlab="User(each), from low to high frequence")

# Question: no. match by day
vRatioMatchDays <- tbUser_Matchs$N/tbUser_Days$N

plot(sort(vRatioMatchDays), main="No. of matches by day ",sub="N = 1.7M obs., User >90 days, db = GamePro", ylab="No. of matches by day", xlab="User(each), from low to high frequence")

xseq = c(0,1,6,11,16)
xtext = c("0-1","1-6","6-10","11-15",">16")
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

xx <- barplot(ybar, names.arg = xtext, main="No. of matches by day on each user dist.", sub="N = 1.7M obs., User >90 days, db = GamePro", xlab="No. of matches by day", ylab="No. of users")
text(x = xx, y = ybar, label = ybar, pos = 3, cex = 0.8, col = "blue")

# Plot a table
plot.new()
grid.draw(gtable(summary(cars)))
title("The classification of No. of users per playing days")

# ens

compute<-function(table){
  table = table[,.(N = .N,MIN = min (Jour_PR, na.rm=TRUE), MAX = max (Jour_PR, na.rm=TRUE)), by=Seniorid]
  table = table[, duration := difftime(MAX, MIN, units = "days")]
  # 90 days
  plot(table$MIN)
}


