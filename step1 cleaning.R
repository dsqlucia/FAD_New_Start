rm(list = ls())

library("psych")

testdata <- read.csv("fadallbynow.csv")
head(testdata)

fadnames <- c("FD1","FD5","FD9","FD13","FD17",
              "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
              "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
              "FW4","FW8","FW12","FW16","FW21","FW23","FW26")

faddata <- testdata[,fadnames]
head(faddata)

apply(faddata, 2, psych::describe)
anyNA(faddata)
length(which(is.na(faddata)))

cleaning01 <-function(v){   
  if (length(v!=27)) res <- 0 
  if (anyNA(v)==T) res <- 0
  else {fretable <- table(v)
       frenvm <- length(fretable)
        if (frenvm<=1) res <- 0
        else res <- 1}
  return(res)
}

stayornot <- apply(faddata,1,cleaning01) # res==0 --> any NA| case with duplicated; res==1 --> case could stay
table(stayornot)

stayMARKEDall <- cbind(testdata,stayornot)
cleanall <- stayMARKEDall[which(stayornot==1),]
not <- stayMARKEDall[which(stayornot==0),] #recheck cases with duplicated or NA

cleanfad <- cleanall[,fadnames]

length(cleanall[,1])

FADScores <- function(v){
  FDNames <- c("FD1","FD5","FD9","FD13", "FD17")
  SDNames <- c("SD2","SD6","SD10","SD14","SD18","SD22","SD24")
  UPNames <- c("UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27")
  FWNames <- c("FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  FD <- sum(v[FDNames])/length(FDNames)
  SD <- sum(v[SDNames])/length(SDNames)
  UP <- sum(v[UPNames])/length(UPNames)
  FW <- sum(v[FWNames])/length(FWNames)
  Socres <- cbind(FD,SD,UP,FW)
  return(Socres)
}

fadscores <- t(apply(cleanfad, 1, FADScores)) #calculate the 4 dimensions´ scores
colnames(fadscores) <- c("FD","SD","UP","FW")
cleanfadscores <- cbind(fadscores,cleanfad)
head(cleanfadscores)

CLEANfad <- cbind(cleanall[1:5],fadscores,cleanall[6:(length(cleanall)-1)])
head(CLEANfad)
write.csv(CLEANfad,"cleanfadall.csv",row.names=FALSE,fileEncoding ="UTF-8")
