
#################################################################
#################################################################
##### STEP 1 ####################################################
##### Clean for the omissions & cases with same responses #######
##### Calculate FAD+ Scores #####################################
#################################################################
#################################################################


rm(list = ls())
setwd("~/Documents/GitHub/FAD_New_Start/DATA") #setwa("*****/GitHub/FAD_New_Start/DATA") 

library("psych")

data1 <- read.csv("NEW_FAD_1.csv")
data2 <- read.csv("NEW_FAD_2.csv")
data3 <- read.csv("NEW_FAD_3.csv")
dataA <- read.csv("NEW_FAD_A.csv")
dataF <- read.csv("NEW_FAD_F.csv")
dataJ <- read.csv("NEW_FAD_J.csv")

dataall <- list(data1,data2,data3,dataA,dataF,dataJ)

fadnames <- c("FD1","FD5","FD9","FD13","FD17",
              "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
              "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
              "FW4","FW8","FW12","FW16","FW21","FW23","FW26")

bfinames <- c("BFI_A1","BFI_A2","BFI_A3","BFI_A4","BFI_A5","BFI_A6","BFI_A7","BFI_A8","BFI_A9",
              "BFI_C1","BFI_C2","BFI_C3","BFI_C4","BFI_C5","BFI_C6","BFI_C7","BFI_C8","BFI_C9",
              "BFI_N1","BFI_N2","BFI_N3","BFI_N4","BFI_N5","BFI_N6","BFI_N7","BFI_N8",
              "BFI_O1","BFI_O2","BFI_O3","BFI_O4","BFI_O5","BFI_O6","BFI_O7","BFI_O8","BFI_O9","BFI_O10",
              "BFI_E1","BFI_E2","BFI_E3","BFI_E4","BFI_E5","BFI_E6","BFI_E7","BFI_E8")

mlocnames <-  c("MLOC1","MLOC4","MLOC5","MLOC9","MLOC18","MLOC19","MLOC21","MLOC23",
"MLOC3","MLOC8","MLOC11","MLOC13","MLOC15","MLOC17","MLOC20","MLOC22",
"MLOC2","MLOC6","MLOC7","MLOC10","MLOC12","MLOC14","MLOC16","MLOC24")

#function for deleting omission & same responses cases
cleaning <- function(m){
  fadnames <- c("FD1","FD5","FD9","FD13","FD17",
                "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  mfad <- m[,fadnames]
  cleaning01 <-function(v){   
    if (length(v!=27)) res <- 0 
    if (anyNA(v)==T) res <- 0 #detect cases with NA
    else {fretable <- table(v) #detect cases with same responses
    frenvm <- length(fretable)
    if (frenvm<=1) res <- 0
    else res <- 1}
    return(res)
  }
  stayornot <- apply(mfad, 1, cleaning01)
  staymarked <- cbind(m,stayornot)
  cleaned <- staymarked[which(stayornot==1),1:(length(m[1,]))]
  return(cleaned)
}

#cleaning with whole list data
cleaneddataall <- lapply(dataall,cleaning)


#function for calculating FAD+ Scores
scorescalculate <- function(m){
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
  fad4scores <- t(apply(m,1,FADScores))
  colnames(fad4scores) <- c("FD","SD","UP","FW")
  allwithscores <- cbind(m,fad4scores)
  return(allwithscores)
}
#calculating for whole list data based on cleaned data
cleaneddataallwithscores <- lapply(cleaneddataall, scorescalculate)

write.csv(cleaneddataallwithscores[[1]],"fads_all_1.csv",row.names=FALSE)
write.csv(cleaneddataallwithscores[[2]],"fads_all_2.csv",row.names=FALSE)
write.csv(cleaneddataallwithscores[[3]],"fads_all_3.csv",row.names=FALSE)
write.csv(cleaneddataallwithscores[[4]],"fads_all_A.csv",row.names=FALSE)
write.csv(cleaneddataallwithscores[[5]],"fads_all_F.csv",row.names=FALSE)
write.csv(cleaneddataallwithscores[[6]],"fads_all_J.csv",row.names=FALSE)

#extract only FAD+ related data, with dataset marked, 4 dimensions scores, and FAD+ items responses
takethefads <- function(m){
  usefulnames <- c("NO","FD","SD","UP","FW",
                   "FD1","FD5","FD9","FD13","FD17",
                   "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                   "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                   "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  fads <- m[,usefulnames]
  return(fads)
}

fadsall <- lapply(cleaneddataallwithscores, takethefads)

write.csv(fadsall[[1]],"fads_only_1.csv",row.names=FALSE)
write.csv(fadsall[[2]],"fads_only_2.csv",row.names=FALSE)
write.csv(fadsall[[3]],"fads_only_3.csv",row.names=FALSE)
write.csv(fadsall[[4]],"fads_only_A.csv",row.names=FALSE)
write.csv(fadsall[[5]],"fads_only_F.csv",row.names=FALSE)
write.csv(fadsall[[6]],"fads_only_J.csv",row.names=FALSE)







