
detach("package:psych")
library(ltm)
library(mirt)
library(lordif)

setwd("~/Desktop/FAD_New_Start/2_Registered_Report/2_4_Analayses/2_4_3_Save_points")

CHN_1.1_Clean <- read.csv("CHN_1.1_Cleaned.csv")
CHN_1.2_Clean <- read.csv("CHN_1.2_Cleaned.csv")
CHN_1.3_Clean <- read.csv("CHN_1.3_Cleaned.csv")
ENG_Clean <- read.csv("ENG_Cleaned.csv")
#testENG <- ENG_Clean[which(ENG_Clean[,1]=="ENG3.5"),]
FRN_Clean <- read.csv("FRN_Cleaned.csv")
JPN_1_Clean <- read.csv("JPN_1_Cleaned.csv")
JPN_2_Clean <- read.csv("JPN_2_Cleaned.csv")


FDnames <- c("FD1","FD5","FD9","FD13","FD17")
SDnames <- c("SD2","SD6","SD10","SD14","SD18","SD22","SD24")
UPnames <- c("UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27")
FWnames <- c("FW4","FW8","FW12","FW16","FW21","FW23","FW26")


ggumfad1 <- function(m,dimensionnames) { 
  n <- length(m[1,])
  mdim<- m[,dimensionnames]
  res <- mirt(mdim,1,itemtype = "ggum") #generalized graded unfolding model 
  ressum <- summary(res)
  coefres <- coef(res)
  listcoefres <- unlist(coefres)
  l <- length(listcoefres)
  listcoefres <- listcoefres[-c(l-1,l)]
  coefmatrix <- matrix(listcoefres,nrow = n,byrow = T)
  resfinal <- cbind(ressum[[1]],ressum[[2]],coefmatrix)
  return(resfinal)
}

#little estimation of each dimension with irt using generalized graded unfolding model #TAKES TIME
CHN1.1_FD_ggum <- ggumfad1(CHN_1.1_Clean,FDnames)
CHN1.1_SD_ggum <- ggumfad1(CHN_1.1_Clean,SDnames)
CHN1.1_UP_ggum <- ggumfad1(CHN_1.1_Clean,UPnames)
CHN1.1_FW_ggum <- ggumfad1(CHN_1.1_Clean,FWnames)



forLordif <- function(m1,m2,dimensionnames){
  m1dim <- m1[,dimensionnames]
  m2dim <- m2[,dimensionnames]
  g <- c(rep("0",length(m1dim[,1])),rep("1",length(m2dim[,1])))
  datas <- rbind(m1dim,m2dim)
  reslordif <- lordif(datas, g, criterion = "Chisqr", 
                      pseudo.R2 = c("Nagelkerke"),
                      alpha = 0.05, minCell = 1)
}


###test dif ENGtest
forLordif(JPN_2_Clean,testENG,FDnames) 
forLordif(JPN_2_Clean,testENG,SDnames) #SD 24 JPN1-testENG 
forLordif(JPN_2_Clean,testENG,UPnames) 
forLordif(JPN_2_Clean,testENG,FWnames)

###res CHN1.1-ENG
forLordif(CHN_1.1_Clean,ENG_Clean,FDnames) 
forLordif(CHN_1.1_Clean,ENG_Clean,SDnames) #SD 24 
forLordif(CHN_1.1_Clean,ENG_Clean,UPnames) 
forLordif(CHN_1.1_Clean,ENG_Clean,FWnames)

###res FRN-ENG
forLordif(FRN_Clean,ENG_Clean,FDnames) #        #FD13, 17
forLordif(FRN_Clean,ENG_Clean,SDnames) #        #SD14, 22, 24 
forLordif(FRN_Clean,ENG_Clean,UPnames) #UP3 ,11 #UP7, 11
forLordif(FRN_Clean,ENG_Clean,FWnames) #        #FW8, 16, 23, 26

###res JPN_1-ENG
forLordif(JPN_1_Clean,ENG_Clean,FDnames) 
forLordif(JPN_1_Clean,ENG_Clean,SDnames) #SD 24 
forLordif(JPN_1_Clean,ENG_Clean,UPnames) 
forLordif(JPN_1_Clean,ENG_Clean,FWnames)

###res JPN_2-ENG
forLordif(JPN_2_Clean,ENG_Clean,FDnames) 
forLordif(JPN_2_Clean,ENG_Clean,SDnames)
forLordif(JPN_2_Clean,ENG_Clean,UPnames) 
forLordif(JPN_2_Clean,ENG_Clean,FWnames)
