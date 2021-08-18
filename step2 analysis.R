#################################################################
#################################################################
##### STEP 2 ####################################################
##### Basic analysis step with traditional process ##############
#################################################################
#################################################################

rm(list = ls())
setwd("~/Documents/GitHub/FAD_New_Start/DATA") #setwa("*****/GitHub/FAD_New_Start/DATA") 

#install.packages("CTT")
library("CTT")
library("dplyr")
library("psych")
library("lavaan")
library("semPlot")
library("semTools")


D1 <- read.csv("fads_all_1.csv")
D2 <- read.csv("fads_all_2.csv")
D3 <- read.csv("fads_all_3.csv")
DA <- read.csv("fads_all_A.csv")
DF <- read.csv("fads_all_F.csv")
DJ <- read.csv("fads_all_J.csv")

alldata <- list(D1,D2,D3,DA,DF,DJ)

#extract descriptions for each variable
fordescriptions <- function(m){
  des <- apply(m,2,psych::describe)
  return(des)
}

descriptionsall <- lapply(alldata, fordescriptions)

#extract frecuency for each variable
forfrecuencies <- function(m){
  fre <- apply(m, 2, table)
  return(fre)
}
tablesall <- lapply(alldata,forfrecuencies)


fad1 <- read.csv("fads_only_1.csv")
fad2 <- read.csv("fads_only_2.csv")
fad3 <- read.csv("fads_only_3.csv")
fadA <- read.csv("fads_only_A.csv")
fadF <- read.csv("fads_only_F.csv")
fadJ <- read.csv("fads_only_J.csv")

allfad <- list(fad1,fad2,fad3,fadA,fadF,fadJ) #only FAD+ related data

########################################
########################################
############  RELIABILITY ##############
########################################
########################################
fadnames <- c("FD1","FD5","FD9","FD13","FD17",
              "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
              "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
              "FW4","FW8","FW12","FW16","FW21","FW23","FW26")

FDnames <- c("FD1","FD5","FD9","FD13","FD17")
SDnames <- c("SD2","SD6","SD10","SD14","SD18","SD22","SD24")
UPnames <- c("UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27")
FWnames <- c("FW4","FW8","FW12","FW16","FW21","FW23","FW26")


#function for whole and 4 dimensions reliability with alpha & omega
alphasandomegas <- function(m){ 
  fadnames <- c("FD1","FD5","FD9","FD13","FD17",
                "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  alpha0 <- (CTT::reliability(m[,fadnames]))$alpha
  omega0 <- (psych::omega(m[,fadnames]))$omega.tot
  FDnames <- c("FD1","FD5","FD9","FD13","FD17")
  SDnames <- c("SD2","SD6","SD10","SD14","SD18","SD22","SD24")
  UPnames <- c("UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27")
  FWnames <- c("FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  alphafd <- (CTT::reliability(m[,FDnames]))$alpha
  alphasd <- (CTT::reliability(m[,SDnames]))$alpha
  alphaup <- (CTT::reliability(m[,UPnames]))$alpha
  alphafw <- (CTT::reliability(m[,FWnames]))$alpha
  omegafd <- (psych::omega(m[,FDnames]))$omega.tot
  omegasd <- (psych::omega(m[,SDnames]))$omega.tot
  omegaup <- (psych::omega(m[,UPnames]))$omega.tot
  omegafw <- (psych::omega(m[,FWnames]))$omega.tot
  res <- cbind(alpha0,alphafd,alphasd,alphaup,alphafw,
               omega0,omegafd,omegasd,omegaup,omegafw)
  return(res)
}
alphaandomega <- lapply(allfad,alphasandomegas)
reliabilities <- matrix(unlist(alphaandomega),nrow = 6,byrow = T) 
colnames(reliabilities) <- c("alpha","alphaFD","alphaSD","alphaUP","alphaFW",
                             "omega","omegaFD","omegaSD","omegaUP","omegaFW")
rownames(reliabilities) <- c("FAD_1","FAD_2","FAD_3","FAD_A","FAD_F","FAD_J")
write.table(reliabilities,"alphasandomegas.txt")


########################################
########################################
############  CORRELATIONS #############
########################################
########################################

#function for correlations between 4 dimensions and all items in FAD+
correlations <- function(m){
  usefulnames <- c("FD","SD","UP","FW",
                   "FD1","FD5","FD9","FD13","FD17",
                   "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                   "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                   "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  relations <- cor(m[,usefulnames])
  correlationsto01 <- function(v){
    v_abs <- abs(v)
    cuts <- cut(v_abs,breaks = c(0,0.5,1),labels=c(0,1))
    return(cuts)
  }
  c01 <- apply(relations,1,correlationsto01)
  return(cbind(relations,c01))
}

correlationsres <- lapply(allfad,correlations)

#save correlation matrix for each dataset
for (i in 1:6) {
  textnames <- paste("correlations",i,".txt",sep = "")
  write.table(correlationsres[[i]],textnames)
}


###########################
###########################
##########  CFA  ##########
###########################
###########################

#function for CFA
docfa <- function(m){
  model <-  'FD =~ FD1 + FD5 + FD9 + FD13 + FD17;
SD =~ SD2 + SD6 + SD10 + SD14 + SD18 + SD22 + SD24;
  UP =~ UP3 + UP7 + UP11 + UP15 + UP19 + UP20 + UP25 + UP27;
  FW =~ FW4 + FW8 + FW12 + FW16 + FW21 + FW23 + FW26'
  fadnames <- c("FD1","FD5","FD9","FD13","FD17",
                "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  m <- m[,fadnames]
  rescfa <- lavaan::cfa(model,m,estimator="ml")
  semPlot::semPaths(rescfa,"std",rotation = 2,layout = "tree2",nCharNodes = 0,
                    sizeLat = 15, sizeLat2 = 7, label.norm = "OOOOO", 
                    mar=c(2,6,2,4), curvePivot = TRUE,
                    edge.label.cex=1.2, residuals = F)
  res <- standardizedSolution(rescfa)
  return(res)
}

cfares <- lapply(allfad, docfa)

#save CFA results for each dataset
for (i in 1:6) {
  textnames <- paste("cfas",i,".txt",sep = "")
  write.table(cfares[[i]],textnames)
}



#the possible model based on FAD_1
# model1 <- 'FD =~ FD1 + FD5 + FD9 + FD13 + FD17;
# SD1 =~ SD6 + SD22;
# GEN =~ SD2 + SD10 + SD18;
# ENV =~ SD14 +SD24;
# UP =~ UP3 + UP7 + UP11 + UP15 + UP19 + UP20 + UP25 + UP27;
# FW =~ FW4 + FW8 + FW12 + FW16 + FW21 + FW23 + FW26'

########################################
########################################
###### Measurement Invariance ##########
######### CFA MULTIGROUP ###############
########################################
########################################

#function for measurement invariance
MI <- function(m1,m2){
  fadnames <- c("FD1","FD5","FD9","FD13","FD17",
                "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  model <-  'FD =~ FD1 + FD5 + FD9 + FD13 + FD17;
  SD =~ SD2 + SD6 + SD10 + SD14 + SD18 + SD22 + SD24;
  UP =~ UP3 + UP7 + UP11 + UP15 + UP19 + UP20 + UP25 + UP27;
  FW =~ FW4 + FW8 + FW12 + FW16 + FW21 + FW23 + FW26'
  m1fad <- m1[,fadnames]
  m2fad <- m2[,fadnames]
  g <- c(rep("0",length(m1[,1])),rep("1",length(m2[,1])))
  datas <- cbind(rbind(m1fad,m2fad),g)
  datas$g <- factor(datas$g,levels = c("0","1"))
  
  config <- cfa(model, datas, group="g")  
  weak <- cfa(model, datas, group="g", group.equal="loadings") 
  strong<- cfa(model, datas, group="g", group.equal= c("loadings", "intercepts"))
  strict<- cfa(model, datas, group="g", 
               group.equal= c("loadings", "intercepts", "residuals"))
  res1 <- anova(config, weak, strong, strict)
  res2 <- measurementInvariance(model=model, data=datas, group="g")
  return(c(res1,res2))
}


#eg test measurement invariance bewteen fad2-English, French-English, Japanese-English
MI(fad2,fadA)
MI(fadA,fadF)
MI(fadA,fadJ)




#######################################
#######################################
############ CORRELATION ##############
############ with BIG 5 ###############
#######################################
#######################################

#select cases with BFI data
BFI_NAlocated <- which(is.na(D2[,"BFI_A1"]))
forBFI_FAD <- D2[-BFI_NAlocated,]
FAD_BFI <- forBFI_FAD[,c("FD","SD","UP","FW",fadnames)]

#function for BIG5 5 dimensions' scores
BFIS <- function(datos,nombre,key){
  databfi <- datos[,nombre]
  data_corrected <- matrix(t(apply(databfi,1,function(x){x*key})),ncol = length(key))
  datafinal <- na.exclude(data_corrected)
  bfiscores <- apply(datafinal, 1, function(x){sum(x)/length(key)})
  return(bfiscores)
}  

# This BFI scores part base on Qinglan's code
BFI_ANames <- c("BFI_A1","BFI_A2","BFI_A3","BFI_A4","BFI_A5","BFI_A6","BFI_A7","BFI_A8","BFI_A9")
BFI_AKeys <- c(1,1,1,1,1,-1,-1,-1,-1) 
BFIA <- BFIS(D2,BFI_ANames,BFI_AKeys)
length(BFIA)

BFI_CNames <- c("BFI_C1","BFI_C2","BFI_C3","BFI_C4","BFI_C5","BFI_C6","BFI_C7","BFI_C8","BFI_C9")
BFI_CKeys <- c(1,1,1,1,1,-1,-1,-1,-1)
BFIC <- BFIS(D2,BFI_CNames,BFI_CKeys)
length(BFIC)

BFI_NNames <- c("BFI_N1","BFI_N2","BFI_N3","BFI_N4","BFI_N5","BFI_N6","BFI_N7","BFI_N8")
BFI_NKeys <- c(1,1,1,1,1,-1,-1,-1)
BFIN <- BFIS(D2,BFI_NNames,BFI_NKeys)
length(BFIN)

BFI_ONames <- c("BFI_O1","BFI_O2","BFI_O3","BFI_O4","BFI_O5","BFI_O6","BFI_O7","BFI_O8","BFI_O9","BFI_O10")
BFI_OKeys <- c(1,1,1,1,1,1,1,1,1,1)
BFIO <- BFIS(D2,BFI_ONames,BFI_OKeys)
length(BFIO)


BFI_ENames <- c("BFI_E1","BFI_E2","BFI_E3","BFI_E4","BFI_E5","BFI_E6","BFI_E7","BFI_E8")
BFI_EKeys <- c(1,1,1,1,1,1,1,1)
BFIE <- BFIS(D2,BFI_ENames,BFI_EKeys)
length(BFIE)

BFIS <- cbind(BFIA,BFIC,BFIN,BFIO,BFIE)

BFI_FAD_Final <- cbind(BFIS,FAD_BFI)
corBFI_FAD <- cor(BFI_FAD_Final[,1:9])
write.table(corBFI_FAD,"BFI_FAD.txt") #save correlation martix with BFI 5 dimensions


corBFI_FAD[which(abs(corBFI_FAD)<1 & abs(corBFI_FAD)>0.6)] 

corBFI_FAD27 <- cor(BFI_FAD_Final[,-c(6,7,8,9)]) #correlations between BFI 5 dimensions with FAD+ 27 items
corBFI_FAD27[which(abs(corBFI_FAD27)<1 & abs(corBFI_FAD27)>0.6)] #FAD1-9

#######################################
#######################################
############ CORRELATION ##############
############ with MLOC ################
#######################################
#######################################

#select cases with MLOC data
MLOC_NAlocated <- which(is.na(D2[,"MLOC1"]))
forMLOC_FAD <- D2[-MLOC_NAlocated,]
FAD_MLOC <- forMLOC_FAD[,c("FD","SD","UP","FW",fadnames)]

#calculate MLOC scores
MLOC_INames <- c("MLOC1","MLOC4","MLOC5","MLOC9","MLOC18","MLOC19","MLOC21","MLOC23")
MLOC_I <- apply(forMLOC_FAD[,MLOC_INames]+3,1,sum)

MLOC_PNames <- c("MLOC3","MLOC8","MLOC11","MLOC13","MLOC15","MLOC17","MLOC20","MLOC22")
MLOC_P <- apply(forMLOC_FAD[,MLOC_PNames]+3,1,sum)


MLOC_CNames <- c("MLOC2","MLOC6","MLOC7","MLOC10","MLOC12","MLOC14","MLOC16","MLOC24")
MLOC_C <- apply(forMLOC_FAD[,MLOC_CNames]+3,1,sum) 

MLOCS <- cbind(MLOC_I,MLOC_P,MLOC_C)

MLOC_FAD_Final <- cbind(MLOCS,FAD_MLOC)
corMLOC_FAD <- cor(MLOC_FAD_Final[,1:7])
write.table(corMLOC_FAD,"MLOC_FAD.txt") #save correlation matrix with 3 factor of MLOC


corMLOC_FAD[which(abs(corMLOC_FAD)<1 & abs(corMLOC_FAD)>0.6)] #low correlations bewteen MLOC & FAD; but, noted MLOC_P*MLOC_C

corMLOC_FAD27 <- cor(MLOC_FAD_Final[,-c(4,5,6,7)]) #correlations between MLOC 3 dimensions with FAD+ 27 items
corMLOC_FAD27[which(abs(corMLOC_FAD27)<1 & abs(corMLOC_FAD27)>0.6)]

