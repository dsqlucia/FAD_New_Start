rm(list = ls())
#setwd("~/Desktop/FAD_New_Start/2_Registered_Report/2_4_Analayses/2_4_3_Save_points")

#install.packages("CTT")
library("CTT")
library("dplyr")
library("psych")
library("lavaan")
library("semPlot")
library("semTools")

CHN_1.1 <- read.csv("CHN_1.1.csv")
CHN_1.2 <- read.csv("CHN_1.2.csv")
CHN_1.3 <- read.csv("CHN_1.3.csv")
ENG <- read.csv("ENG.csv")
FRN <- read.csv("FRN.csv")
JPN_1 <- read.csv("JPN_1.csv")
JPN_2 <- read.csv("JPN_2.csv")

ALLdata <- list(CHN_1.1,CHN_1.2,CHN_1.3,ENG,FRN,JPN_1,JPN_2)

#dim(ENG)

fadnames <- c("FD1","FD5","FD9","FD13","FD17",
              "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
              "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
              "FW4","FW8","FW12","FW16","FW21","FW23","FW26")

names_BFI_CHN <- c("BFI_A1","BFI_A2","BFI_A3","BFI_A4","BFI_A5","BFI_A6","BFI_A7","BFI_A8","BFI_A9",
              "BFI_C1","BFI_C2","BFI_C3","BFI_C4","BFI_C5","BFI_C6","BFI_C7","BFI_C8","BFI_C9",
              "BFI_N1","BFI_N2","BFI_N3","BFI_N4","BFI_N5","BFI_N6","BFI_N7","BFI_N8",
              "BFI_O1","BFI_O2","BFI_O3","BFI_O4","BFI_O5","BFI_O6","BFI_O7","BFI_O8","BFI_O9","BFI_O10",
              "BFI_E1","BFI_E2","BFI_E3","BFI_E4","BFI_E5","BFI_E6","BFI_E7","BFI_E8")

names_MLOC_CHN <-  c("MLOC1","MLOC4","MLOC5","MLOC9","MLOC18","MLOC19","MLOC21","MLOC23",
                "MLOC3","MLOC8","MLOC11","MLOC13","MLOC15","MLOC17","MLOC20","MLOC22",
                "MLOC2","MLOC6","MLOC7","MLOC10","MLOC12","MLOC14","MLOC16","MLOC24")

names_BFI_FRN <- c( "BFI_1","BFI_2","BFI_3","BFI_4","BFI_5","BFI_6","BFI_7","BFI_8", "BFI_9" ,
                    "BFI_10","BFI_11","BFI_12", "BFI_13", "BFI_14","BFI_15" ,"BFI_16","BFI_17" ,"BFI_18", 
                    "BFI_19","BFI_20","BFI_21" ,"BFI_22","BFI_23" , "BFI_24","BFI_25", "BFI_26" , "BFI_27",
                    "BFI_28", "BFI_29","BFI_30","BFI_31" ,"BFI_32", "BFI_33" ,"BFI_34","BFI_35","BFI_36",
                    "BFI_37","BFI_38", "BFI_39","BFI_40","BFI_41" , "BFI_42" , "BFI_43", "BFI_44" ,"BFI_45",
                    "Extraversion","Agreabilite" , "Conscience", "EmotionsNegatives","Ouverture")

names_LOC_JPN <- c("LOC_1","LOC_2","LOC_3","LOC_4","LOC_5","LOC_6","LOC_7")

#function for deleting omission & same responses cases
cleaning_OmissionSames <- function(m){
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
cleaned_Alldata <- lapply(ALLdata,cleaning_OmissionSames)


#function for calculating FAD+ Scores
FAD_Plus_scorescalculate <- function(m){
  fadnames <- c("FD1","FD5","FD9","FD13","FD17",
                "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  mfad <- as.data.frame(m[,fadnames])
  FADScores <- function(v){
    FDNames <- c("FD1","FD5","FD9","FD13", "FD17")
    SDNames <- c("SD2","SD6","SD10","SD14","SD18","SD22","SD24")
    UPNames <- c("UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27")
    FWNames <- c("FW4","FW8","FW12","FW16","FW21","FW23","FW26")
    FD <- sum(v[FDNames])/length(FDNames)
    SD <- sum(v[SDNames])/length(SDNames)
    UP <- sum(v[UPNames])/length(UPNames)
    FW <- sum(v[FWNames])/length(FWNames)
    Scores <- cbind(FD,SD,UP,FW)
    return(Scores)
  }
  fad4scores <- t(apply(mfad,1,FADScores))
  colnames(fad4scores) <- c("FD","SD","UP","FW")
  allwithscores <- cbind(m,fad4scores)
  return(allwithscores)
}
#calculating for whole list data based on cleaned data
Alldata_cleaned_scores <- lapply(cleaned_Alldata, FAD_Plus_scorescalculate)

write.csv(Alldata_cleaned_scores[[1]],"CHN_1.1_Cleaned.csv",row.names=FALSE)
write.csv(Alldata_cleaned_scores[[2]],"CHN_1.2_Cleaned.csv",row.names=FALSE)
write.csv(Alldata_cleaned_scores[[3]],"CHN_1.3_Cleaned.csv",row.names=FALSE)
write.csv(Alldata_cleaned_scores[[4]],"ENG_Cleaned.csv",row.names=FALSE)
write.csv(Alldata_cleaned_scores[[5]],"FRN_Cleaned.csv",row.names=FALSE)
write.csv(Alldata_cleaned_scores[[6]],"JPN_1_Cleaned.csv",row.names=FALSE)
write.csv(Alldata_cleaned_scores[[7]],"JPN_2_Cleaned.csv",row.names=FALSE)



########################################
########################################
########### Basic descriptions #########
########################################
########################################

#dim(Alldata_cleaned_scores[[4]])

descriptions_CHN1.1 <- psych::describe(Alldata_cleaned_scores[[1]])
table(Alldata_cleaned_scores[[1]][,"gender"])
table(Alldata_cleaned_scores[[1]][,"edu"])

CHN_1.1_BFI <- Alldata_cleaned_scores[[1]][-c(which(is.na(Alldata_cleaned_scores[[1]][,"BFI_A1"]))),]
descriptions_CHN1.1_BFI <- psych::describe(CHN_1.1_BFI)
table(CHN_1.1_BFI[,"gender"])


descriptions_CHN1.2 <- psych::describe(Alldata_cleaned_scores[[2]])
table(Alldata_cleaned_scores[[2]][,"gender"])
table(Alldata_cleaned_scores[[2]][,"edu"])


descriptions_CHN1.3 <- psych::describe(Alldata_cleaned_scores[[3]])
table(Alldata_cleaned_scores[[3]][,"gender"])


descriptions_ENG <- psych::describe(Alldata_cleaned_scores[[4]])
table(Alldata_cleaned_scores[[4]][,"gender"])


class(Alldata_cleaned_scores[[5]])
descriptions_FRN <- psych::describe(Alldata_cleaned_scores[[5]])
table(Alldata_cleaned_scores[[5]][,"gender"])

FRN_BFI <- Alldata_cleaned_scores[[5]][-c(which(is.na(Alldata_cleaned_scores[[5]][,"BFI_1"]))),]
descriptions_FRN_BFI <- psych::describe(FRN_BFI)
table(FRN_BFI[,"gender"])


descriptions_JPN1 <- psych::describe(Alldata_cleaned_scores[[6]])
table(Alldata_cleaned_scores[[6]][,"gender"])
JPN1_LOC <- Alldata_cleaned_scores[[6]][-c(which(is.na(Alldata_cleaned_scores[[6]][,"LOC_1"]))),]
descriptions_JPN1_LOC <- psych::describe(JPN1_LOC)
table(JPN1_LOC[,"gender"])


descriptions_JPN2 <- psych::describe(Alldata_cleaned_scores[[7]])
table(Alldata_cleaned_scores[[7]][,"gender"])


table(Alldata_cleaned_scores[[4]][,1])

N_Alldata_cleaned <- unlist(lapply(Alldata_cleaned_scores, nrow))



fordescriptionsFADitems <- function(m){
  fadnames <- c("FD1","FD5","FD9","FD13","FD17",
                "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  mfad <- as.data.frame(m[,fadnames]) #where only takes FAD-Plus items
  des <- apply(mfad,2,psych::describe)
  return(des)
}

#basic descriptions of each item of FAD-Plus in different datasets
descriptionsFADitems <- lapply(Alldata_cleaned_scores, fordescriptionsFADitems) 

forfrecuenciesFADitems <- function(m){
  fadnames <- c("FD1","FD5","FD9","FD13","FD17",
                "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  mfad <- as.data.frame(m[,fadnames])
  fre <- apply(mfad, 2, table)
  return(fre)
}

#frecuencies of each item of FAD-Plus in different datasets
frecuenciesFADitems <- lapply(Alldata_cleaned_scores,forfrecuenciesFADitems)



########################################
########################################
############  RELIABILITY ##############
########################################
########################################

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
alphaandomega <- lapply(Alldata_cleaned_scores,alphasandomegas)
reliabilities <- matrix(unlist(alphaandomega),nrow = 7,byrow = T) 
colnames(reliabilities) <- c("alpha","alphaFD","alphaSD","alphaUP","alphaFW",
                             "omega","omegaFD","omegaSD","omegaUP","omegaFW")
rownames(reliabilities) <- c("CHN_1.1","CHN_1.2","CHN_1.3","ENG","FRN","JPN_1","JPN_2")
write.table(reliabilities,"alphasandomegas.txt")



########################################
########################################
############  CORRELATIONS #############
########################################
########################################

forcorrelationsFAD <- function(m){
  fadnameswith4 <- c("FD","SD","UP","FW",
                     "FD1","FD5","FD9","FD13","FD17",
                     "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                     "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                     "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  relations <- cor(m[,fadnameswith4])
  correlationsto01 <- function(v){
    v_abs <- abs(v)
    cuts <- cut(v_abs,breaks = c(0,0.5,1),labels=c(0,1))
    return(cuts)
  }
  c01 <- apply(relations,1,correlationsto01)
  return(cbind(relations,c01))
}

correlationsFAD <- lapply(Alldata_cleaned_scores,forcorrelationsFAD)

#save correlation matrix for each dataset
for (i in 1:length(correlationsFAD)) {
  textnames <- paste("correlations",i,".txt",sep = "")
  write.table(correlationsFAD[[i]],textnames)
}



########################################
########################################
################  CFA ##################
########################################
########################################

forcfaFAD_model4 <- function(m){
  model <-  'FD =~ FD1 + FD5 + FD9 + FD13 + FD17;
  SD =~ SD2 + SD6 + SD10 + SD14 + SD18 + SD22 + SD24;
  UP =~ UP3 + UP7 + UP11 + UP15 + UP19 + UP20 + UP25 + UP27;
  FW =~ FW4 + FW8 + FW12 + FW16 + FW21 + FW23 + FW26'
  fadnames <- c("FD1","FD5","FD9","FD13","FD17",
                "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
  mfad <- m[,fadnames]
  rescfa <- lavaan::cfa(model,mfad,estimator="ml")
  semPlot::semPaths(rescfa,"std",rotation = 2,layout = "tree2",nCharNodes = 0,
                    sizeLat = 15, sizeLat2 = 7, label.norm = "OOOOO", 
                    mar=c(2,6,2,4), curvePivot = TRUE,
                    edge.label.cex=1.2, residuals = F)
  res <- standardizedSolution(rescfa)
  return(res)
}

cfaFAD_model4 <- lapply(Alldata_cleaned_scores, forcfaFAD_model4)

for (i in 1:length(cfaFAD_model4)) {
  textnames <- paste("cfas",i,".txt",sep = "")
  write.table(cfaFAD_model4[[i]],textnames)
}

#testcpa <-  psych::fa.parallel(Alldata_cleaned_scores[[1:7]][,fadnames],fm="uls")

########################################
########################################
###### Measurement Invariance ##########
########### CFA MULTIGROUP #############
########################################
########################################


FAD_model4 <-  'FD =~ FD1 + FD5 + FD9 + FD13 + FD17
                SD =~ SD2 + SD6 + SD10 + SD14 + SD18 + SD22 + SD24
                UP =~ UP3 + UP7 + UP11 + UP15 + UP19 + UP20 + UP25 +UP27
                FW =~ FW4 + FW8 + FW12 + FW16 + FW21 + FW23 + FW26'


FAD_model4_test <-  'FD =~ FD1 + FD5 + c(v1,v1)*FD9 + c(v1,v1)*FD13 + c(v1,v1)*FD17
                     SD =~ SD2 + c(v1,v1)*SD6 + SD10 + c(v1,v1)*SD14 + c(v1,v1)*SD18 + c(v1,v1)*SD22 + c(v1,v1)*SD24
                     UP =~ UP3 + c(v1,v1)*UP7 + c(v1,v1)* UP11 + c(v1,v1)*UP15 + c(v1,v1)*UP19 + c(v1,v1)*UP20 + c(v1,v1)*UP25 + c(v1,v1)*UP27  
                     FW =~ FW4 + c(v1,v1)*FW8 + c(v1,v1)*FW12 + c(v1,v1)*FW16 + c(v1,v1)*FW21 + c(v1,v1)*FW23 + c(v1,v1)*FW26'
#this model is a test model, we have tried to release different items one by one...

forcfaMI <- function(m1,m2,nombre,modelo){
  res <- list()
  fadnames <- nombre
  model <-  modelo
  m1fad <- m1[,fadnames]
  m2fad <- m2[,fadnames]
  g <- c(rep("0",length(m1[,1])),rep("1",length(m2[,1])))
  datas <- cbind(rbind(m1fad,m2fad),g)
  datas$g <- factor(datas$g,levels = c("0","1"))
  fit1 <- cfa(model,datas,group = "g") 
  fit2 <- cfa(model,datas,group = "g", group.equal="loadings")
  fit3 <- cfa(model,datas,group = "g", group.equal = c("intercepts", "loadings"))
  fit4 <- cfa(model,datas,group = "g", group.equal = c("intercepts", "loadings","residuals"))
  res[[1]] <- lavTestLRT(fit1, fit2, fit3, fit4)
  res[[2]] <- rbind(summary(fit1,fit.measures=T)$FIT, summary(fit2,fit.measures=T)$FIT, summary(fit3,fit.measures=T)$FIT, summary(fit4,fit.measures=T)$FIT)
  #res1 <- modindices(fit1, sort = T, maximum.number = 10)
  return(res)
} #function made for testing MI in our case that could use different cfa model, which we achieved partial MI

#test MI with testENG
MI_model4 <- forcfaMI(Alldata_cleaned_scores[[7]],testENG,fadnames,FAD_model4)
MI_modeltest <- forcfaMI(Alldata_cleaned_scores[[7]],testENG,fadnames,FAD_model4_test)

#testing measurement invariance bewteen CHN1.1-ENG, FRN-ENG, JPN1-English, JPN2-English
CHN1.1_ENG_model4 <- forcfaMI(Alldata_cleaned_scores[[1]],Alldata_cleaned_scores[[4]],fadnames,FAD_model4)
CHN1.1_ENG_modeltest <- forcfaMI(Alldata_cleaned_scores[[1]],Alldata_cleaned_scores[[4]],fadnames,FAD_model4_test)

write.table(CHN1.1_ENG_model4,"MI_CHN1.1_ENG_model4.txt")
write.table(CHN1.1_ENG_modeltest,"MI_CHN1.1_ENG_modeltest.txt")


FRN_ENG_model4 <- forcfaMI(Alldata_cleaned_scores[[5]],Alldata_cleaned_scores[[4]],fadnames,FAD_model4)
FRN_ENG_modeltest <- forcfaMI(Alldata_cleaned_scores[[5]],Alldata_cleaned_scores[[4]],fadnames,FAD_model4_test)

write.table(FRN_ENG_model4,"MI_FRN_ENG_model4.txt")
write.table(FRN_ENG_modeltest,"MI_FRN_ENG_modeltest.txt")


JPN1_ENG_model4 <- forcfaMI(Alldata_cleaned_scores[[6]],Alldata_cleaned_scores[[4]],fadnames,FAD_model4)
JPN1_ENG_modeltest <- forcfaMI(Alldata_cleaned_scores[[6]],Alldata_cleaned_scores[[4]],fadnames,FAD_model4_test)

write.table(JPN1_ENG_model4,"MI_JPN1_ENG_model4.txt")
write.table(JPN1_ENG_modeltest,"MI_JPN1_ENG_modeltest.txt")


JPN2_ENG_model4 <- forcfaMI(Alldata_cleaned_scores[[7]],Alldata_cleaned_scores[[4]],fadnames,FAD_model4)
JPN2_ENG_modeltest <- forcfaMI(Alldata_cleaned_scores[[7]],Alldata_cleaned_scores[[4]],fadnames,FAD_model4_test)

write.table(JPN2_ENG_model4,"MI_JPN2_ENG_model4.txt")
write.table(JPN2_ENG_modeltest,"MI_JPN2_ENG_modeltest.txt")

#####partial with semTools#####
#MItest <- semTools::measurementInvariance(model = model, data = datas, group = "g")
#testpartialInvariance <- semTools::partialInvariance(MItest, "metric", free = NULL, fix = NULL, refgroup = 1, poolvar = TRUE, p.adjust = "none", fbound = 2, return.fit = FALSE) 



########################################
########################################
############## CORRELATION #############
############## with BIG 5 ##############
########################################
########################################

########################
########CHN DATA########
########################
#select cases with BFI data
BFI_NAlocated <- which(is.na(Alldata_cleaned_scores[[1]][,"BFI_A1"]))
forCHN_BFI_FAD <- Alldata_cleaned_scores[[1]][-BFI_NAlocated,]
CHN1.1_FAD_BFI <- forCHN_BFI_FAD[,c("FD","SD","UP","FW",fadnames)]

#function for BIG5 5 dimensions' scores
CHN_BFIS <- function(datos,nombre,key){
  databfi <- datos[,nombre]
  data_corrected <- matrix(t(apply(databfi,1,function(x){x*key})),ncol = length(key))
  datafinal <- na.exclude(data_corrected)
  bfiscores <- apply(datafinal, 1, function(x){sum(x)/length(key)})
  return(bfiscores)
}  

# This BFI scores part base on Qinglan's code
CHN_BFI_ANames <- c("BFI_A1","BFI_A2","BFI_A3","BFI_A4","BFI_A5","BFI_A6","BFI_A7","BFI_A8","BFI_A9")
CHN_BFI_AKeys <- c(1,1,1,1,1,-1,-1,-1,-1) 
CHN_BFIA <- CHN_BFIS(Alldata_cleaned_scores[[1]],CHN_BFI_ANames,CHN_BFI_AKeys)
#length(CHN_BFIA)

CHN_BFI_CNames <- c("BFI_C1","BFI_C2","BFI_C3","BFI_C4","BFI_C5","BFI_C6","BFI_C7","BFI_C8","BFI_C9")
CHN_BFI_CKeys <- c(1,1,1,1,1,-1,-1,-1,-1)
CHN_BFIC <- CHN_BFIS(Alldata_cleaned_scores[[1]],CHN_BFI_CNames,CHN_BFI_CKeys)
#length(CHN_BFIC)

CHN_BFI_NNames <- c("BFI_N1","BFI_N2","BFI_N3","BFI_N4","BFI_N5","BFI_N6","BFI_N7","BFI_N8")
CHN_BFI_NKeys <- c(1,1,1,1,1,-1,-1,-1)
CHN_BFIN <- CHN_BFIS(Alldata_cleaned_scores[[1]],CHN_BFI_NNames,CHN_BFI_NKeys)
#length(CHN_BFIN)

CHN_BFI_ONames <- c("BFI_O1","BFI_O2","BFI_O3","BFI_O4","BFI_O5","BFI_O6","BFI_O7","BFI_O8","BFI_O9","BFI_O10")
CHN_BFI_OKeys <- c(1,1,1,1,1,1,1,1,1,1)
CHN_BFIO <- CHN_BFIS(Alldata_cleaned_scores[[1]],CHN_BFI_ONames,CHN_BFI_OKeys)
#length(CHN_BFIO)


CHN_BFI_ENames <- c("BFI_E1","BFI_E2","BFI_E3","BFI_E4","BFI_E5","BFI_E6","BFI_E7","BFI_E8")
CHN_BFI_EKeys <- c(1,1,1,1,1,1,1,1)
CHN_BFIE <- CHN_BFIS(Alldata_cleaned_scores[[1]],CHN_BFI_ENames,CHN_BFI_EKeys)
#length(CHN_BFIE)

CHN_BFIS <- cbind(CHN_BFIA,CHN_BFIC,CHN_BFIN,CHN_BFIO,CHN_BFIE)

CHN_BFI_FAD_Final <- cbind(CHN_BFIS,CHN1.1_FAD_BFI)
corCHN_BFI_FAD <- cor(CHN_BFI_FAD_Final[,1:9])
write.table(corCHN_BFI_FAD,"CHN_BFI_FAD.txt") #save correlation martix with BFI 5 dimensions


corCHN_BFI_FAD[which(abs(corCHN_BFI_FAD)<1 & abs(corCHN_BFI_FAD)>0.6)]  #NA

corCHN_BFI_FAD27 <- cor(CHN_BFI_FAD_Final[,-c(6,7,8,9)]) #correlations between BFI 5 dimensions with FAD+ 27 items, - 6,7,8,9 delete 4 dimensions of FAD, that already caculated above
corCHN_BFI_FAD27[which(abs(corCHN_BFI_FAD27)<1 & abs(corCHN_BFI_FAD27)>0.6)] #only FAD1-9


########################
########FRN DATA########
########################
FRN_FAD_BFI <- FRN_BFI[,c("FD","SD","UP","FW",fadnames)]
cor_FRN_BFI <- cor(FRN_BFI[,c("Extraversion","Agreabilite" , "Conscience", "EmotionsNegatives","Ouverture")],FRN_FAD_BFI)

write.table(cor_FRN_BFI,"FRN_BFI_FAD.txt")



########################################
########################################
############## CORRELATION #############
############## with MLOC ###############
########################################
########################################

########################
########CHN DATA########
########################

#select cases with MLOC data
MLOC_NAlocated <- which(is.na(Alldata_cleaned_scores[[1]][,"MLOC1"]))
forCHN_MLOC_FAD <- Alldata_cleaned_scores[[1]][-MLOC_NAlocated,]
CHN_FAD_MLOC <- forCHN_MLOC_FAD[,c("FD","SD","UP","FW",fadnames)]

#calculate MLOC scores
CHN_MLOC_INames <- c("MLOC1","MLOC4","MLOC5","MLOC9","MLOC18","MLOC19","MLOC21","MLOC23")
CHN_MLOC_I <- apply(forCHN_MLOC_FAD[,CHN_MLOC_INames]+3,1,sum)

CHN_MLOC_PNames <- c("MLOC3","MLOC8","MLOC11","MLOC13","MLOC15","MLOC17","MLOC20","MLOC22")
CHN_MLOC_P <- apply(forCHN_MLOC_FAD[,CHN_MLOC_PNames]+3,1,sum)


CHN_MLOC_CNames <- c("MLOC2","MLOC6","MLOC7","MLOC10","MLOC12","MLOC14","MLOC16","MLOC24")
CHN_MLOC_C <- apply(forCHN_MLOC_FAD[,CHN_MLOC_CNames]+3,1,sum) 

CHN_MLOCS <- cbind(CHN_MLOC_I,CHN_MLOC_P,CHN_MLOC_C)

CHN_MLOC_FAD_Final <- cbind(CHN_MLOCS,CHN_FAD_MLOC)
corCHN_MLOC_FAD <- cor(CHN_MLOC_FAD_Final[,1:7])
write.table(corCHN_MLOC_FAD,"CHN_MLOC_FAD.txt") #save correlation matrix with 3 factor of MLOC


corCHN_MLOC_FAD[which(abs(corCHN_MLOC_FAD)<1 & abs(corCHN_MLOC_FAD)>0.6)] #low correlations bewteen MLOC & FAD; but, noted MLOC_P*MLOC_C high correlations

corCHN_MLOC_FAD27 <- cor(CHN_MLOC_FAD_Final[,-c(4,5,6,7)]) #correlations between MLOC 3 dimensions with FAD+ 27 items, removing 4 dimensions of FAD-Plus
corCHN_MLOC_FAD27[which(abs(corCHN_MLOC_FAD27)<1 & abs(corCHN_MLOC_FAD27)>0.6)] #MLOC_P*MLOC_C; FD1-9

########################
########JPN DATA########
########################

#check the structure of JPN_LOC 
psych::fa(JPN1_LOC[,names_LOC_JPN], nfactors = 2, rotate = "oblimin", fm = "ml")$loading

#calculate the scores of LOC assuming that 1-4 as factor 1, 5-7 factor 2
only_JPN1_LOC <- JPN1_LOC[,names_LOC_JPN]
JPN1_LOC_1_Scores <- apply(only_JPN1_LOC[,c("LOC_1","LOC_2","LOC_3","LOC_4")],1,sum)
JPN1_LOC_2_Scores <- apply(only_JPN1_LOC[,c("LOC_5","LOC_6","LOC_7")],1,sum)

cor_LOC_JPN1 <- cor(cbind(JPN1_LOC_1_Scores,JPN1_LOC_2_Scores),JPN1_LOC[,c("FD","SD","UP","FW")])



only_JPN2_LOC <- Alldata_cleaned_scores[[7]][,names_LOC_JPN]
JPN2_LOC_1_Scores <- apply(only_JPN2_LOC[,c("LOC_1","LOC_2","LOC_3","LOC_4")],1,sum)
JPN2_LOC_2_Scores <- apply(only_JPN2_LOC[,c("LOC_5","LOC_6","LOC_7")],1,sum)

cor_LOC_JPN2 <- cor(cbind(JPN2_LOC_1_Scores,JPN2_LOC_2_Scores),Alldata_cleaned_scores[[7]][,c("FD","SD","UP","FW")])
