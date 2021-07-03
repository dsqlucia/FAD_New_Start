rm(list = ls())
#install.packages("CTT")

library("CTT")
library("dplyr")
library("psych")
library("lavaan")
library("semPlot")
library("semTools")


datafadall <- read.csv("cleanfadall.csv")
datafadchina <- datafadall[which(datafadall$g==0),-1] 
datafadforeign <- datafadall[which(datafadall$g==1),-1] 


desdatachina <- datafadchina[,1:4] #descriptions of chinese datasets
apply(desdatachina, 2, psych::describe)
apply(desdatachina, 2, table)

desdataforeign <- datafadforeign[,1:4] #descriptions of foriegn datasets
apply(desdataforeign, 2, psych::describe)
apply(desdataforeign, 2, table)

########################################
########################################
######  CTT IN CHINESE DATASET  ########
########################################
########################################

fadnames <- c("FD1","FD5","FD9","FD13","FD17",
              "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
              "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
              "FW4","FW8","FW12","FW16","FW21","FW23","FW26")


fadchinasolo <- datafadchina[,fadnames]
apply(fadchinasolo,2,describe)
apply(fadchinasolo,2,table)


FDnames <- c("FD1","FD5","FD9","FD13","FD17")
SDnames <- c("SD2","SD6","SD10","SD14","SD18","SD22","SD24")
UPnames <- c("UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27")
FWnames <- c("FW4","FW8","FW12","FW16","FW21","FW23","FW26")

fadFDC <- datafadchina[,FDnames]
fadSDC <- datafadchina[,SDnames]
fadUPC <- datafadchina[,UPnames]
fadFWC <- datafadchina[,FWnames]
fadchinalist <- list(fadFDC,fadSDC,fadUPC,fadFWC)

fadFDF <- datafadforeign[,FDnames]
fadSDF <- datafadforeign[,SDnames]
fadUPF <- datafadforeign[,UPnames]
fadFWF <- datafadforeign[,FWnames]
fadforeignlist <- list(fadFDF,fadSDF,fadUPF,fadFWF)


resctt <- CTT::reliability(fadchinasolo)
str(resctt) #alpha china ==0.758
psych::alpha(fadchinasolo) 
#psych::alpha(fadchinasolo,check.keys=TRUE) #FW12 may be reversed FW12:只要真心想做，人们可以克服一切困难

psych::omega(fadchinasolo)

reschinactt <- lapply(fadchinalist,CTT::reliability)
str(reschinactt)
resalphachina <- lapply(fadchinalist,psych::alpha)
resomegachina <- lapply(fadchinalist,psych::omega)

iteminfoctt <- as.data.frame(cbind(resctt$itemMean,
                                   apply(fadchinasolo, MARGIN = 2, FUN = sd),
                                   resctt$pBis,resctt$alphaIfDeleted)) #pBis=item-total correlation
colnames(iteminfoctt) <- c("mean","sd","rpbc","alpha If Deleted")
round(iteminfoctt,3)
iteminfoctt[order(iteminfoctt$`alpha If Deleted`,decreasing = T),] #SD6 心理学家和精神病学家最终会弄清楚人类所有的行为  Psychologists and psychiatrists will eventually figure out all human behavior

cor4 <- cor(datafadchina[,c("FD","SD","UP","FW")])
coritems <- cor(datafadchina[,fadnames])

which(abs(coritems)<1 & abs(coritems)>0.6)  #FD1-FD9 == 0.7356 
#FD1 我相信未来是命运已经安排好了的  I believe that the future has already been determined by fate
#FD9 命运早已为每个人做好安排  Fate already has a plan for everyone

corFD <- cor(datafadchina$FD,datafadchina[,fadnames]) #correlation between each dimension and each items
corSD <- cor(datafadchina$SD,datafadchina[,fadnames])
corUP <- cor(datafadchina$UP,datafadchina[,fadnames])
corFW <- cor(datafadchina$FW,datafadchina[,fadnames])

cor4withitems <- t(rbind(corFD,corSD,corUP,corFW))
colnames(cor4withitems) <- c("FD","SD","UP","FW") #correlation between 4 dimensions
round(cor4withitems,3)
# cor4withitems[order(cor4withitems[,4],decreasing = T),]  #1:4

########################################
########################################
######  CTT IN FOREIGN DATASET  ########
########################################
########################################

fadforeignsolo <- datafadforeign[,fadnames]
apply(fadforeignsolo,2,describe)
apply(fadforeignsolo,2,table)

rescttf <- CTT::reliability(fadforeignsolo)
str(rescttf)#alpha==0.829

psych::alpha(fadforeignsolo) 
psych::omega(fadforeignsolo)

resforeignctt <- lapply(fadforeignlist,CTT::reliability)
str(resforeignctt)
resalphaforeign <- lapply(fadforeignlist,psych::alpha)
resomegaforeign <- lapply(fadforeignlist,psych::omega)


iteminfocttf <- as.data.frame(cbind(rescttf$itemMean,
                                   apply(fadforeignsolo, MARGIN = 2, FUN = sd),
                                   rescttf$pBis,rescttf$alphaIfDeleted)) #pBis=item total correlation
colnames(iteminfocttf) <- c("mean","sd","rpbc","alpha If Deleted")
round(iteminfocttf,3)
iteminfocttf[order(iteminfocttf$`alpha If Deleted`,decreasing = T),] #FW12 People can overcome any obstacles if they truly want to

cor4f <- cor(datafadforeign[,c("FD","SD","UP","FW")])
coritemsf <- cor(datafadforeign[,fadnames])

which(abs(coritemsf)<1 & abs(coritemsf)>0.6)  #FD1-FD9 == 0.6345 
#FD1 我相信未来是命运已经安排好了的  I believe that the future has already been determined by fate
#FD9 命运早已为每个人做好安排  Fate already has a plan for everyone

corFDf <- cor(datafadforeign$FD,datafadforeign[,fadnames])
corSDf <- cor(datafadforeign$SD,datafadforeign[,fadnames])
corUPf <- cor(datafadforeign$UP,datafadforeign[,fadnames])
corFWf <- cor(datafadforeign$FW,datafadforeign[,fadnames])

cor4withitemsf <- t(rbind(corFDf,corSDf,corUPf,corFWf))
colnames(cor4withitemsf) <- c("FD","SD","UP","FW")
round(cor4withitemsf,3)


###########################
###########################
##########  CFA  ##########
###########################
###########################

model <- 'FD =~ FD1 + FD5 + FD9 + FD13 + FD17;
SD =~ SD2 + SD6 + SD10 + SD14 + SD18 + SD22 + SD24;
UP =~ UP3 + UP7 + UP11 + UP15 + UP19 + UP20 + UP25 + UP27;
FW =~ FW4 + FW8 + FW12 + FW16 + FW21 + FW23 + FW26'

rescfafitCHINA <- lavaan::cfa(model, fadchinasolo,estimator="ml") #"ULS","WLSM"
summary(rescfafitCHINA, standardized = T, fit.measures = TRUE)
semPlot::semPaths(rescfafitCHINA,"std",rotation = 2, layout = "tree2", nCharNodes = 0,
                  sizeLat = 15, sizeLat2 = 7, label.norm = "OOOOO", 
                  mar=c(2,6,2,4), curvePivot = TRUE,
                  edge.label.cex=1.2, residuals = F)
semTools::moreFitIndices(rescfafitCHINA)

rescfafitFOREIGN <- lavaan::cfa(model, fadforeignsolo,estimator="ml") #"ULS","WLSM"
summary(rescfafitFOREIGN, standardized = T, fit.measures = TRUE)
semPlot::semPaths(rescfafitFOREIGN,"std",rotation = 2, layout = "tree2", nCharNodes = 0,
                  sizeLat = 15, sizeLat2 = 7, label.norm = "OOOOO", 
                  mar=c(2,6,2,4), curvePivot = TRUE,
                  edge.label.cex=1.2, residuals = F)
semTools::moreFitIndices(rescfafitFOREIGN)

#the possible model based on FAD_1
# model1 <- 'FD =~ FD1 + FD5 + FD9 + FD13 + FD17;
# SD1 =~ SD6 + SD22;
# GEN =~ SD2 + SD10 + SD18;
# ENV =~ SD14 +SD24;
# UP =~ UP3 + UP7 + UP11 + UP15 + UP19 + UP20 + UP25 + UP27;
# FW =~ FW4 + FW8 + FW12 + FW16 + FW21 + FW23 + FW26'


#################################################
############# Invariance ########################
############# CFA MULTIGROUP ####################
############# BETWEEN CHINESE & FORIEGN #########
#################################################

fadsolo <- datafadall[,fadnames]
head(fadsolo)
g <- datafadall$g
dataforinvariance <- cbind(g,fadsolo)
head(dataforinvariance)
table(g)


dataforinvariance$g <- factor(dataforinvariance[,1],levels=c("0","1"))
table(dataforinvariance$g)

config <- cfa(model, dataforinvariance, group="g")  
weak <- cfa(model, dataforinvariance, group="g", group.equal="loadings") #factor weight
strong<- cfa(model, dataforinvariance, group="g", group.equal= c("loadings", "intercepts"))  #set the same weight and intercepts
strict<- cfa(model, dataforinvariance, group="g", 
             group.equal= c("loadings", "intercepts", "residuals"))   #residues
summary(config)
summary(weak)
summary(strong)
summary(strict)
anova(config, weak, strong, strict)

measurementInvariance(model=model, data=dataforinvariance, group="g",estimator="WLSM") #"ULS"


###########################
###########################
##########  EFA  ##########
###########################
###########################

resefaCHINA <- psych::fa(fadchinasolo, nfactors = 4, rotate = "oblimin", fm = "ml")  #with 4 dimensions
resefaCHINA$loading
fa.diagram(resefaCHINA)

resefaFOREIGN <- psych::fa(fadforeignsolo, nfactors = 4, rotate = "oblimin", fm = "ml") 
resefaFOREIGN$loading
fa.diagram(resefaFOREIGN)

###########################
###########################
##########  CPA  ##########
###########################
###########################

rescpa <- psych::fa.parallel(fadsolo,fm = "uls")

rescpaCHINA <- psych::fa.parallel(fadchinasolo,fm = "uls")
resPA2CHINA<- as.data.frame(cbind(rescpaCHINA$pc.values,rescpaCHINA$pc.sim))
colnames(resPA2CHINA) <- c("PC values","PC simulated")
round(resPA2CHINA,3)

# psych::fa(fadchinasolo, nfactors = 4, rotate = "oblimin", fm = "ml")$loading

rescpaFOREIGN <- psych::fa.parallel(fadforeignsolo,fm = "uls")
resPA2FOREIGN<- as.data.frame(cbind(rescpaFOREIGN$pc.values,rescpaFOREIGN$pc.sim))
colnames(resPA2FOREIGN) <- c("PC values","PC simulated")
round(resPA2FOREIGN,3)


#######################################
#######################################
############ CORRELATION ##############
############ with BIG 5 ###############
#######################################
#######################################

BFI_NAlocated <- which(is.na(datafadall[,"BFI_A1"]))
forBFI_FAD <- datafadall[-BFI_NAlocated,]
FAD_BFI <- forBFI_FAD[,c("FD","SD","UP","FW",fadnames)]

BFIS <- function(datos,nombre,key){
  databfi <- datos[,nombre]
  data_corrected <- matrix(t(apply(databfi,1,function(x){x*key})),ncol = length(key))
  datafinal <- na.exclude(data_corrected)
  bfiscores <- apply(datafinal, 1, function(x){sum(x)/length(key)})
  return(bfiscores)
} #function for BIG5 5 dimensions' Scores 

BFI_ANames <- c("BFI_A1","BFI_A2","BFI_A3","BFI_A4","BFI_A5","BFI_A6","BFI_A7","BFI_A8","BFI_A9")
BFI_AKeys <- c(1,1,1,1,1,-1,-1,-1,-1) 
BFIA <- BFIS(datafadall,BFI_ANames,BFI_AKeys)
length(BFIA)

BFI_CNames <- c("BFI_C1","BFI_C2","BFI_C3","BFI_C4","BFI_C5","BFI_C6","BFI_C7","BFI_C8","BFI_C9")
BFI_CKeys <- c(1,1,1,1,1,-1,-1,-1,-1)
BFIC <- BFIS(datafadall,BFI_CNames,BFI_CKeys)
length(BFIC)


BFI_NNames <- c("BFI_N1","BFI_N2","BFI_N3","BFI_N4","BFI_N5","BFI_N6","BFI_N7","BFI_N8")
BFI_NKeys <- c(1,1,1,1,1,-1,-1,-1)
BFIN <- BFIS(datafadall,BFI_NNames,BFI_NKeys)
length(BFIN)

BFI_ONames <- c("BFI_O1","BFI_O2","BFI_O3","BFI_O4","BFI_O5","BFI_O6","BFI_O7","BFI_O8","BFI_O9","BFI_O10")
BFI_OKeys <- c(1,1,1,1,1,1,1,1,1,1)
BFIO <- BFIS(datafadall,BFI_ONames,BFI_OKeys)
length(BFIO)


BFI_ENames <- c("BFI_E1","BFI_E2","BFI_E3","BFI_E4","BFI_E5","BFI_E6","BFI_E7","BFI_E8")
BFI_EKeys <- c(1,1,1,1,1,1,1,1)
BFIE <- BFIS(datafadall,BFI_ENames,BFI_EKeys)
length(BFIE)

BFIS <- cbind(BFIA,BFIC,BFIN,BFIO,BFIE)

BFI_FAD_Final <- cbind(BFIS,FAD_BFI)
corBFI_FAD <- cor(BFI_FAD_Final[,1:9])
corBFI_FAD[which(abs(corBFI_FAD)<1 & abs(corBFI_FAD)>0.6)]

corBFI_FAD27 <- cor(BFI_FAD_Final[,-c(6,7,8,9)])
corBFI_FAD27[which(abs(corBFI_FAD27)<1 & abs(corBFI_FAD27)>0.6)] #FAD1-9

#######################################
#######################################
############ CORRELATION ##############
############ with MLOC ################
#######################################
#######################################

MLOC_NAlocated <- which(is.na(datafadall[,"MLOC1"]))
forMLOC_FAD <- datafadall[-MLOC_NAlocated,]
FAD_MLOC <- forMLOC_FAD[,c("FD","SD","UP","FW",fadnames)]

MLOC_INames <- c("MLOC1","MLOC4","MLOC5","MLOC9","MLOC18","MLOC19","MLOC21","MLOC23")
MLOC_I <- apply(forMLOC_FAD[,MLOC_INames]+3,1,sum)

MLOC_PNames <- c("MLOC3","MLOC8","MLOC11","MLOC13","MLOC15","MLOC17","MLOC20","MLOC22")
MLOC_P <- apply(forMLOC_FAD[,MLOC_PNames]+3,1,sum)


MLOC_CNames <- c("MLOC2","MLOC6","MLOC7","MLOC10","MLOC12","MLOC14","MLOC16","MLOC24")
MLOC_C <- apply(forMLOC_FAD[,MLOC_CNames]+3,1,sum) 

MLOCS <- cbind(MLOC_I,MLOC_P,MLOC_C)

MLOC_FAD_Final <- cbind(MLOCS,FAD_MLOC)
corMLOC_FAD <- cor(MLOC_FAD_Final[,1:7])
corMLOC_FAD[which(abs(corMLOC_FAD)<1 & abs(corMLOC_FAD)>0.6)] #low correlations bewteen MLOC & FAD; buuuuut, noted MLOC_P*MLOC_C

corMLOC_FAD27 <- cor(MLOC_FAD_Final[,-c(4,5,6,7)])
corMLOC_FAD27[which(abs(corMLOC_FAD27)<1 & abs(corMLOC_FAD27)>0.6)]

###########################
###########################
##########  IRT  ##########
###########################
###########################

detach("package:psych")
library(ltm)
library(mirt)
library(lordif)


fitCHINA <- grm(fadchinasolo)
fitCHINA
plot(fitCHINA, items = 3) #example
plot(fitCHINA, type = c("IIC"), items=1:5)
plot(fitCHINA, type = c("IIC"), items=3)
plot(fitCHINA, type = c("IIC"), items=0,lwd = 2)

resirtCHINA <- factor.scores(fitCHINA,fadchinasolo)
cor(fadchinasolo$FD5,resirtCHINA$score.dat$FD5)

difC <- resirtCHINA$score.dat[,1:27]-fadchinasolo
sum(difC) #differences between the estimated scores and the real ones


fitFOREIGN <- grm(fadforeignsolo)
fitFOREIGN

resirtFOREIGN <- factor.scores(fitFOREIGN,fadforeignsolo)
cor(fadforeignsolo$FD5,resirtFOREIGN$score.dat$FD5)

difF <- resirtFOREIGN$score.dat[,1:27]-fadforeignsolo
sum(difF)


ggumfad <- function(m) {
  n <- length(m[1,])
  res <- mirt(m,1,itemtype = "ggum")
  ressum <- summary(res)
  coefres <- coef(res)
  listcoefres <- unlist(coefres)
  l <- length(listcoefres)
  listcoefres <- listcoefres[-c(l-1,l)]
  coefmatrix <- matrix(listcoefres,nrow = n,byrow = T)
  resfinal <- cbind(ressum[[1]],ressum[[2]],coefmatrix)
  return(resfinal)
}

resggumCHINA <- lapply(fadchinalist,ggumfad) # estimated with generalized graded unfolding model in each dimension, limited by mirt function only use for items unidimensional
resggumFOREIGN <- lapply(fadforeignlist,ggumfad)


################################################
############### Invariance #####################
############### LORDIF IN IRT ##################
######### Using g as the latent variable########
############### WASNT RUNNING WELL##############
################################################

reslordif <- lordif(fadsolo[,-23], g, criterion = "Chisqr", 
                    pseudo.R2 = c("Nagelkerke"),
                    alpha = 0.05, minCell = 1)
print(reslordif) #choosing Lordif as one of the most appropriate and economic way
summary(reslordif)

pchi <-as.data.frame(round(cbind(reslordif$stats$chi12,
                                 reslordif$stats$chi13,
                                 reslordif$stats$chi23),3))
names(pchi)<-c("pchi12","pchi13","pchi23");pchi


R2<-as.data.frame(round(cbind(reslordif$stats$pseudo12.Nagelkerke,
                              reslordif$stats$pseudo13.Nagelkerke,
                              reslordif$stats$pseudo23.Nagelkerke),6))
names(R2)<-c("R2_12","R2_13","R2_23");R2

#plot(reslordif)