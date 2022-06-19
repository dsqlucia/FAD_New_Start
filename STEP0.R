#################################################################
#################################################################
##### STEP 0 ####################################################
##### Sort out data & merge different datasets ##################
#################################################################
#################################################################



rm(list = ls())
setwd("~/Desktop/FAD_New_Start/2_Registered_Report/2_4_Analayses/2_4_1_Standardized_Data")

library("car")
library("dplyr")
library("psych")
Sys.setlocale(category="LC_ALL",locale="en_US.UTF-8") #run this for mac OS setting the local language solving the problems in reading Chinese dataset with CHN_1.2

fadnames <- c("FD1","FD5","FD9","FD13","FD17",
              "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
              "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
              "FW4","FW8","FW12","FW16","FW21","FW23","FW26")


################################
########## CHN_1.1 #############
################################
#datasets from datapapaer Liu et al (2020), 4 sub-dataset, the 4th dataset with BFI & MLOC data
#and one dataset (CHN_1.1.5) using the same translation version CHN_1.1 from Kong Yuwei (Jiang Xi)

CHN_1.1.1 <- read.csv("CHN_1.1_dataset1_clean.csv")
CHN_1.1.2 <- read.csv("CHN_1.1_dataset2_clean.csv")
CHN_1.1.3 <- read.csv("CHN_1.1_dataset3_clean.csv")
CHN_1.1.4 <- read.csv("CHN_1.1_dataset4_clean.csv")
CHN_1.1.5 <- read.csv("CHN_1.1_Kong.csv") #this dataset already only has the variable that we want

names_CHN_1.1 <- c("age","gender","edu",
                  "FD1","FD5","FD9","FD13","FD17",
                  "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                  "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                  "FW4","FW8","FW12","FW16","FW21","FW23","FW26")

CHN_1.1.1 <- CHN_1.1.1[,names_CHN_1.1]
CHN_1.1.2 <- CHN_1.1.2[,names_CHN_1.1]
CHN_1.1.3 <- CHN_1.1.3[,names_CHN_1.1]

names_CHN_1.1.4 <- c("age","gender","edu",
                    "FD1","FD5","FD9","FD13","FD17",
                    "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                    "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                    "FW4","FW8","FW12","FW16","FW21","FW23","FW26",
                    "BFI_A1","BFI_A2","BFI_A3","BFI_A4","BFI_A5","BFI_A6","BFI_A7","BFI_A8","BFI_A9",
                    "BFI_C1","BFI_C2","BFI_C3","BFI_C4","BFI_C5","BFI_C6","BFI_C7","BFI_C8","BFI_C9",
                    "BFI_N1","BFI_N2","BFI_N3","BFI_N4","BFI_N5","BFI_N6","BFI_N7","BFI_N8",
                    "BFI_O1","BFI_O2","BFI_O3","BFI_O4","BFI_O5","BFI_O6","BFI_O7","BFI_O8","BFI_O9","BFI_O10",
                    "BFI_E1","BFI_E2","BFI_E3","BFI_E4","BFI_E5","BFI_E6","BFI_E7","BFI_E8",
                    "MLOC1","MLOC4","MLOC5","MLOC9","MLOC18","MLOC19","MLOC21","MLOC23",
                    "MLOC3","MLOC8","MLOC11","MLOC13","MLOC15","MLOC17","MLOC20","MLOC22",
                    "MLOC2","MLOC6","MLOC7","MLOC10","MLOC12","MLOC14","MLOC16","MLOC24") #which the dataset has the BFI and MLOC data

CHN_1.1.4 <- CHN_1.1.4[,names_CHN_1.1.4]

CHN_1.1 <- dplyr::bind_rows(CHN_1.1.1,CHN_1.1.2,CHN_1.1.3,CHN_1.1.4,CHN_1.1.5)

NO <- c(rep("CHN1.1.1",times=length(CHN_1.1.1[,1])), 
        rep("CHN1.1.2",times=length(CHN_1.1.2[,1])),
        rep("CHN1.1.3",times=length(CHN_1.1.3[,1])),
        rep("CHN1.1.4",times=length(CHN_1.1.4[,1])),
        rep("CHN1.1.5",times=length(CHN_1.1.5[,1])))

#NO this is a new mark variable that distinguishing the original different sources: 
#1.1.1-Liu dataset1; 
#1.1.2-Liu dataset2; 
#1.1.3-Liu dataset3; 
#1.1.4-Liu dataset4; 
#1.1.5-Kong dataset

CHN_1.1_final <- cbind(NO,CHN_1.1)
write.csv(CHN_1.1_final,"CHN_1.1.csv",row.names=FALSE,fileEncoding ="UTF-8")




################################
########## CHN_1.2 #############
################################
#dataset from Gese DNA, with BIG5 data but we may not use
#translation version CHN_1.2

CHN_1.2_Gese <- read.csv("CHN_1.2_Gese.csv",encoding="UTF-8",header = T) #where resave FAD_1 as csv with UTF-8
#head(CHN_1.2[,1:10])

names_CHN_1.2 <- c("id", "age","gender_gene","gender_self_report","education_level",
                  "FD1","FD5","FD9","FD13","FD17",
                  "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                  "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27", 
                  "FW4","FW8","FW12","FW16","FW21","FW23","FW26")

CHN_1.2_Gese <- CHN_1.2_Gese[,names_CHN_1.2]


#table(CHN_1.2_Gese$education_level)
educationlvl <- as.vector(CHN_1.2_Gese$education_level)
edu <- car::recode(educationlvl,"'???????????????'=1;  
                   '??????'=2;
                   '???????????????'=3;
                   '??????'=3;
                   '??????'=4;
                   '??????'=5;
                   '??????'=6;
                   '??????'=7;else=0") #recode education's level with CHN_1.1's standard 
#table(edu) 

table(CHN_1.2_Gese$gender_gene)
table(CHN_1.2_Gese$gender_self_report)
genderCHN1.2 <- function(a){ #function for gender, take the genetic gender in our case
  gen <- a[3:4]
  genders <- car::recode(gen,"'female'=2;'Female'=2;'male'=1;'Male'=1;else =0") #NA-0
  if (genders[1]==0 & genders[2] >=1) gender <- genders[2]
  else gender <- genders[1]
  return(gender)
}

gender <- apply(CHN_1.2_Gese, 1, genderCHN1.2) #recode variable geneder, where value 0 equals NA
#table(gender)

fadnames <- c("FD1","FD5","FD9","FD13","FD17",
              "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
              "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
              "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
#length(fadnames) #check the number of items

NO <- rep("CHN1.2",times=length(CHN_1.2_Gese[,1])) 
age <- CHN_1.2_Gese[,"age"]
CHN_1.2 <- cbind(NO,age,gender,edu,CHN_1.2_Gese[,fadnames]+1) # + 1 menas recode FAD-Plus data CHN_1.2 from 0-4 to 1-5

write.csv(CHN_1.2,"CHN_1.2.csv",row.names=FALSE,fileEncoding ="UTF-8")


################################
########## CHN_1.3 #############
################################
#dataset from Li et al (2018)
#translation version CHN_1.3

CHN_1.3_Li <- read.csv("CHN_1.3_Li.csv") #CHN_1.3_Li just included gender and 27 items of FAD-Plus

NO <- rep("CHN1.3",times=length(CHN_1.3_Li[,1]))
CHN_1.3 <- cbind(NO,CHN_1.3_Li)

write.csv(CHN_1.3,"CHN_1.3.csv",row.names=FALSE,fileEncoding ="UTF-8")



################################
################################
################################
##########  ENG data ###########
################################
################################
################################
#where rearrranged the English version datasets from the original datasets from OSF


ENG1.1 <- read.csv("ENG_Brian_1A.csv")
ENG1.2 <- read.csv("ENG_Brian_1B.csv")

ENG1.3 <- read.csv("ENG_Brian_1C.csv")
age <- ENG1.3[,"Age"]
gender <- ENG1.3[,"Gender"]
ENG1.3 <- cbind(age,gender,ENG1.3[,fadnames])


ENG1.4 <- read.csv("ENG_Brian_2.csv")
describe(ENG1.4) #which FW4 needs recode
table(ENG1.4[,"FW4"])

recode13578_ENG1.4 <- function(v){ #function made for recoding FW4 in Brian2 dataset
  v[which(v==3)] <- 2
  v[which(v==5)] <- 3
  v[which(v==7)] <- 4
  v[which(v==8)] <- 5
  return(v)
} 
ENG1.4[,"FW4"] <- recode13578_ENG1.4(ENG1.4[,"FW4"])

table(ENG1.4$age)
ENG1.4$age <- ENG1.4$age+17 #recode "age" with original data with 17 difference


ENG2 <- read.csv("ENG_Lysanne.csv")


ENG3.1 <- read.csv("ENG_Nadelhoffer_1.csv")
#table(ENG3.1[,"Check"]), base on SPSS info, where should select "Disagree" == 2
ENG3.1 <- ENG3.1[which(ENG3.1[,"Check"]==2),]


ENG3.2 <- read.csv("ENG_Nadelhoffer_2.csv")
#table(ENG3.2[,"Check"]), base on SPSS info, where should select "Disagree" == 2
ENG3.2 <- ENG3.2[which(ENG3.2[,"Check"]==2),]

ENG3.3 <- read.csv("ENG_Nadelhoffer_3.csv")
#table(ENG3.3[,"Check"]), base on SPSS info, the correct answer should be 2 (questions: the day before today and the day after)
ENG3.3 <- ENG3.3[which(ENG3.3[,"Check"]==2),]

ENG3.4 <- read.csv("ENG_Nadelhoffer_4.csv")
#table(ENG3.4[,"Check"]), base on SPSS info, the correct answer should be 9 or 2 (questions: the day before today and the day after)
ENG3.4 <- ENG3.4[which(ENG3.4[,"Check"]==9),]

ENG3.5 <- read.csv("ENG_Nadelhoffer_5.csv")
#table(ENG3.5[,"Check"]), base on SPSS info, the correct answer should be 24 selecting "Disagree"
ENG3.5 <- ENG3.5[which(ENG3.5[,"Check"]==24),]


ENG <- dplyr::bind_rows(ENG1.1,ENG1.2,ENG1.3,ENG1.4,ENG2,ENG3.1,ENG3.2,ENG3.3,ENG3.4,ENG3.5)

NO <- c(rep("ENG1.1",times=length(ENG1.1[,1])),
        rep("ENG1.2",times=length(ENG1.2[,1])),
        rep("ENG1.3",times=length(ENG1.3[,1])),
        rep("ENG1.4",times=length(ENG1.4[,1])),
        rep("ENG2",times=length(ENG2[,1])),
        rep("ENG3.1",times=length(ENG3.1[,1])),
        rep("ENG3.2",times=length(ENG3.2[,1])),
        rep("ENG3.3",times=length(ENG3.3[,1])),
        rep("ENG3.4",times=length(ENG3.4[,1])),
        rep("ENG3.5",times=length(ENG3.5[,1])))
#where Brian1A: NO=ENG1.1
#Brian1B: NO=ENG1.2
#Brian1C: NO=ENG1.3
#Brian2: NO=ENG1.4
#Lysanne: NO=ENG2
#Nadelhoffer_1: NO=ENG3.1
#Nadelhoffer_2: NO=ENG3.2
#Nadelhoffer_3: NO=ENG3.3
#Nadelhoffer_4: NO=ENG3.4
#Nadelhoffer_5: NO=ENG3.5


ENG_final <- cbind(NO,ENG)
write.csv(ENG_final,"ENG.csv",row.names=FALSE,fileEncoding ="UTF-8")



################################
##########  FRN data ###########
################################
#French version study 1 only with FAD-Plus data, and study 2 dataset with BFI data, but this dataset with BFI first version 45 items, we continue to have the BFI data and dimensions' scores for now

names_BFI_FRN <- c( "BFI_1","BFI_2","BFI_3","BFI_4","BFI_5","BFI_6","BFI_7","BFI_8", "BFI_9" ,
                    "BFI_10","BFI_11","BFI_12", "BFI_13", "BFI_14","BFI_15" ,"BFI_16","BFI_17" ,"BFI_18", 
                    "BFI_19","BFI_20","BFI_21" ,"BFI_22","BFI_23" , "BFI_24","BFI_25", "BFI_26" , "BFI_27",
                    "BFI_28", "BFI_29","BFI_30","BFI_31" ,"BFI_32", "BFI_33" ,"BFI_34","BFI_35","BFI_36",
                    "BFI_37","BFI_38", "BFI_39","BFI_40","BFI_41" , "BFI_42" , "BFI_43", "BFI_44" ,"BFI_45",
                    "Extraversion","Agreabilite" , "Conscience", "EmotionsNegatives","Ouverture")

FRN_1.1 <- read.csv("FRN_Study1.csv")
FRN_1.2_BFI <- read.csv("FRN_Study2_BFI.csv")

FRN_data <- dplyr::bind_rows(FRN_1.1,FRN_1.2_BFI)
FRN_data <- FRN[,c("gender","age",fadnames,names_BFI_FRN)]


NO <- rep("FRN",times=length(FRN_data[,1]))
FRN <- cbind(NO,FRN_data) #removing the first "id" column 

write.csv(FRN,"FRN.csv",row.names=FALSE,fileEncoding ="UTF-8")


################################
########## JPN data ############
################################
#In this case, we have two translations of Japanese FAD-Plus version, the Goto version has two data sources, one with LOC data; the other version only has one source and with LOC data. 
# we will rearrange these two sources, and have two datasets following:
#JPN_1: Goto version, part of data has LOC data
#JPN_2: Watanabe version with LOC data

JPN_1_Goto2015 <- read.csv("JPN_1_Goto2015.csv") #this is the data from Goto (2015) only have FAD-Plus data with Goto translation version
JPN_2_LOC2021 <- read.csv("JPN_2_LOC2021.csv") #two datasets in different versions with LOC data

names_LOC_JPN <- c("LOC_1","LOC_2","LOC_3","LOC_4","LOC_5","LOC_6","LOC_7")
JPN_1_Goto2021_LOC <- JPN_2_LOC2021[,c("age","gender",fadnames,names_LOC_JPN)]

JPN_1 <- dplyr::bind_rows(JPN_1_Goto2015[,-1],JPN_1_Goto2021_LOC)
JPN_2 <- JPN_2_LOC2021[,c("age","gender",
                          "FAD_W_1","FAD_W_2","FAD_W_3", "FAD_W_4", "FAD_W_5", "FAD_W_6",
                          "FAD_W_7","FAD_W_8","FAD_W_9", "FAD_W_10", "FAD_W_11", "FAD_W_12",
                          "FAD_W_13","FAD_W_14","FAD_W_15", "FAD_W_16", "FAD_W_17", "FAD_W_18",
                          "FAD_W_19","FAD_W_20","FAD_W_21", "FAD_W_22", "FAD_W_23", "FAD_W_24",
                          "FAD_W_25","FAD_W_26","FAD_W_27",
                          names_LOC_JPN)]
colnames(JPN_2) <- c("age","gender",colnames(JPN_1)[1:27],names_LOC_JPN) #where we should rename the item varible names in the second version 

NO <- rep("JPN1",times=length(JPN_1[,1]))
JPN_1 <- cbind(NO,JPN_1)
write.csv(JPN_1,"JPN_1.csv",row.names=FALSE,fileEncoding ="UTF-8")

NO <- rep("JPN2",times=length(JPN_2[,1]))
JPN_2 <- cbind(NO,JPN_2)
write.csv(JPN_2,"JPN_2.csv",row.names=FALSE,fileEncoding ="UTF-8")



