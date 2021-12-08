
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
Sys.setlocale(category="LC_ALL",locale="en_US.UTF-8") #run this for mac OS setting the local language solving the problems in reading Chinese in FAD_1

################################
##########  FAD_1  #############
################################
#dataset from Gese DNA, with BIG5 data
#traduction version Old_V1.2

fad1all <- read.csv("FAD_1_CSV.csv",encoding="UTF-8",header = T) #where resave FAD_1 as csv with UTF-8
head(fad1all[,1:10])

usefulnames1 <- c("id", "age","gender_gene","gender_self_report","education_level",
                  "FD1","FD5","FD9","FD13","FD17",
                  "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                  "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27", 
                  "FW4","FW8","FW12","FW16","FW21","FW23","FW26",
                  "BIG5_A1","BIG5_A2","BIG5_A3","BIG5_A4","BIG5_A5","BIG5_A6","BIG5_A7","BIG5_A8","BIG5_A9","BIG5_A10","BIG5_A11","BIG5_A12",
                  "BIG5_N1","BIG5_N2","BIG5_N3","BIG5_N4","BIG5_N5","BIG5_N6","BIG5_N7","BIG5_N8","BIG5_N9","BIG5_N10","BIG5_N11","BIG5_N12",
                  "BIG5_C1","BIG5_C2","BIG5_C3","BIG5_C4","BIG5_C5","BIG5_C6","BIG5_C7","BIG5_C8","BIG5_C9","BIG5_C10","BIG5_C11","BIG5_C12", 
                  "BIG5_E1","BIG5_E2","BIG5_E3","BIG5_E4","BIG5_E5","BIG5_E6","BIG5_E7","BIG5_E8", "BIG5_E9","BIG5_E10","BIG5_E11","BIG5_E12", 
                  "BIG5_O1","BIG5_O2","BIG5_O3", "BIG5_O4","BIG5_O5","BIG5_O6","BIG5_O7","BIG5_O8","BIG5_O9","BIG5_O10","BIG5_O11","BIG5_O12")
#useful names in collected dataset, FAD+ & BIG FIVE INVENTORY with 5 dimensions x 12 items

fad1useful <- fad1all[,usefulnames1]


table(fad1useful$education_level)
educationlvl <- as.vector(fad1useful$education_level)
edu <- car::recode(educationlvl,"'小学及以下'=1; 
                   '初中'=2;
                   '中专或职高'=3;
                   '高中'=3;
                   '大专'=4;
                   '本科'=5;
                   '硕士'=6;
                   '博士'=7;else=0")
table(edu) #recode education´s level with FAD_2´s standard 

table(fad1useful$gender_gene)
table(fad1useful$gender_self_report)
genderfad1 <- function(a){ 
  gen <- a[3:4]
  genders <- car::recode(gen,"'female'=2;'Female'=2;'male'=1;'Male'=1;else =0") #NA-0
  if (genders[1]==0 & genders[2] >=1) gender <- genders[2]
  else gender <- genders[1]
  return(gender)
}
#function for gender, take the genetic gender in our case

gender <- apply(fad1useful, 1, genderfad1) #recode variable geneder, use the biological gender
table(gender)

fadnames <- c("FD1","FD5","FD9","FD13","FD17",
              "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
              "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
              "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
length(fadnames) #check the number of items

NO <- rep(1,times=length(fad1useful[,1])) #new mark variable, distinguishes different original datasets
age <- fad1useful[,2]
fad1final <- cbind(NO,age,gender,edu,fad1useful[,fadnames]+1,fad1useful[,33:92]) #recode FAD_1 from 0-4 to 1-5; and take the big five´s variables

write.csv(fad1final,"NEW_FAD_1.csv",row.names=FALSE,fileEncoding ="UTF-8")

################################
##########  FAD_2  #############
################################
#dataset from datapapaer Liu et al (2020), 4 sub-dataset, the 4th dataset with BFI & MLOC data
#dataset 2.5 from Kong Yuwei (Jiang Xi)
#traduction version Old_V1.1

fad2.1 <- read.csv("FADGS_dataset1_clean.csv")
fad2.2 <- read.csv("FADGS_dataset2_clean.csv")
fad2.3 <- read.csv("FADGS_dataset3_clean.csv")
fad2.4 <- read.csv("FADGS_dataset4_clean.csv")
fad2.5 <- read.csv("FAD_Kong.csv")

usefulnames2 <- c("age","gender","edu",
                  "FD1","FD5","FD9","FD13","FD17",
                  "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                  "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                  "FW4","FW8","FW12","FW16","FW21","FW23","FW26")

fad2.1 <- fad2.1[,usefulnames2]
fad2.2 <- fad2.2[,usefulnames2]
fad2.3 <- fad2.3[,usefulnames2]

usefulnames2.4 <- c("age","gender","edu",
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
                  "MLOC2","MLOC6","MLOC7","MLOC10","MLOC12","MLOC14","MLOC16","MLOC24")

fad2.4 <- fad2.4[,usefulnames2.4]

fad2 <- dplyr::bind_rows(fad2.1,fad2.2,fad2.3,fad2.4,fad2.5)

NO <- c(rep(2.1,times=length(fad2.1[,1])),
        rep(2.2,times=length(fad2.2[,1])),
        rep(2.3,times=length(fad2.3[,1])),
        rep(2.4,times=length(fad2.4[,1])),
        rep(2.5,times=length(fad2.5[,1])))
fad2final <- cbind(NO,fad2)
write.csv(fad2final,"NEW_FAD_2.csv",row.names=FALSE,fileEncoding ="UTF-8")



################################
##########  FAD_3  #############
################################
#dataset from Li et al (2018)
#traduction version Old_V1.3

fad3all <- read.csv("FAD_3.csv") #cause FAD3 just included gender and 27 items of FAD+

NO <- rep(3,times=length(fad3all[,1]))
fad3final <- cbind(NO,fad3all)

write.csv(fad3final,"NEW_FAD_3.csv",row.names=FALSE,fileEncoding ="UTF-8")



################################
##########  FAD_A  #############
################################
#where rearrranged the English version datasets from the original datasets from OSF
#Brian1A: NO=5.11
#Brian1B: NO=5.12
#Brian1C: NO=5.13
#Brian2: NO=5.2
#Lysanne: NO=5.3


Brian1A <- read.csv("Brian_1A.csv")
Brian1B <- read.csv("Brian_1B.csv")


Brian1C <- read.csv("Brian_1C.csv")
age <- Brian1C[,"Age"]
gender <- Brian1C[,"Gender"]
Brian1C <- cbind(age,gender,Brian1C[,fadnames])


Brian2 <- read.csv("Brian_2.csv")
describe(Brian2) #which FW4 needs recode
table(Brian2[,6])

recode13578 <- function(v){ #function made for recoding FW4 in Brian2 dataset
  v[which(v==3)] <- 2
  v[which(v==5)] <- 3
  v[which(v==7)] <- 4
  v[which(v==8)] <- 5
  return(v)
} 
Brian2[,6] <- recode13578(Brian2[,6])

table(Brian2$age)
Brian2$age <- Brian2$age+17 #recode "age" with original data with 17 difference

Lysanne <- read.csv("Lysanne.csv")

fadAall <- dplyr::bind_rows(Brian1A,Brian1B,Brian1C,Brian2,Lysanne)

NO <- c(rep(5.11,times=length(Brian1A[,1])),
        rep(5.12,times=length(Brian1B[,1])),
        rep(5.13,times=length(Brian1C[,1])),
        rep(5.2,times=length(Brian2[,1])),
        rep(5.3,times=length(Lysanne[,1])))

fadAfinal <- cbind(NO,fadAall)
write.csv(fadAfinal,"NEW_FAD_A.csv",row.names=FALSE,fileEncoding ="UTF-8")


################################
##########  FAD_F  #############
################################
#French version dataset - marked with NO 6

fadFall <- read.csv("FAD_F.csv")
gender <- fadFall$Sexe
age <- fadFall$Age
NO <- rep(6,times=length(fadFall[,1]))
fadFfinal <- cbind(NO,gender,age,fadFall[,fadnames])

write.csv(fadFfinal,"NEW_FAD_F.csv",row.names=FALSE,fileEncoding ="UTF-8")

################################
##########  FAD_J  #############
################################
#Japanese version dataset - marked with NO 7

fadJ1 <- read.csv("FAD_J_1.csv")
fadJ2 <- read.csv("FAD_J_2.csv")


NO <- c(rep(7.1,times=length(fadJ1[,1])),
        rep(7.2,times=length(fadJ2[,1])))

fadJall <- rbind(fadJ1[,fadnames],fadJ2[,fadnames])
        
fadJfinal <- cbind(NO,fadJall)

write.csv(fadJfinal,"NEW_FAD_J.csv",row.names=FALSE,fileEncoding ="UTF-8")




