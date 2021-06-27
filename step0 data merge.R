rm(list = ls())

library("car")
library("dplyr")
# Sys.setlocale(category="LC_ALL",locale="en_US.UTF-8") #run this for mac OS setting the local language solving the problems in reading Chinese in FAD_1

################################
##########  FAD_1  #############
################################

fad1all <- read.csv("FAD_1_CSV.csv",encoding="UTF-8") #where resave FAD_1 as csv with UTF-8
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
head(fad1useful)

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
table(edu) #recode education level with FAD_2´s standard 

table(fad1useful$gender_gene)
table(fad1useful$gender_self_report)
genderfad1 <- function(a){
  gen <- a[3:4]
  genders <- car::recode(gen,"'female'=2;'Female'=2;'male'=1;'Male'=1;else =0") #NA-0
  if (genders[1]==0 & genders[2] >=1) gender <- genders[2]
  else gender <- genders[1]
  return(gender)
}

gender <- apply(fad1useful, 1, genderfad1) #recode variable geneder, use the biological gender
table(gender)

fadnames <- c("FD1","FD5","FD9","FD13","FD17",
              "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
              "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
              "FW4","FW8","FW12","FW16","FW21","FW23","FW26")
length(fadnames)

NO <- rep(1,times=length(fad1useful[,1])) #new mark variable, distinguishes different original datasets
age <- fad1useful[,2]
fad1final <- cbind(NO,age,gender,edu,fad1useful[,fadnames]+1,fad1useful[,33:92]) #recode FAD_1 from 0-4 to 1-5
head(fad1final)

write.csv(fad1final,"NEW_FAD_1.csv",row.names=FALSE,fileEncoding ="UTF-8")

################################
##########  FAD_2  #############
################################

fad2all <- read.csv("FAD_2.csv")
head(fad2all)
table(fad2all$native)
table(fad2all$age) #1 subject's age = 19.5
table(fad2all$check)

usefulnames2 <- c("age","gender","edu",
                  "FD1","FD5","FD9","FD13","FD17",
                  "SD2","SD6","SD10","SD14","SD18","SD22","SD24",
                  "UP3","UP7","UP11","UP15","UP19","UP20","UP25","UP27",
                  "FW4","FW8","FW12","FW16","FW21","FW23","FW26",
                  "IPC1","IPC2","IPC3","IPC4","IPC5","IPC6","IPC7","IPC8","IPC9","IPC10","IPC11","IPC12","IPC13","IPC14","IPC15","IPC16","IPC17","IPC18","IPC19","IPC20","IPC21","IPC22","IPC23","IPC24",
                  "BFI_A1","BFI_A2","BFI_A3","BFI_A4","BFI_A5","BFI_A6","BFI_A7","BFI_A8","BFI_A9",
                  "BFI_C1","BFI_C2","BFI_C3","BFI_C4","BFI_C5","BFI_C6","BFI_C7","BFI_C8","BFI_C9",
                  "BFI_N1","BFI_N2","BFI_N3","BFI_N4","BFI_N5","BFI_N6","BFI_N7","BFI_N8",
                  "BFI_O1","BFI_O2","BFI_O3","BFI_O4","BFI_O5","BFI_O6","BFI_O7","BFI_O8","BFI_O9","BFI_O10",
                  "BFI_E1","BFI_E2","BFI_E3","BFI_E4","BFI_E5","BFI_E6","BFI_E7","BFI_E8")

fad2useful <- fad2all[,usefulnames2] #useful names in collected dataset, FAD+, IPC & BIG FIVE INVENTORY with 5 dimensions x 8 items
NO <- rep(2,times=length(fad2useful[,1]))

fad2final <- cbind(NO,fad2useful)
head(fad2final)
write.csv(fad2final,"NEW_FAD_2.csv",row.names=FALSE,fileEncoding ="UTF-8")


fad12 <- dplyr::bind_rows(fad1final,fad2final)
head(fad12)
table(fad12$NO)

################################
##########  FAD_3  #############
################################

fad3all <- read.csv("FAD_3.csv")
head(fad3all)

NO <- rep(3,times=length(fad3all[,1]))
fad3final <- cbind(NO,fad3all)
head(fad3final)

fad123 <- dplyr::bind_rows(fad12,fad3final)
head(fad123)
table(fad123$NO)

################################
##########  FAD_A  #############
################################

fadAall <- read.csv("NEW_FAD_A.csv")
head(fadAall)

NO <- rep(4,times=length(fadAuseful[,1]))
fadAfinal <- cbind(NO,fadAall)
head(fadAfinal)



################################
##########  FAD_F  #############
################################

fadFall <- read.csv("FAD_F.csv")
head(fadFall)
gender <- fadFall$Sexe
age <- fadFall$Age
NO <- rep(5,times=length(fadFall[,1]))
fadFfinal <- cbind(NO,gender,age,fadFall[,fadnames])
head(fadFfinal)



fadAF <- dplyr::bind_rows(fadAfinal,fadFfinal)
head(fadAF)

################################
##########  FAD_F  #############
################################

fadJall <- read.csv("FAD_J.csv")
head(fadJall)

NO <- rep(6,times=length(fadJall[,1]))
fadJfinal <- cbind(NO,fadJall[,fadnames])
head(fadJfinal)



fadAFJ <- dplyr::bind_rows(fadAF,fadJfinal)
head(fadAFJ)
table(fadAFJ$NO)

g <- c(rep(0,length(fad123[,1])),rep(1,length(fadAFJ[,1]))) #other new mark variable for CHINESE DATASETS & FOREIGN DATASETS

fadALL <- dplyr::bind_rows(fad123,fadAFJ)
fadALLfinal <- cbind(g,fadALL)
head(fadALLfinal)
table(fadALLfinal$g)

write.csv(fad123,"fadchina.csv",row.names=FALSE,fileEncoding ="UTF-8")
write.csv(fadALLfinal,"fadallbynow.csv",row.names=FALSE,fileEncoding ="UTF-8")

