######################################################################
# customer churn survey 

setwd("/Users/admin/Documents/Linh-R Studio/CHURN 2019")

library(tidyverse)
library(readxl)
library(knitr)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(writexl)

Khaosat <- read.csv("Mapping_Tel_LOSING_TREASURE_20191122.csv", header = TRUE, sep = ";")
Channel <- read.csv("CHANNEL_20191128.csv", header = TRUE, sep = ",")
NewLabel <- read.csv("NEW_LIST_CUSTOMER_FOR_NEW_LABEL.csv", header = TRUE, sep = ",")

df1 <- Khaosat

####################################
### CHANGE WIDE TO LONG DATA FORMAT  
###################################
data_long <- gather(NewLabel, condition, measurement, CHURN_LABEL:CHURN_ALL_KV_LABEL, factor_key=TRUE)
data_long$condition <- as.character(data_long$condition)

####################################
### CHANGE VALUE in order to filter 
###################################
data_long$condition[data_long$condition == "CHURN_LABEL"] <- "Itself"
data_long$condition[data_long$condition == "CHURN_KV_LABEL"] <- "Cluster"
data_long$condition[data_long$condition == "CHURN_ALL_KV_LABEL"] <- "HCM"
data_long

tail(data_long,10)


####################################
# DATA GUI BEO- DASHBOARD  
####################################
LOSING_TREASURE_28Nov19 <- merge(data_long,Khaosat, by = "CUSTCODE")
write_xlsx(x = LOSING_TREASURE_28Nov19, path = "LOSING_TREASURE_28Nov19.xlsx", col_names = TRUE)




##################
# Remove column 
##################
df1 <- df1[-c(35:36)]
df1 <- subset(df1, CUST_KHU_VUC == "HO CHI MINH")
df1 <- df1 %>% filter(!str_detect(df1$PRODID, "^P"))
str(df1)

########################################
# Lay ra 250 Khach hang survey Phase 2  
#######################################

# Khong su dung remove NA duoc ma phai dung filter vi "Phan loai cau 1" la bien Factor, cac dap an la dang cac
# levels # nhau, missing data "NA" cung la 1 level/ 1 dap an lua chon, nen phai dung filter chu khong dung remove NA value nhu doi voi bien numeric 
df2 <- df1
df2$Phân.loại.Câu.1. <- factor(df2$Phân.loại.Câu.1.)
str(df2)                      
df2 <- df2 %>% filter(Phân.loại.Câu.1. != "") # Bo level "NA", lay levels la dap an tra loi 
df2 <- df2[-c(47)]

mean(df2$Recency)

########################################
# 1. Thong tin tong quan ve 50 khach nay (TABLE)  
#######################################
a <- df2 %>% group_by(CHURN_LABEL,Phân.loại.Câu.1.,TYPE) %>% summarise(n=n(), Mean_Age = mean(AGE),Mean_Recency = mean(Recency), FST_Yr = mean(FRM_FST_TIME), Freqency = mean(Frequency)) %>%
  mutate(freq = paste0(round(100 * n/sum(n), 0), "%")) 

a_0 <- df2 %>% group_by(CHURN_LABEL,Phân.loại.Câu.1.,GENDER) %>%
  summarise(n=n()) %>%
  mutate(freq = paste0(round(100 * n/sum(n), 0), "%")) 

write_xlsx(x = a, path = "a.xlsx", col_names = TRUE) # Xuat ra file excel 
write_xlsx(x = a_0, path = "a_0.xlsx", col_names = TRUE) # Xuat ra file excel 

#################
### CHURN_L1 ###
################
# Cardtype
a1 <- df2 %>% filter(CHURN_LABEL == "CHURN_L1") %>%
  group_by(CARDTYPE) %>% 
  summarise(n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))
write_xlsx(x = a1, path = "a1.xlsx", col_names = TRUE) # Xuat ra file excel 

# What they by (CHART)
a2 <- df2 %>% filter(CHURN_LABEL == "CHURN_L1", TYPE == "NORMAL") %>%
  group_by(BRANDNAME, ITEM) %>% 
  summarise(n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))
# Tach text bang dau ","
a2$BRANDNAME <- as.character(a2$BRANDNAME)
s <- strsplit(a2$BRANDNAME, split = ",")
u <- data.frame(V1 = rep(a2$BRANDNAME, sapply(s, length)), V2 = unlist(s))
# Ve chart cho sp 
k <- as.data.frame(u %>% group_by(u$V2) %>% summarise(n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%")) %>% arrange(desc(n)))

write_xlsx(x = k, path = "k.xlsx", col_names = TRUE) # Xuat ra file excel 


#################
### CHURN_L2 ###
################# 
Lieu rang day la dam khach mua Nhan cuoi roi thoi? 
# Cardtype
a3 <- df2 %>% filter(CHURN_LABEL == "CHURN_L2" & Phân.loại.Câu.1. == "Khách không có nhu cầu mua""") %>%
  group_by(CARDTYPE) %>% 
  summarise(n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))

write_xlsx(x = a3, path = "a3.xlsx", col_names = TRUE) # Xuat ra file excel 

############# FIRST
# What they by (CHART) -MNHOM FIRTS n NORMAL 
a4 <- df2 %>% filter(CHURN_LABEL == "CHURN_L2" & Phân.loại.Câu.1. == "Khách không có nhu cầu mua """ & TYPE == "FIRST") %>%
  group_by(BRANDNAME, ITEM) %>% 
  summarise(n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))
# Tach text bang dau ","
a4$BRANDNAME <- as.character(a4$BRANDNAME)
s1 <- strsplit(a4$BRANDNAME, split = ",")
u1 <- data.frame(V1 = rep(a4$BRANDNAME, sapply(s1, length)), V2 = unlist(s1))
u1 <- u1 %>% filter(V2 != "nan")
# Ve chart cho sp 
u1 <- as.data.frame(u1 %>% group_by(u1$V2) %>% summarise(n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%")) %>% arrange(desc(n)))
write_xlsx(x = u1, path = "u1.xlsx", col_names = TRUE) # Xuat ra file excel 

############# NORMAL
a4 <- df2 %>% filter(CHURN_LABEL == "CHURN_L2" & Phân.loại.Câu.1. == "Khách không có nhu cầu mua """ & TYPE == "NORMAL") %>%
  group_by(BRANDNAME, ITEM) %>% 
  summarise(n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))
# Tach text bang dau ","
a4$BRANDNAME <- as.character(a4$BRANDNAME)
s1 <- strsplit(a4$BRANDNAME, split = ",")
u1 <- data.frame(V1 = rep(a4$BRANDNAME, sapply(s1, length)), V2 = unlist(s1))
u1 <- u1 %>% filter(V2 != "nan")

# Ve chart cho sp 
u22 <- as.data.frame(u1 %>% group_by(u1$V2) %>% summarise(n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%")) %>% arrange(desc(n)))

write_xlsx(x = u22, path = "u22.xlsx", col_names = TRUE) # Xuat ra file excel 



### CHURN_L2 - Phan nan ve "" ###
a5 <- df2 %>% filter(CHURN_LABEL == "CHURN_L2" & Phân.loại.Câu.1. == "Khách phàn nàn về """) %>%
  group_by(CARDTYPE) %>% 
  summarise(n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))

write_xlsx(x = a5, path = "a5.xlsx", col_names = TRUE) # Xuat ra file excel 

# What they by 
a6 <- df2 %>% filter(CHURN_LABEL == "CHURN_L2" & Phân.loại.Câu.1. == "Khách phàn nàn về """) %>%
  group_by(BRANDNAME, ITEM) %>% 
  summarise(n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))
# Tach text bang dau "," (CHART) day la nhom khong phai mua Nhan cuoi ma ho mua sp thuc su 
a6$BRANDNAME <- as.character(a6$BRANDNAME)
s2 <- strsplit(a6$BRANDNAME, split = ",")
u2 <- data.frame(V1 = rep(a6$BRANDNAME, sapply(s2, length)), V2 = unlist(s2))
# Ve chart cho sp 
u2 <- as.data.frame(u2 %>% group_by(u2$V2) %>% summarise(n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%")) %>% arrange(desc(n)))

write_xlsx(x = u2, path = "u2.xlsx", col_names = TRUE) # Xuat ra file excel 

# Improve and comback 
names(df2)[38]<-"Improvment_ComeBack" # Rename column by number of column
a7 <- df2 %>% filter(CHURN_LABEL == "CHURN_L2" & Phân.loại.Câu.1. == "Khách phàn nàn về """) %>%
  group_by(Improvment_ComeBack) %>% 
  summarise(n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))

write_xlsx(x = a7, path = "a7.xlsx", col_names = TRUE) # Xuat ra file excel 


#### Nhu cau tuong lai va quay lai mua
names(df2)[40]<-"Future_Demand"
a8 <- df2 %>% group_by(CHURN_LABEL,Phân.loại.Câu.1., Future_Demand) %>%
  summarise(n=n()) %>%
  mutate(freq = paste0(round(100 * n/sum(n), 0), "%")) 

write_xlsx(x = a8, path = "a8.xlsx", col_names = TRUE) # Xuat ra file excel 


##### Mua san pham cua thuong hieu khac
names(df2)[44]<-"OtherBrandnames"
names(df2)[45]<-"Brandnames"
a9 <- df2 %>% group_by(CHURN_LABEL,Phân.loại.Câu.1.,OtherBrandnames) %>%
  summarise(n=n()) %>%
  mutate(freq = paste0(round(100 * n/sum(n), 0), "%")) 

write_xlsx(x = a9, path = "a9.xlsx", col_names = TRUE) # Xuat ra file excel 

### Do la thuong hieu gi
BRAND <- as.data.frame(df2 %>% select(Brandnames))
BRAND$Brandnames <- as.character(BRAND$Brandnames)
b1 <- strsplit(BRAND$Brandnames, split = ",")
b2 <- data.frame(V1 = rep(BRAND$Brandnames, sapply(b1, length)), V2 = unlist(b1))


# ####### NHOM KHACH CHI MUA NHAN 
# df3 <- df2
# h <- as.data.frame(df3 %>% select(CUSTCODE, BRANDNAME, CHURN_LABEL, Phân.loại.Câu.1.))
# h$BRANDNAME <- as.character(h$BRANDNAME)
# h1 <- h %>% select(CUSTCODE, BRANDNAME)
# h1$BRANDNAME <- as.character(h1$BRANDNAME)
# h11 <- strsplit(h1$BRANDNAME, split = ",")
# h2 <- data.frame(CUSTCODE = rep(h1$CUSTCODE, sapply(h11, length)), V2 = unlist(h11))
# h3 <- h2 %>% group_by(CUSTCODE) %>% filter(V2 == "Nhẫn cưới")
# h4 <- h3[!duplicated(h3$CUSTCODE), ]
# h5 <- merge(h4,h,by = "CUSTCODE")
# #######
