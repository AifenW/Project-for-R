
# Import data

install.packages("readxl")
library(readxl)

origene_salesdata<-read_excel("C:/Users/awang/Documents/2018-1-22/SAS_R/R/RLearning/RProject/Project 6 Origene sales data analysis/Data Cleaning/origene_salesdata.xlsx")

  # glimpse origene_salesdata

library(tidyverse)

glimpse(origene_salesdata)

# subset salesdata which contain only 5 attributes

install.packages("sqldf")
library(sqldf)

subset_salesdata<-sqldf("
   select Customer, `Sales Order #`, Amount, Date, Email
   from origene_salesdata
         ")

nrow(subset_salesdata)   #248996
length(unique(subset_salesdata$Customer)) #12172

# create new_salesdata1 which Amount>0 and not from Origene

nointer_sd<-subset_salesdata %>%
  filter(!grepl("@origene",Email))%>%
  filter(Amount>0)

head(nointer_sd)
nrow(nointer_sd) #235877 (Amount>0)  248331 (!@origene) 235877 (both)
length(unique(nointer_sd$Email)) #9814

length(unique(nointer_sd$Customer)) #12006

# split Customer into Account_ID and Customer_ID

install.packages("stringi")
library(stringi)

bftrans_sd<-nointer_sd %>%
  mutate(Account_ID=lapply(strsplit(Customer, " "),function(n)n[[1]][1]),
         Customer_ID=lapply(lapply(strsplit(Customer, " "), function(x)x[-1]),paste, collapse=" "),
         Customer_ID=stri_trim(tolower(Customer_ID))
  ) %>%
  rename(SalesOrder='Sales Order #') %>%
  select(-Customer)

bftrans_sd<-bftrans_sd[,c("Account_ID","Customer_ID","SalesOrder","Amount", "Date", "Email")]

glimpse(bftrans_sd)
head(bftrans_sd)
nrow(bftrans_sd) #235877
length(unique(bftrans_sd$Customer_ID))  #11535

# check bftrans_sd

check<-bftrans_sd %>%
  filter(str_detect(Customer_ID,"univ")) 

check<-bftrans_sd %>%
  filter(str_detect(Customer_ID,":")) 

check<-bftrans_sd %>%
  filter(str_detect(Customer_ID,"-")) 

head(check)
tail(check)

# first cleaning of bftrans_sd which convert "university" in Customer_ID to "univ"

install.packages("stringr")
library(stringr)

fclean_bftrans_sd<- bftrans_sd %>%
    mutate(
    Customer_ID=lapply(str_split(Customer_ID, "[:]"),function(n)n[[1]][1]),
    Customer_ID=str_trim(sub("university","univ",x=Customer_ID)))  

nrow(fclean_bftrans_sd) #235877
length(unique(fclean_bftrans_sd$Customer_ID)) #10879
head(fclean_bftrans_sd)
class(fclean_bftrans_sd$Email)

# Second time to clean fclean_bftrans_sd

# produce a list of near matches for Customer_ID 

# create empty dataframe

library(stringdist)

df<-data.frame(Account_ID=character(),Salesorder=character(),Amount=double(),Date=as.Date(character()),Email=character(),LikelyGroup=integer(),Group_Customer_ID=character())



for (i in 0:floor(n/10000))
{
  n<-nrow(fclean_bftrans_sd)   #235877
  start=i*10000+1
  end= (i+1)*10000
  if(n>end)
  {
    lclean_bftrans_sd<-fclean_bftrans_sd[start:end,]
  }else{
    lclean_bftrans_sd<-fclean_bftrans_sd[start:n,] 
  }
  dist_Customer_ID <- stringdistmatrix(lclean_bftrans_sd$Customer_ID,lclean_bftrans_sd$Customer_ID,useNames="strings",method="jw",p=0.1)
  
  row.names(dist_Customer_ID)<-lclean_bftrans_sd$Customer_ID
  names(dist2_Customer_ID)<-lclean_bftrans_sd$Customer_ID
  dist_Customer_ID<-as.dist(dist_Customer_ID)
  
  #Hierarchical clustering to find closest
  
  clusts<-hclust(dist_Customer_ID,method="ward.D2")
  
  #Cut into appropriate clusters based upon height in the dendrogram
  
  lclean_bftrans_sd$LikelyGroup<-cutree(clusts,h=0.1)
  
  #Define "mode" function which only selects one mode even in bimodal cases.
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  #Select modal name for each group
  
  lclean_bftrans_sd<-lclean_bftrans_sd%>%
    group_by(LikelyGroup)%>%
    mutate(Group_Customer_ID=Mode(Customer_ID))
  
  df=rbind(df,as.data.frame(lclean_bftrans_sd))
  
}

nrow(df)  #235877

length(unique(df$Customer_ID))         #10879
length(unique(df$Group_Customer_ID))   #10716
head(df)
check<-df %>%
  filter(Customer_ID != Group_Customer_ID)
head(check)
tail(check)
nrow(check)  # 1336
length(unique(check$Customer_ID))         #534
length(unique(check$Group_Customer_ID))   #500

# final cleaned data ...........will be used for RFM analysis

final_tran_sd<-df  
nrow(df)
nrow(final_tran_sd)
class(df)
class(final_tran_sd)
# data subset for Customer_ID is different from Group_Customer_ID

diff_CustomerID<-final_tran_sd %>%
  filter(Customer_ID != Group_Customer_ID)
nrow(diff_CustomerID)  #1336

# export fntrans_sd for RFM analysis

library(openxlsx)
final_tran_sd<-openxlsx::write.xlsx(final_tran_sd, file="C:/Users/awang/Documents/2018-1-22/SAS_R/R/RLearning/RProject/Project 6 Origene sales data analysis/Data Cleaning/final_tran_sd.xlsx")

diff_CustomerID<-openxlsx::write.xlsx(diff_CustomerID, file="C:/Users/awang/Documents/2018-1-22/SAS_R/R/RLearning/RProject/Project 6 Origene sales data analysis/Data Cleaning/diff_CustomerID.xlsx")

install.packages("xlsx")
library(xlsx)
write.xlsx(df, file="C:/Users/awang/Documents/2018-1-22/SAS_R/R/RLearning/RProject/Project 6 Origene sales data analysis/Data Cleaning/final_tran_sd.xlsx")

