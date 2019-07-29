rm(list=ls(all=TRUE))
library('tidyverse')
library('ggplot2') 
library('xlsx')
library('lme4')
library('nnet')
#library(stringr)
library(Matrix)
#library(dplyr)
#---------------------------------------------------------------------------------------------------
#Section 2
data1 <- read.csv(file.choose(),header=TRUE) ## Importing the Parking Citation CSV file
#data2 <- read.csv(file.choose(),header=TRUE) ## Importating the BPD Victim CSV file


#save(data1,file='E:\\rfs\\Summer2019\\z_Sia\\data1.RData')
#load('E:\\rfs\\Summer2019\\z_Sia\\data1.RData')
#---------------------------------------------------------------------------------------------------
#a) For all citations, what is the mean violation fine?
mean(is.na(data1$ViolFine))
MeanViolFine <- mean(data1$ViolFine)

#---------------------------------------------------------------------------------------------------
#b) Looking only at vehicles that have open penalty fees, what dollar amount is the 81st percentile
#of that group?
#SparseData1 <- lapply(data1, function(x) x[x != 0 & !is.na(x)]) # Sparse Data1, remove NA or 0 values
#SparseOpenPenalty <- SparseData1$OpenPenalty                    # Sparse Data1$OpenPenalty
#SparseOpenPenalty.percentile <- quantile(SparseOpenPenalty, prob = c(0.81)) # 81st percentile

#ecdf(SparseOpenPenalty)(81)                             #The value of the cumulative distribution
#of X at the points in Y. the probability of being below 81 (thus what percentile 81 is in the 
#sample), also good to knowsummary(SparseOpenPenalty)

## FFFFFFFFFFFFFFFFFFFFFF
# FFFFFFFFFFFFFFFFFFFFFF
mean(is.na(data1$OpenPenalty))
data1$OpenPenalty_n <- as.character(data1$OpenPenalty)
data1$OpenPenalty_n2 <- as.numeric(data1$OpenPenalty_n)
mean(is.na(data1$OpenPenalty_n2)) 
G1 <- is.na(data1$OpenPenalty_n2)  # no NAs in the original vector, however, after 
# transforming to numeric, some NAs are generated, checking to see them:
G1_w <- which(G1==TRUE)
data1$OpenPenalty_n[G1] # turns out some numbers have ',' in them, which turn to NA after transforming:
as.numeric(data1$OpenPenalty_n[G1]) # So we remove the ',' s
data1$OpenPenalty_n3 <- str_remove_all(data1$OpenPenalty_n, "[,]")
data1$OpenPenalty_n4 <- as.numeric(data1$OpenPenalty_n3)
mean(is.na(data1$OpenPenalty_n4)) # No more NAs! :)
temp <- filter(data1,OpenPenalty_n4!=0)
ggplot(temp)+geom_histogram(aes(x=OpenPenalty_n4),binwidth = 20)
ANS <- quantile(temp$OpenPenalty_n4, prob = c(0.81)) # answer is 480
sprintf(ANS, fmt = '%#.10f')

# FFFFFFFFFFFFFFFFFFFFFF

#---------------------------------------------------------------------------------------------------
#c) Find all citations where the police district has been given. Next, determine which district has
#the highest mean violation fine. What is that mean violation fine? Keep in mind that Baltimore is 
#divided into nine police districts, so clean the data accordingly.

length(unique(data1$PoliceDistrict))
temp <- filter(data1,PoliceDistrict!='')
length(unique(temp$PoliceDistrict))
temp$PoliceDistrict_2 <- as.character(temp$PoliceDistrict)
temp$PoliceDistrict_2 <- tolower(temp$PoliceDistrict_2)
unique(temp$PoliceDistrict_2)# there is one "notheastern", which has to be "northeastern" Hahaha
temp$PoliceDistrict_2[which(temp$PoliceDistrict_2=="notheastern")] <- "northeastern"
unique(temp$PoliceDistrict_2)# fixed!

t_s <-  temp %>% group_by(PoliceDistrict_2) %>%  
  summarise(mean_fine=mean(ViolFine))

t_s$PoliceDistrict_2[which(t_s$mean_fine ==max(t_s$mean_fine) )] # asnwer is "northeastern"
ANS <- t_s$mean_fine[which(t_s$mean_fine ==max(t_s$mean_fine) )]
sprintf(ANS, fmt = '%#.10f')
  #---------------------------------------------------------------------------------------------------
#d) Find the ten vehicle makes that received the most citations during 2017. For those top ten, find
#all Japanese-made vehicles. What proportion of all citations were written for those vehicles? Note
#that the naming in Make is not consistent over the whole dataset, so you will need to clean the data 
#before calculating your answer. Your answer should be expressed as a decimal number 
#(i.e. 0.42, not 42).
unique(data1$Make) # There are some starange Makes, we first delete those entries that 
# have numbers, the reason is that it seems that in some cases instead of Make the operator entered the data instead
Q <- grepl("\\d", data1$Make)
temp <- data1[which(Q==FALSE),]
unique(temp$Make) 
# Now we filter those that don't have an entry (i.e., Make=='')
temp <- filter(temp,Make!='')
# Now we filter any of them containing a ' ', since no car make have an space in it 
Q <- grepl(" ", temp$Make)
temp <- temp[which(Q==FALSE),]
unique(temp$Make) 
# Now, to take care of cases like TOYT and TOYO (i.e., to make them the same thing), we truncate the string and 
# keep only the first 3 charachters
temp$Make <- substr(temp$Make, 1, 3)
# Now we filter any of them containing a '/', since no car make have a / in it
Q <- grepl("/|//|-", temp$Make)
temp$Make[which(Q==TRUE)]
temp <- temp[which(Q==FALSE),]
unique(temp$Make) 
# Now we filter any of them containing only one or two charachters since no Make car has one or two charachters
Q <- which(nchar(temp$Make)==3 | temp$Make=='VW')
temp <- temp[Q,]
unique(temp$Make)
# Now we filter any of them containing a "."
Q <- grepl(".", temp$Make,fixed=TRUE)
temp$Make[which(Q==TRUE)]
temp <- temp[which(Q==FALSE),]
unique(temp$Make)



t_s_d <-  temp %>% group_by(Make) %>%  
  summarise(mean_fine=n())

t_s_d$Make[which(t_s_d$mean_fine ==max(t_s_d$mean_fine) )] # asnwer is "HON"

ANS <- t_s_d$mean_fine[which(t_s_d$mean_fine ==max(t_s_d$mean_fine) )] 
sprintf(ANS, fmt = '%#.10f')
#---------------------------------------------------------------------------------------------------
#e) First, find the total number of citations given in each year between 2004 and 2014 (inclusive).
#Next, using linear regression, create a function that plots the total number of citations as a 
#function of the year. If you were to plot a line using this function, what would be the slope of 
#that line?

class(data1$NoticeDate)

data1$NoticeDate_22 <- as.Date(data1$NoticeDate, format = "%m/%d/%Y")

#Q <- which(data1$NoticeDate_22 >= "2004-01-01" && data1$NoticeDate_22 < "2015-01-01")
Q <- which(data1$NoticeDate_22 >= "2004-01-01" )
temp <- data1[Q,]

Q <- which(temp$NoticeDate_22 < "2015-01-01")
temp <- temp[Q,]

unique(temp$NoticeDate_22)
min(temp$NoticeDate_22)
max(temp$NoticeDate_22)
NAzz <- is.na(temp$NoticeDate_22)
mean(NAzz)

temp$NoticeDate_33 <- format(as.Date(temp$NoticeDate_22, format="%m/%d/%Y"),"%Y")
unique(temp$NoticeDate_33)

t_s_e <-  temp %>% group_by(NoticeDate_33) %>%  
  summarise(countZ=n())

t_s_e$NoticeDate_44 <- as.numeric(t_s_e$NoticeDate_33)

t_s_e$NoticeDate_55 <- t_s_e$NoticeDate_44 - 2004

ggplot(t_s_e,aes(x=NoticeDate_33,y=countZ))+ geom_point()+
  geom_smooth(method=lm, se=TRUE)

m1 <- lm(countZ~NoticeDate_55,data=t_s_e)
summary(m1)
sprintf(m1$coefficients[2], fmt = '%#.10f')

