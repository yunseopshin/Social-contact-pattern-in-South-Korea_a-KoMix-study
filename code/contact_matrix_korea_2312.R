setwd('C:/project/Social-contact-pattern-in-South-Korea-a-KoMix-study/data/Survey on Changes in Contact Patterns After COVID-19 (23.11)')
data= read.csv('Data(2311).csv',fileEncoding = "utf-8")
library(stringr)
library(dplyr)

colnames(data)
contact_num_index = 8:57 # Age group columns (Q8A_1_1 to Q8A_50_1): 50 columns


#############################################################################
# If the number of COVID-19 vaccinated NA, we replace NA to 0
data$Q3A[is.na(data$Q3A)]=0

# Age, Weekday/Weekend, Number of contact, Age group of contactees, Mask time, Influenza vaccinated, Infected COVID-19, The number of COVID-19 vaccinated
# New column positions: SQ2(1), SQ5AA(2), Q8(7), Q8A_1_1~Q8A_50_1(8-57), Q4(5), Q7(6), Q2(3), Q3A(4)
contact_data = data[,c(1,2,7,contact_num_index,5,6,3,4)]
contact_data[is.na(contact_data)]=0



#############################################################################################################################
# Contact matrix, Weekday contact matrix, Weekend contact matrix, Low mask time contact matrix, High mask time contact matrix, 
phi =  matrix(0,nrow=10,ncol=10)
phi_week = matrix(0,nrow=10,ncol=10)
phi_weekend = matrix(0,nrow=10,ncol=10)

phi_mask_low =  matrix(0,nrow=10,ncol=10)
phi_mask_low_week = matrix(0,nrow=10,ncol=10)
phi_mask_low_weekend = matrix(0,nrow=10,ncol=10)

phi_mask_high =  matrix(0,nrow=10,ncol=10)
phi_mask_high_week = matrix(0,nrow=10,ncol=10)
phi_mask_high_weekend = matrix(0,nrow=10,ncol=10)

phi_flu_vaccine =  matrix(0,nrow=10,ncol=10)
phi_flu_vaccine_week = matrix(0,nrow=10,ncol=10)
phi_flu_vaccine_weekend = matrix(0,nrow=10,ncol=10)

phi_no_flu_vaccine =  matrix(0,nrow=10,ncol=10)
phi_no_flu_vaccine_week = matrix(0,nrow=10,ncol=10)
phi_no_flu_vaccine_weekend = matrix(0,nrow=10,ncol=10)

phi_covid_infect =  matrix(0,nrow=10,ncol=10)
phi_covid_infect_week = matrix(0,nrow=10,ncol=10)
phi_covid_infect_weekend = matrix(0,nrow=10,ncol=10)

phi_no_covid_infect =  matrix(0,nrow=10,ncol=10)
phi_no_covid_infect_week = matrix(0,nrow=10,ncol=10)
phi_no_covid_infect_weekend = matrix(0,nrow=10,ncol=10)

phi_covid_vaccine =  matrix(0,nrow=10,ncol=10)
phi_covid_vaccine_week = matrix(0,nrow=10,ncol=10)
phi_covid_vaccine_weekend = matrix(0,nrow=10,ncol=10)

phi_no_covid_vaccine =  matrix(0,nrow=10,ncol=10)
phi_no_covid_vaccine_week = matrix(0,nrow=10,ncol=10)
phi_no_covid_vaccine_weekend = matrix(0,nrow=10,ncol=10)

################################
# Age group of contact matrix
age=c(0,3,7,13,16,19,30,40,50,60)
head(contact_data)
#############################################################################################################################
# Construct contact matrix
contact_week = contact_data %>% filter(SQ5AA==5)
contact_weekend = contact_data %>% filter(SQ5AA==2)

contact_mask_low = contact_data %>% filter(Q4<4)
contact_mask_low_week = contact_mask_low %>% filter(SQ5AA==5)
contact_mask_low_weekend = contact_mask_low %>% filter(SQ5AA==2)

contact_mask_high = contact_data %>% filter(Q4>=4)
contact_mask_high_week = contact_mask_high %>% filter(SQ5AA==5)
contact_mask_high_weekend = contact_mask_high %>% filter(SQ5AA==2)

contact_mask_high = contact_data %>% filter(Q4>=4)
contact_mask_high_week = contact_mask_high %>% filter(SQ5AA==5)
contact_mask_high_weekend = contact_mask_high %>% filter(SQ5AA==2)

contact_vaccine = contact_data %>% filter(Q7==1)
contact_vaccine_week = contact_vaccine %>% filter(SQ5AA==5)
contact_vaccine_weekend = contact_vaccine %>% filter(SQ5AA==2)

contact_no_vaccine = contact_data %>% filter(Q7==2)
contact_no_vaccine_week = contact_no_vaccine %>% filter(SQ5AA==5)
contact_no_vaccine_weekend = contact_no_vaccine %>% filter(SQ5AA==2)

contact_covid_infect = contact_data %>% filter(Q2==1)
contact_covid_infect_week = contact_covid_infect %>% filter(SQ5AA==5)
contact_covid_infect_weekend = contact_covid_infect %>% filter(SQ5AA==2)

contact_no_covid_infect = contact_data %>% filter(Q2==2)
contact_no_covid_infect_week = contact_no_covid_infect %>% filter(SQ5AA==5)
contact_no_covid_infect_weekend = contact_no_covid_infect %>% filter(SQ5AA==2)

contact_covid_vaccine = contact_data %>% filter( (SQ2>=20 & Q3A >=3) | (SQ2 < 20 & Q3A >=1))
contact_covid_vaccine_week = contact_covid_vaccine %>% filter(SQ5AA==5)
contact_covid_vaccine_weekend = contact_covid_vaccine %>% filter(SQ5AA==2)

contact_no_covid_vaccine = contact_data %>%  filter( (SQ2>=20 & Q3A <3) | (SQ2 < 20 & Q3A < 1))
contact_no_covid_vaccine_week = contact_no_covid_vaccine %>% filter(SQ5AA==5)
contact_no_covid_vaccine_weekend = contact_no_covid_vaccine %>% filter(SQ5AA==2)


for( i in 1:10){
  if(i !=10){
    temp = contact_data %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_week = contact_week %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_weekend = contact_weekend %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    
    temp_mask_low = contact_mask_low %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_mask_low_week = contact_mask_low_week %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_mask_low_weekend = contact_mask_low_weekend %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    
    temp_mask_high = contact_mask_high %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_mask_high_week = contact_mask_high_week %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_mask_high_weekend = contact_mask_high_weekend %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    
    temp_vaccine = contact_vaccine %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_vaccine_week = contact_vaccine_week %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_vaccine_weekend = contact_vaccine_weekend %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    
    temp_no_vaccine = contact_no_vaccine %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_no_vaccine_week = contact_no_vaccine_week %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_no_vaccine_weekend = contact_no_vaccine_weekend %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    
    temp_covid_infect = contact_covid_infect %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_covid_infect_week = contact_covid_infect_week %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_covid_infect_weekend = contact_covid_infect_weekend %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    
    temp_no_covid_infect = contact_no_covid_infect %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_no_covid_infect_week = contact_no_covid_infect_week %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_no_covid_infect_weekend = contact_no_covid_infect_weekend %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    
    temp_covid_vaccine  = contact_covid_vaccine %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_covid_vaccine_week = contact_covid_vaccine_week %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_covid_vaccine_weekend = contact_covid_vaccine_weekend %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    
    temp_no_covid_vaccine = contact_no_covid_vaccine %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_no_covid_vaccine_week = contact_no_covid_vaccine_week %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    temp_no_covid_vaccine_weekend = contact_no_covid_vaccine_weekend %>% filter(SQ2>=age[i] & SQ2<age[i+1])
    
  }else{
    temp = contact_data %>% filter(SQ2>=age[10])
    temp_week = contact_week %>% filter(SQ2>=age[10])
    temp_weekend = contact_weekend %>% filter(SQ2>=age[10])
    
    temp_mask_low = contact_mask_low %>% filter(SQ2>=age[10])
    temp_mask_low_week = contact_mask_low_week %>% filter(SQ2>=age[10])
    temp_mask_low_weekend = contact_mask_low_weekend %>% filter(SQ2>=age[10])
    
    temp_mask_high = contact_mask_high %>% filter(SQ2>=age[10])
    temp_mask_high_week = contact_mask_high_week %>% filter(SQ2>=age[10])
    temp_mask_high_weekend = contact_mask_high_weekend %>% filter(SQ2>=age[10])
    
    temp_vaccine = contact_vaccine %>% filter(SQ2>=age[10])
    temp_vaccine_week = contact_vaccine_week %>% filter(SQ2>=age[10])
    temp_vaccine_weekend = contact_vaccine_weekend %>% filter(SQ2>=age[10])
    
    temp_no_vaccine = contact_no_vaccine %>% filter(SQ2>=age[10])
    temp_no_vaccine_week = contact_no_vaccine_week %>% filter(SQ2>=age[10])
    temp_no_vaccine_weekend = contact_no_vaccine_weekend %>% filter(SQ2>=age[10])
    
    temp_covid_infect = contact_covid_infect %>% filter(SQ2>=age[10])
    temp_covid_infect_week = contact_covid_infect_week %>% filter(SQ2>=age[10])
    temp_covid_infect_weekend = contact_covid_infect_weekend %>% filter(SQ2>=age[10])
    
    temp_no_covid_infect = contact_no_covid_infect %>% filter(SQ2>=age[10])
    temp_no_covid_infect_week = contact_no_covid_infect_week %>% filter(SQ2>=age[10])
    temp_no_covid_infect_weekend = contact_no_covid_infect_weekend %>% filter(SQ2>=age[10])
    
    temp_covid_vaccine = contact_covid_vaccine %>% filter(SQ2>=age[10])
    temp_covid_vaccine_week = contact_covid_vaccine_week %>% filter(SQ2>=age[10])
    temp_covid_vaccine_weekend = contact_covid_vaccine_weekend %>% filter(SQ2>=age[10])
    
    temp_no_covid_vaccine = contact_no_covid_vaccine %>% filter(SQ2>=age[10])
    temp_no_covid_vaccine_week = contact_no_covid_vaccine_week %>% filter(SQ2>=age[10])
    temp_no_covid_vaccine_weekend = contact_no_covid_vaccine_weekend %>% filter(SQ2>=age[10])
  }
  for(j in 1:10){
    
    if(j !=10){
      phi[i,j] = sum(sapply(1:nrow(temp),function(x) length(which(temp[x,4:53]==j))))/nrow(temp)
      phi_week[i,j] = sum(sapply(1:nrow(temp_week),function(x) length(which(temp_week[x,4:53]==j))))/nrow(temp_week)
      phi_weekend[i,j] = sum(sapply(1:nrow(temp_weekend),function(x) length(which(temp_weekend[x,4:53]==j))))/nrow(temp_weekend)
      
      phi_mask_low[i,j] = sum(sapply(1:nrow(temp_mask_low),function(x) length(which(temp_mask_low[x,4:53]==j))))/nrow(temp_mask_low)
      phi_mask_low_week[i,j] = sum(sapply(1:nrow(temp_mask_low_week),function(x) length(which(temp_mask_low_week[x,4:53]==j))))/nrow(temp_mask_low_week)
      phi_mask_low_weekend[i,j] = sum(sapply(1:nrow(temp_mask_low_weekend),function(x) length(which(temp_mask_low_weekend[x,4:53]==j))))/nrow(temp_mask_low_weekend)
      
      phi_mask_high[i,j] = sum(sapply(1:nrow(temp_mask_high),function(x) length(which(temp_mask_high[x,4:53]==j))))/nrow(temp_mask_high)
      phi_mask_high_week[i,j] = sum(sapply(1:nrow(temp_mask_high_week),function(x) length(which(temp_mask_high_week[x,4:53]==j))))/nrow(temp_mask_high_week)
      phi_mask_high_weekend[i,j] = sum(sapply(1:nrow(temp_mask_high_weekend),function(x) length(which(temp_mask_high_weekend[x,4:53]==j))))/nrow(temp_mask_high_weekend)
      
      phi_flu_vaccine[i,j] = sum(sapply(1:nrow(temp_vaccine),function(x) length(which(temp_vaccine[x,4:53]==j))))/nrow(temp_vaccine)
      phi_flu_vaccine_week[i,j] = sum(sapply(1:nrow(temp_vaccine_week),function(x) length(which(temp_vaccine_week[x,4:53]==j))))/nrow(temp_vaccine_week)
      phi_flu_vaccine_weekend[i,j] = sum(sapply(1:nrow(temp_vaccine_weekend),function(x) length(which(temp_vaccine_weekend[x,4:53]==j))))/nrow(temp_vaccine_weekend)
      
      phi_no_flu_vaccine[i,j] = sum(sapply(1:nrow(temp_no_vaccine),function(x) length(which(temp_no_vaccine[x,4:53]==j))))/nrow(temp_no_vaccine)
      phi_no_flu_vaccine_week[i,j] = sum(sapply(1:nrow(temp_no_vaccine_week),function(x) length(which(temp_no_vaccine_week[x,4:53]==j))))/nrow(temp_no_vaccine_week)
      phi_no_flu_vaccine_weekend[i,j] = sum(sapply(1:nrow(temp_no_vaccine_weekend),function(x) length(which(temp_no_vaccine_weekend[x,4:53]==j))))/nrow(temp_no_vaccine_weekend)
      
      phi_covid_infect[i,j] = sum(sapply(1:nrow(temp_covid_infect),function(x) length(which(temp_covid_infect[x,4:53]==j))))/nrow(temp_covid_infect)
      phi_covid_infect_week[i,j] = sum(sapply(1:nrow(temp_covid_infect_week),function(x) length(which(temp_covid_infect_week[x,4:53]==j))))/nrow(temp_covid_infect_week)
      phi_covid_infect_weekend[i,j] = sum(sapply(1:nrow(temp_covid_infect_weekend),function(x) length(which(temp_covid_infect_weekend[x,4:53]==j))))/nrow(temp_covid_infect_weekend)
      
      phi_no_covid_infect[i,j] = sum(sapply(1:nrow(temp_no_covid_infect),function(x) length(which(temp_no_covid_infect[x,4:53]==j))))/nrow(temp_no_covid_infect)
      phi_no_covid_infect_week[i,j] = sum(sapply(1:nrow(temp_no_covid_infect_week),function(x) length(which(temp_no_covid_infect_week[x,4:53]==j))))/nrow(temp_no_covid_infect_week)
      phi_no_covid_infect_weekend[i,j] = sum(sapply(1:nrow(temp_no_covid_infect_weekend),function(x) length(which(temp_no_covid_infect_weekend[x,4:53]==j))))/nrow(temp_no_covid_infect_weekend)
      
      phi_covid_vaccine[i,j] = sum(sapply(1:nrow(temp_covid_vaccine),function(x) length(which(temp_covid_vaccine[x,4:53]==j))))/nrow(temp_covid_vaccine)
      phi_covid_vaccine_week[i,j] = sum(sapply(1:nrow(temp_covid_vaccine_week),function(x) length(which(temp_covid_vaccine_week[x,4:53]==j))))/nrow(temp_covid_vaccine_week)
      phi_covid_vaccine_weekend[i,j] = sum(sapply(1:nrow(temp_covid_vaccine_weekend),function(x) length(which(temp_covid_vaccine_weekend[x,4:53]==j))))/nrow(temp_covid_vaccine_weekend)
      
      phi_no_covid_vaccine[i,j] = sum(sapply(1:nrow(temp_no_covid_vaccine),function(x) length(which(temp_no_covid_vaccine[x,4:53]==j))))/nrow(temp_no_covid_vaccine)
      phi_no_covid_vaccine_week[i,j] = sum(sapply(1:nrow(temp_no_covid_vaccine_week),function(x) length(which(temp_no_covid_vaccine_week[x,4:53]==j))))/nrow(temp_no_covid_vaccine_week)
      phi_no_covid_vaccine_weekend[i,j] = sum(sapply(1:nrow(temp_no_covid_vaccine_weekend),function(x) length(which(temp_no_covid_vaccine_weekend[x,4:53]==j))))/nrow(temp_no_covid_vaccine_weekend)
      
    }else{
      phi[i,j] = sum(sapply(1:nrow(temp),function(x) length(which(temp[x,4:53]>=j))))/nrow(temp)
      phi_week[i,j] = sum(sapply(1:nrow(temp_week),function(x) length(which(temp_week[x,4:53]>=j))))/nrow(temp_week)
      phi_weekend[i,j] = sum(sapply(1:nrow(temp_weekend),function(x) length(which(temp_weekend[x,4:53]>=j))))/nrow(temp_weekend)
      
      phi_mask_low[i,j] = sum(sapply(1:nrow(temp_mask_low),function(x) length(which(temp_mask_low[x,4:53]>=j))))/nrow(temp_mask_low)
      phi_mask_low_week[i,j] = sum(sapply(1:nrow(temp_mask_low_week),function(x) length(which(temp_mask_low_week[x,4:53]>=j))))/nrow(temp_mask_low_week)
      phi_mask_low_weekend[i,j] = sum(sapply(1:nrow(temp_mask_low_weekend),function(x) length(which(temp_mask_low_weekend[x,4:53]>=j))))/nrow(temp_mask_low_weekend)
      
      phi_mask_high[i,j] = sum(sapply(1:nrow(temp_mask_high),function(x) length(which(temp_mask_high[x,4:53]>=j))))/nrow(temp_mask_high)
      phi_mask_high_week[i,j] = sum(sapply(1:nrow(temp_mask_high_week),function(x) length(which(temp_mask_high_week[x,4:53]>=j))))/nrow(temp_mask_high_week)
      phi_mask_high_weekend[i,j] = sum(sapply(1:nrow(temp_mask_high_weekend),function(x) length(which(temp_mask_high_weekend[x,4:53]>=j))))/nrow(temp_mask_high_weekend)
      
      phi_flu_vaccine[i,j] = sum(sapply(1:nrow(temp_vaccine),function(x) length(which(temp_vaccine[x,4:53]>=j))))/nrow(temp_vaccine)
      phi_flu_vaccine_week[i,j] = sum(sapply(1:nrow(temp_vaccine_week),function(x) length(which(temp_vaccine_week[x,4:53]>=j))))/nrow(temp_vaccine_week)
      phi_flu_vaccine_weekend[i,j] = sum(sapply(1:nrow(temp_vaccine_weekend),function(x) length(which(temp_vaccine_weekend[x,4:53]>=j))))/nrow(temp_vaccine_weekend)
      
      phi_no_flu_vaccine[i,j] = sum(sapply(1:nrow(temp_no_vaccine),function(x) length(which(temp_no_vaccine[x,4:53]>=j))))/nrow(temp_no_vaccine)
      phi_no_flu_vaccine_week[i,j] = sum(sapply(1:nrow(temp_no_vaccine_week),function(x) length(which(temp_no_vaccine_week[x,4:53]>=j))))/nrow(temp_no_vaccine_week)
      phi_no_flu_vaccine_weekend[i,j] = sum(sapply(1:nrow(temp_no_vaccine_weekend),function(x) length(which(temp_no_vaccine_weekend[x,4:53]>=j))))/nrow(temp_no_vaccine_weekend)
      
      phi_covid_infect[i,j] = sum(sapply(1:nrow(temp_covid_infect),function(x) length(which(temp_covid_infect[x,4:53]>=j))))/nrow(temp_covid_infect)
      phi_covid_infect_week[i,j] = sum(sapply(1:nrow(temp_covid_infect_week),function(x) length(which(temp_covid_infect_week[x,4:53]>=j))))/nrow(temp_covid_infect_week)
      phi_covid_infect_weekend[i,j] = sum(sapply(1:nrow(temp_covid_infect_weekend),function(x) length(which(temp_covid_infect_weekend[x,4:53]>=j))))/nrow(temp_covid_infect_weekend)
      
      phi_no_covid_infect[i,j] = sum(sapply(1:nrow(temp_no_covid_infect),function(x) length(which(temp_no_covid_infect[x,4:53]>=j))))/nrow(temp_no_covid_infect)
      phi_no_covid_infect_week[i,j] = sum(sapply(1:nrow(temp_no_covid_infect_week),function(x) length(which(temp_no_covid_infect_week[x,4:53]>=j))))/nrow(temp_no_covid_infect_week)
      phi_no_covid_infect_weekend[i,j] = sum(sapply(1:nrow(temp_no_covid_infect_weekend),function(x) length(which(temp_no_covid_infect_weekend[x,4:53]>=j))))/nrow(temp_no_covid_infect_weekend)
      
      phi_covid_vaccine[i,j] = sum(sapply(1:nrow(temp_covid_vaccine),function(x) length(which(temp_covid_vaccine[x,4:53]>=j))))/nrow(temp_covid_vaccine)
      phi_covid_vaccine_week[i,j] = sum(sapply(1:nrow(temp_covid_vaccine_week),function(x) length(which(temp_covid_vaccine_week[x,4:53]>=j))))/nrow(temp_covid_vaccine_week)
      phi_covid_vaccine_weekend[i,j] = sum(sapply(1:nrow(temp_covid_vaccine_weekend),function(x) length(which(temp_covid_vaccine_weekend[x,4:53]>=j))))/nrow(temp_covid_vaccine_weekend)
      
      phi_no_covid_vaccine[i,j] = sum(sapply(1:nrow(temp_no_covid_vaccine),function(x) length(which(temp_no_covid_vaccine[x,4:53]>=j))))/nrow(temp_no_covid_vaccine)
      phi_no_covid_vaccine_week[i,j] = sum(sapply(1:nrow(temp_no_covid_vaccine_week),function(x) length(which(temp_no_covid_vaccine_week[x,4:53]>=j))))/nrow(temp_no_covid_vaccine_week)
      phi_no_covid_vaccine_weekend[i,j] = sum(sapply(1:nrow(temp_no_covid_vaccine_weekend),function(x) length(which(temp_no_covid_vaccine_weekend[x,4:53]>=j))))/nrow(temp_no_covid_vaccine_weekend)
      
    }
  }
}

########################################################################################################
# Sebastian contact matrix

#1. Weighted average of weekday contact and weekend contact
phi_res = (5/7) * phi_week + (2/7) * phi_weekend
phi_mask_low_res = (5/7) * phi_mask_low_week + (2/7) * phi_mask_low_weekend
phi_mask_high_res = (5/7) * phi_mask_high_week + (2/7) * phi_mask_high_weekend
phi_flu_vaccine_res = (5/7) * phi_flu_vaccine_week + (2/7) * phi_flu_vaccine_weekend
phi_no_flu_vaccine_res = (5/7) * phi_no_flu_vaccine_week + (2/7) * phi_no_flu_vaccine_weekend
phi_covid_infect_res = (5/7) * phi_covid_infect_week + (2/7) * phi_covid_infect_weekend
phi_no_covid_infect_res = (5/7) * phi_no_covid_infect_week + (2/7) * phi_no_covid_infect_weekend
phi_covid_vaccine_res = (5/7) * phi_covid_vaccine_week + (2/7) * phi_covid_vaccine_weekend
phi_no_covid_vaccine_res = (5/7) * phi_no_covid_vaccine_week + (2/7) * phi_no_covid_vaccine_weekend

#2. Load Korea population structure
n= rep(0,10)
setwd('C:/project/Social-contact-pattern-in-South-Korea-a-KoMix-study/data')
skage = read.csv('skage.csv',header=T)
skage = skage[1:101,5]
skage = as.numeric(sapply(skage, function(x) gsub(',','',x)))
sum(skage[1:3])
for(i in 1:9){
  n[i] = sum(skage[(age[i]+1):(age[(i+1)])])
}
n[10] = sum(skage[61:101])


#3. Calculating the symmetric component by reflecting the population structure.
phi_res_sb = matrix(0,nrow=10,ncol=10)
phi_res_sb_mask_low = matrix(0,nrow=10,ncol=10)
phi_res_sb_mask_high = matrix(0,nrow=10,ncol=10)
phi_res_sb_flu_vaccine = matrix(0,nrow=10,ncol=10)
phi_res_sb_no_flu_vaccine = matrix(0,nrow=10,ncol=10)
phi_res_sb_covid_infect = matrix(0,nrow=10,ncol=10)
phi_res_sb_no_covid_infect = matrix(0,nrow=10,ncol=10)
phi_res_sb_covid_vaccine = matrix(0,nrow=10,ncol=10)
phi_res_sb_no_covid_vaccine = matrix(0,nrow=10,ncol=10)

for(i in 1:10){
  for(j in 1:10){
    phi_res_sb[i,j] = (phi_res[i,j]*n[i] + phi_res[j,i]*n[j])/(2*n[i])
    phi_res_sb_mask_low[i,j] = (phi_mask_low_res[i,j]*n[i] + phi_mask_low_res[j,i]*n[j])/(2*n[i])
    phi_res_sb_mask_high[i,j] = (phi_mask_high_res[i,j]*n[i] + phi_mask_high_res[j,i]*n[j])/(2*n[i])
    phi_res_sb_flu_vaccine[i,j] = (phi_flu_vaccine_res[i,j]*n[i] + phi_flu_vaccine_res[j,i]*n[j])/(2*n[i])
    phi_res_sb_no_flu_vaccine[i,j] = (phi_no_flu_vaccine_res[i,j]*n[i] + phi_no_flu_vaccine_res[j,i]*n[j])/(2*n[i])
    phi_res_sb_covid_infect[i,j] = (phi_covid_infect_res[i,j]*n[i] + phi_covid_infect_res[j,i]*n[j])/(2*n[i])
    phi_res_sb_no_covid_infect[i,j] = (phi_no_covid_infect_res[i,j]*n[i] + phi_no_covid_infect_res[j,i]*n[j])/(2*n[i])
    phi_res_sb_covid_vaccine[i,j] = (phi_covid_vaccine_res[i,j]*n[i] + phi_covid_vaccine_res[j,i]*n[j])/(2*n[i])
    phi_res_sb_no_covid_vaccine[i,j] = (phi_no_covid_vaccine_res[i,j]*n[i] + phi_no_covid_vaccine_res[j,i]*n[j])/(2*n[i])
    
    
  }
}
########################################################################################################
########################################################################################################
name = c('[0,3)','[3,7)','[7,13)','[13,16)','[16,19)','[19,30)','[30,40)','[40,50)','[50,60)','[60,+)')

phi = as.data.frame(phi)
rownames(phi) = name
colnames(phi) = name

phi_mask_low = as.data.frame(phi_mask_low)
rownames(phi_mask_low) = name
colnames(phi_mask_low) = name

phi_mask_high = as.data.frame(phi_mask_high)
rownames(phi_mask_high) = name
colnames(phi_mask_high) = name

phi_flu_vaccine = as.data.frame(phi_flu_vaccine)
rownames(phi_flu_vaccine) = name
colnames(phi_flu_vaccine) = name

phi_no_flu_vaccine = as.data.frame(phi_no_flu_vaccine)
rownames(phi_no_flu_vaccine) = name
colnames(phi_no_flu_vaccine) = name

phi_covid_infect = as.data.frame(phi_covid_infect)
rownames(phi_covid_infect) = name
colnames(phi_covid_infect) = name

phi_no_covid_infect = as.data.frame(phi_no_covid_infect)
rownames(phi_no_covid_infect) = name
colnames(phi_no_covid_infect) = name

phi_covid_vaccine = as.data.frame(phi_covid_vaccine)
rownames(phi_covid_vaccine) = name
colnames(phi_covid_vaccine) = name

phi_no_covid_vaccine = as.data.frame(phi_no_covid_vaccine)
rownames(phi_no_covid_vaccine) = name
colnames(phi_no_covid_vaccine) = name


phi_res_sb = as.data.frame(phi_res_sb)
rownames(phi_res_sb) = name
colnames(phi_res_sb) = name

phi_res_sb_mask_low = as.data.frame(phi_res_sb_mask_low)
rownames(phi_res_sb_mask_low) = name
colnames(phi_res_sb_mask_low) = name

phi_res_sb_mask_high = as.data.frame(phi_res_sb_mask_high)
rownames(phi_res_sb_mask_high) = name
colnames(phi_res_sb_mask_high) = name

phi_res_sb_flu_vaccine = as.data.frame(phi_res_sb_flu_vaccine)
rownames(phi_res_sb_flu_vaccine) = name
colnames(phi_res_sb_flu_vaccine) = name

phi_res_sb_no_flu_vaccine = as.data.frame(phi_res_sb_no_flu_vaccine)
rownames(phi_res_sb_no_flu_vaccine) = name
colnames(phi_res_sb_no_flu_vaccine) = name

phi_res_sb_covid_infect = as.data.frame(phi_res_sb_covid_infect)
rownames(phi_res_sb_covid_infect) = name
colnames(phi_res_sb_covid_infect) = name

phi_res_sb_no_covid_infect = as.data.frame(phi_res_sb_no_covid_infect)
rownames(phi_res_sb_no_covid_infect) = name
colnames(phi_res_sb_no_covid_infect) = name

phi_res_sb_covid_vaccine = as.data.frame(phi_res_sb_covid_vaccine)
rownames(phi_res_sb_covid_vaccine) = name
colnames(phi_res_sb_covid_vaccine) = name

phi_res_sb_no_covid_vaccine = as.data.frame(phi_res_sb_no_covid_vaccine)
rownames(phi_res_sb_no_covid_vaccine) = name
colnames(phi_res_sb_no_covid_vaccine) = name

setwd('C:/project/Social-contact-pattern-in-South-Korea-a-KoMix-study')


write.csv(phi_res_sb,'./contact matrix/contact_2311_Sebastian.csv')
write.csv(phi_res_sb_mask_low,'./contact matrix/contact_2311_Sebastian_mask_low.csv')
write.csv(phi_res_sb_mask_high,'./contact matrix/contact_2311_Sebastian_mask_high.csv')
write.csv(phi_res_sb_flu_vaccine,'./contact matrix/contact_2311_Sebastian_flu_vaccine.csv')
write.csv(phi_res_sb_no_flu_vaccine,'./contact matrix/contact_2311_Sebastian_no_flu_vaccine.csv')
write.csv(phi_res_sb_covid_infect,'./contact matrix/contact_2311_Sebastian_covid_infect.csv')
write.csv(phi_res_sb_no_covid_infect,'./contact matrix/contact_2311_Sebastian_no_covid_infect.csv')
write.csv(phi_res_sb_covid_vaccine,'./contact matrix/contact_2311_Sebastian_covid_vaccine.csv')
write.csv(phi_res_sb_no_covid_vaccine,'./contact matrix/contact_2311_Sebastian_no_covid_vaccine.csv')


