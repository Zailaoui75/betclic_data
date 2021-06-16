#install.packages("DBI")
#install.packages("RMySQL")
#install.packages("RMariaDB")
#install.packages("RSQLite")

library("DBI")
library("RMySQL")
library("RMariaDB")
library("RSQLite")

new_date= function(date, months_added)
{
  new_year= as.character(    as.numeric(substr(date, 1, 4)) +  trunc( (as.numeric(substr(date, 6, 7)) + months_added -1) / 12) )
  new_m= (as.numeric(substr(date, 6, 7))+  months_added %% 12) %% 12
  new_m
  if(new_m==0){ new_m=12}
  
  new_month= ifelse( new_m < 10 , 
                     paste("0", new_m, sep=""),
                     new_m )
  new_date=paste(new_year, new_month  , sep="-")
  new_date
  return(new_date)
}

new_date("2020-09", 3)
new_date("2020-11",13)

new_date("2020-11",1)
new_date("2020-11",2)
new_date("2020-11",3)

new_date("2018-07",18)
new_date("2018-07",19)

####################################
#  Simulation de la table clients  #
####################################

date_min='2019-01-01'
date_max='2021-06-01'

nb_clients_simul=10000
acquisition_source= sample(c("Mobile","Organic","Affiliation","SEO","SEA"), size=nb_clients_simul, prob=c(0.35,0.30,0.15,0.10,0.10), replace=TRUE)
registration_date=sample( substr(seq(as.Date(date_min), as.Date(date_max) , by="month" ),1,7) , nb_clients_simul , replace=TRUE)
behaviour_segment=sample(c("A","B","C","D"), size=nb_clients_simul, prob=c(0.25,0.25,0.25,0.25), replace=TRUE)

# Creation dataframe user

user=data.frame(matrix(0,nb_clients_simul,8 , 9))
colnames(user)=c("Registration_MonthDate","Age","Acquisition_Source","User_id","Behaviour_Segment","Cash_Balance","Sexe","Age_Category")
#View(user)
for(i in 1:nrow(user)){
  user[i,"Registration_MonthDate"]=registration_date[i]
  user[i,"Registration_MonthDate"]=registration_date[i]
  user[i,"Acquisition_Source"]=acquisition_source[i]
  user[i,"Behaviour_Segment"]=behaviour_segment[i]
  user[i,"User_id"]=i
  if(acquisition_source[i] == "Mobile" ){
    user[i,"Age"]=max(18, rpois(1,20))
    user[i,"Cash_Balance"]=abs(round(rnorm(1)*100,2))
    user[i,"Sexe"]=ifelse(rbinom(1, 1, 0.2)==1, "W","M")
  }else if(acquisition_source[i] == "Organic" ){
    user[i,"Age"]=max(18, rpois(1,25))
    user[i,"Cash_Balance"]=abs(round(rnorm(1)*80,2))
    user[i,"Sexe"]=ifelse(rbinom(1, 1, 0.2)==1, "W","M")
  }else if(acquisition_source[i] == "SEO" ){
    user[i,"Age"]=max(18, rpois(1,25))
    user[i,"Cash_Balance"]=abs(round(rnorm(1)*80,2))
    user[i,"Sexe"]=ifelse(rbinom(1, 1, 0.2)==1, "W","M")
  }
  else if(acquisition_source[i] == "Affiliation" ){
    user[i,"Age"]=max(18, rpois(1,20))
    user[i,"Cash_Balance"]=abs(round(rnorm(1)*80,2))
    user[i,"Sexe"]=ifelse(rbinom(1, 1, 0.2)==1, "W","M")
  }else if(acquisition_source[i] == "SEA" ){
    user[i,"Age"]=max(18, rpois(1,28))
    user[i,"Cash_Balance"]=abs(round(rnorm(1)*50,2))
    user[i,"Sexe"]=ifelse(rbinom(1, 1, 0.2)==1, "W","M")
  }
  # Age_Category
  if( user[i,"Age"] >= 18 & user[i,"Age"] <= 20 ){
    user[i,"Age_Category"] ="18-20"
  }else if( user[i,"Age"] >= 21 & user[i,"Age"] <=  25 )  {
    user[i,"Age_Category"] ="21-25"
  }else if( user[i,"Age"] >= 26 & user[i,"Age"] <=  29 )  {
    user[i,"Age_Category"] ="26-29"
  }else if( user[i,"Age"] >= 30 & user[i,"Age"] <=  39 )  {
    user[i,"Age_Category"] ="30-39"
  }else if( user[i,"Age"] >= 40  )  {
    user[i,"Age_Category"] =">40"
  }
}
View(user)

#######################################
#  On construit les données de jeu    #
#######################################

# Creation dataframe monthly_user_revenue
monthly_user_revenue=data.frame(matrix(NA, 0 ,7))
colnames(monthly_user_revenue)=c("MonthDate","User_id","Revenue","activity_duration","Amount_bets","compt","Inactivity")
View(monthly_user_revenue)

for(i in 1:nrow(user))
{
  
  indiv_user_revenue=data.frame(matrix(NA, 0 ,7))
  colnames(indiv_user_revenue)=c("MonthDate","User_id","Revenue","activity_duration","Amount_bets","compt","Inactivity")
  
  if(i %% 50 == 0){print(i)}
  activity_duration=rpois(1,20)
  user_id_revenue=data.frame(matrix(NA, 1 ,7))
  colnames(user_id_revenue)=c("MonthDate","User_id","Revenue","activity_duration","Amount_bets","compt","Inactivity")
  j=1
  inactivity=1
  while(new_date(user[i,"Registration_MonthDate"], j-1) <= substr(date_max,1,7) ) {
    
    if(j <= activity_duration ){
      user_id_revenue[1,"User_id"]=i
      user_id_revenue[1,"activity_duration"]=activity_duration
      user_id_revenue[1,"MonthDate"]=new_date(user[i,"Registration_MonthDate"], j-1)
      user_id_revenue[1,"Amount_bets"]=abs(round(rnorm(1)*300,2))
      user_id_revenue[1,"compt"]=j
      user_id_revenue[1,"Revenue"]=min(user_id_revenue[1,"Amount_bets"], user_id_revenue[1,"Amount_bets"] - 0.9 * round(rnorm(1)*100))
      user_id_revenue[1,"Inactivity"]=0
    }else{
      
      user_id_revenue[1,"User_id"]=i
      user_id_revenue[1,"activity_duration"]=activity_duration
      user_id_revenue[1,"MonthDate"]=new_date(user[i,"Registration_MonthDate"], j-1)
      user_id_revenue[1,"Amount_bets"]=0
      user_id_revenue[1,"compt"]=j
      user_id_revenue[1,"Revenue"]=0
      user_id_revenue[1,"Inactivity"]=inactivity
      inactivity=inactivity+1
    }
    indiv_user_revenue=rbind(indiv_user_revenue, user_id_revenue)
    j=j+1
  }
  monthly_user_revenue=rbind(monthly_user_revenue, indiv_user_revenue)
  
  # On additionne 2 dates en R des mois
  if(user[i,"Acquisition_Source"]  == "Mobile" ){
    
  }else if(user[i,"Acquisition_Source"] == "Organic" ){
    
  }else if(user[i,"Acquisition_Source"]  == "SEO" ){
    
  }else if(user[i,"Acquisition_Source"]  == "Affiliation" ){
    
  }else if(user[i,"Acquisition_Source"]  == "SEA" ){
    
  }
}
View(monthly_user_revenue)
nrow(monthly_user_revenue)

Inactivity_zoom=monthly_user_revenue[monthly_user_revenue$MonthDate=="2021-06",c("User_id","Inactivity")]
user_with_inactivity=merge(user, Inactivity_zoom, by="User_id")
user_with_inactivity[is.na(user_with_inactivity$Inactivity),]=0
user_with_inactivity$Inactivity_Category=""
user_with_inactivity[user_with_inactivity$Inactivity == 0,]$Inactivity_Category="Active"
user_with_inactivity[user_with_inactivity$Inactivity == 1,]$Inactivity_Category="Inactive_1M"
user_with_inactivity[user_with_inactivity$Inactivity == 2,]$Inactivity_Category="Inactive_2M"
user_with_inactivity[user_with_inactivity$Inactivity >= 3 & user_with_inactivity$Inactivity <= 5 ,]$Inactivity_Category="Inactive_3M_5M"
user_with_inactivity[user_with_inactivity$Inactivity >= 6 & user_with_inactivity$Inactivity <= 11 ,]$Inactivity_Category="Inactive_6M_11M"
user_with_inactivity[user_with_inactivity$Inactivity >= 12 ,]$Inactivity_Category="Inactive_12M"
user_with_inactivity[user_with_inactivity$Inactivity_Category != "Active" , ]$Cash_Balance=0

user_with_inactivity$Cash_Balance_Category=""
for (i in 1:nrow(user_with_inactivity))
{
  if( user_with_inactivity[i,"Cash_Balance"]==0){
    user_with_inactivity[i,"Cash_Balance_Category"]="CB=0"
  }else if ( user_with_inactivity[i,"Cash_Balance"] > 0 & user_with_inactivity[i,"Cash_Balance"] < 10 )
  {
    user_with_inactivity[i,"Cash_Balance_Category"]="0<CB<10"
  }
  else if ( user_with_inactivity[i,"Cash_Balance"] >= 10 & user_with_inactivity[i,"Cash_Balance"] < 20 )
  {
    user_with_inactivity[i,"Cash_Balance_Category"]="10<=CB<20"
  }
  else if ( user_with_inactivity[i,"Cash_Balance"] >= 20 & user_with_inactivity[i,"Cash_Balance"] < 50 )
  {
    user_with_inactivity[i,"Cash_Balance_Category"]="20<=CB<50"
  } else if ( user_with_inactivity[i,"Cash_Balance"] >=50 & user_with_inactivity[i,"Cash_Balance"] < 100 )
  {
    user_with_inactivity[i,"Cash_Balance_Category"]="50<=CB<100"
  } else if ( user_with_inactivity[i,"Cash_Balance"] >= 100 )
  {
    user_with_inactivity[i,"Cash_Balance_Category"]="100<=CB"
  }
}
View(user_with_inactivity)

###########################################
#  On construit les données de dépenses   #
###########################################

dates=substr(   seq(as.Date(date_min), as.Date(date_max) , by="month" ),1,7)
acquisition_sources=c("Mobile","Organic","SEO","Affiliation","SEA")

monthly_acquisition_cost=data.frame(matrix(NA,  0, 3))
colnames( monthly_acquisition_cost )=c('MonthDate','Acquisition_Source','Cost')
View( monthly_acquisition_cost )

for(i in 1:length(dates)){
  for (j in 1:length(acquisition_sources))
  {
    new_line=data.frame(matrix(NA,  1, 3))
    colnames( new_line )=c('MonthDate','Acquisition_Source','Cost')
    new_line[1,"MonthDate"]=dates[i]
    new_line[1,"Acquisition_Source"]=acquisition_sources[j]
    if(acquisition_sources[j]=="SEA" || acquisition_sources[j]=="Affiliation" )
    {
      new_line[1,"Cost"]=abs(rnorm(1,60000,20000))
    }else{
      new_line[1,"Cost"]=0
    }
    monthly_acquisition_cost= rbind(monthly_acquisition_cost,new_line)
  }
}
View( monthly_acquisition_cost )
#conn <- dbConnect(RSQLite::SQLite(), "/Users/atalaya/Desktop/betclic_pablo_atalaya/appli_shiny/betclic.db")
#dbWriteTable(conn, "monthly_user_revenue", monthly_user_revenue)
#dbWriteTable(conn, "user", user_with_inactivity)
#dbWriteTable(conn, "monthly_acquisition_cost", monthly_acquisition_cost)
#dbListTables(conn)