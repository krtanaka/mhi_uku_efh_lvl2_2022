#this will turn .MOC files into .csvs with appropriately named columns, 
#and conduct quality control,
#and filter CTD data down to most necessary columns
library(tidyverse)
library(lubridate)
#For TC8604########
setwd("~/M&B_larval_dist_1996/MOCNESS Data/TC-86-04")
files<-list.files(path=".", pattern = "*.MOC")
#files<-list.files(path="~/M&B_larval_dist_1996/MOCNESS Data/test", pattern = "*.MOC") #test
is.wholenumber<-function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
moc_read_8604 = function(input) {
  d<-read.table(input)
  index=which(files==input)
  d<-d %>%filter(str_detect(string=d$V1,negate=T,pattern="\"") & str_detect(string=d$V1,negate=T,pattern="^[[:upper:]]")) 
  discrep<-ifelse(is.wholenumber((nrow(d)/10)-3), 0,((nrow(d)/10-3)%%1)*10) 
  d<-d %>% head(d,n=floor(-discrep))
  V1<-matrix(d[1,], nrow=(nrow(d)/10), ncol = 1)
  V2<-matrix(d[2,], nrow=(nrow(d)/10), ncol = 1)
  V3<-matrix(d[3,], nrow=(nrow(d)/10), ncol = 1)
  num_offset<-10
  V4<-matrix(d[seq(from=4, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V5<-matrix(d[seq(from=5, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V6<-matrix(d[seq(from=6, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V7<-matrix(d[seq(from=7, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1) 
  V8<-matrix(d[seq(from=8, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V9<-matrix(d[seq(from=9, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V10<-matrix(d[seq(from=10, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V11<-matrix(d[seq(from=11, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V12<-matrix(d[seq(from=12, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V13<-matrix(d[seq(from=13, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  max.len = max(length(V1), length(V2),length(V3),length(V4),length(V5),length(V6),length(V7),
                length(V8),length(V9),length(V10),length(V11),length(V12),length(V13))#set max length based on longest vector here
  #pad nas to fill space discrepancies between shortest and longest
  V1=c(V1,rep(NA, max.len-length(V1)))
  V2=c(V2,rep(NA, max.len-length(V2)))
  V3=c(V3,rep(NA, max.len-length(V3)))
  V4=c(V4,rep(NA, max.len-length(V4)))
  V5=c(V5,rep(NA, max.len-length(V5)))
  V6=c(V6,rep(NA, max.len-length(V6)))
  V7=c(V7,rep(NA, max.len-length(V7)))
  V8=c(V8,rep(NA, max.len-length(V8)))
  V9=c(V9,rep(NA, max.len-length(V9)))
  V10=c(V10,rep(NA, max.len-length(V10)))
  V11=c(V11,rep(NA, max.len-length(V11)))
  V12=c(V12,rep(NA, max.len-length(V12)))
  V13=c(V13,rep(NA, max.len-length(V13)))
  newdf<-as.data.frame(cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13)) #CREATE DATAFRAME WITH CBIND
  newdf[,5:13]<-lapply(c(newdf[,5:13]), as.numeric)
  newdf<-rename(newdf,"cruise"="V1", "date"="V2","station_number"="V3","sal"="V7", "temp_DegC"="V6","depth_m"="V5", "time"="V4", "net_number"="V13", "record_number"="V9","time_bins_in_sec"="V12","net_angle"="V8")
  newdf$date<-mdy(newdf$date)#as.Date(newdf$date, format="%m-%d-%Y")#turns date to YMD format
  newdf$date<-ifelse(str_starts(newdf$time,"00", negate = F),(newdf$date+days(1)),newdf$date)
  newdf$date<-as_date(newdf$date)
  newdf<-unite(newdf, "date_time", c("date","time"), remove = F)
  newdf$date_time<-as.POSIXct(newdf$date_time,format="%Y-%m-%d_%H:%M:%OS")
  newdf<-unite(newdf, "moc_id", c(cruise,station_number,net_number),sep="_", remove=F)
  newdf$moc_id<-gsub("-","",as.character(newdf$moc_id))
  newdf$moc_id<-str_to_upper(newdf$moc_id, locale = "en")
  newdf<-unite(newdf, "moc_id_noNet", c(cruise,station_number),sep="_", remove=F)
  newdf$moc_id_noNet<-gsub("-","",as.character(newdf$moc_id_noNet))
  newdf$moc_id_noNet<-str_to_upper(newdf$moc_id_noNet, locale = "en")
  outname = paste("TC",input, '.csv', sep = "") 
  write.csv(x=newdf, file=outname)
}

for (i in 1:length(files)) {
  moc_read_8604(files[i])
}

#several csvs did not work, ran through the next two lines and then the function by hand
#d<-read.table("~/M&B_larval_dist_1996/MOCNESS data/TC-86-04/8604045.MOC") #fixed via 8602 function
#d<-read.table("~/M&B_larval_dist_1996/MOCNESS data/TC-86-04/8604035.MOC") #fixed 8602 function
#d<-read.table("~/M&B_larval_dist_1996/MOCNESS data/TC-86-04/8604009.MOC") #fixed by hand
#d<-read.table("~/M&B_larval_dist_1996/MOCNESS data/TC-86-04/8604016.MOC") # extra three header rows, deleted these and skipped upper case filter step
#d<-tail(d,-3)
#For TC8602########
setwd("~/M&B_larval_dist_1996/MOCNESS data/TC-86-02/")
files<-list.files(path=".", pattern = "*.MOC")
is.wholenumber<-function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
moc_read_8602 = function(input) {
  d<-read.table(input)
  index=which(files==input)
  d<-d %>%filter(str_detect(string=d$V1,negate=T,pattern="\""))#& str_detect(string=d$V1,negate=T,pattern="^[[:upper:]]")) 
  discrep<-ifelse(is.wholenumber((nrow(d)/10)-3), 0,((nrow(d)/10-3)%%1)*10) 
  d<-d %>% head(d,n=floor(-discrep))
  V1<-matrix(d[1,], nrow=(nrow(d)/10), ncol = 1)
  V2<-matrix(d[2,], nrow=(nrow(d)/10), ncol = 1)
  V3<-matrix(d[3,], nrow=(nrow(d)/10), ncol = 1)
  num_offset<-10
  V4<-matrix(d[seq(from=4, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V5<-matrix(d[seq(from=5, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V6<-matrix(d[seq(from=6, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V7<-matrix(d[seq(from=7, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1) 
  V8<-matrix(d[seq(from=8, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V9<-matrix(d[seq(from=9, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V10<-matrix(d[seq(from=10, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V11<-matrix(d[seq(from=11, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V12<-matrix(d[seq(from=12, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  V13<-matrix(d[seq(from=13, by = num_offset,to=nrow(d)),], nrow=(nrow(d)/10), ncol=1)
  max.len = max(length(V1), length(V2),length(V3),length(V4),length(V5),length(V6),length(V7),
                length(V8),length(V9),length(V10),length(V11),length(V12),length(V13))#set max length based on longest vector here
  #pad nas to fill space discrepancies between shortest and longest
  V1=c(V1,rep(NA, max.len-length(V1)))
  V2=c(V2,rep(NA, max.len-length(V2)))
  V3=c(V3,rep(NA, max.len-length(V3)))
  V4=c(V4,rep(NA, max.len-length(V4)))
  V5=c(V5,rep(NA, max.len-length(V5)))
  V6=c(V6,rep(NA, max.len-length(V6)))
  V7=c(V7,rep(NA, max.len-length(V7)))
  V8=c(V8,rep(NA, max.len-length(V8)))
  V9=c(V9,rep(NA, max.len-length(V9)))
  V10=c(V10,rep(NA, max.len-length(V10)))
  V11=c(V11,rep(NA, max.len-length(V11)))
  V12=c(V12,rep(NA, max.len-length(V12)))
  V13=c(V13,rep(NA, max.len-length(V13)))
  newdf<-as.data.frame(cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13)) #CREATE DATAFRAME WITH CBIND
  newdf[,5:13]<-lapply(c(newdf[,5:13]), as.numeric)
  newdf<-rename(newdf,"cruise"="V1", "date"="V2","station_number"="V3","sal"="V7", "temp_DegC"="V6","depth_m"="V5", "time"="V4", "net_number"="V13", "record_number"="V9","time_bins_in_sec"="V12","net_angle"="V8")
  newdf$date<-mdy(newdf$date)#as.Date(newdf$date, format="%m-%d-%Y")#turns date to YMD format
  newdf$date<-ifelse(str_starts(newdf$time,"00", negate = F),(newdf$date+days(1)),newdf$date)
  newdf$date<-as_date(newdf$date)
  newdf<-unite(newdf, "date_time", c("date","time"), remove = F)
  newdf$date_time<-as.POSIXct(newdf$date_time,format="%Y-%m-%d_%H:%M:%OS")
  newdf$station_number<-str_sub(newdf$station_number,-3, 8)
  newdf<-unite(newdf, "moc_id", c(cruise,station_number,net_number),sep="_", remove=F)
  newdf$moc_id<-gsub("-","",as.character(newdf$moc_id))
  newdf$moc_id<-str_to_upper(newdf$moc_id, locale = "en")
  newdf<-unite(newdf, "moc_id_noNet", c(cruise,station_number),sep="_", remove=F)
  newdf$moc_id_noNet<-gsub("-","",as.character(newdf$moc_id_noNet))
  newdf$moc_id_noNet<-str_to_upper(newdf$moc_id_noNet, locale = "en")
  outname = paste("TC",input, '.csv', sep = "")
  write.csv(x=newdf, file=outname)
}
for (i in 1:length(files)) {
  moc_read_8602(files[i])
}

#one csv did not work, ran through the next two lines and then the function by hand
#d<-read.table("~/M&B_larval_dist_1996/MOCNESS data/TC-86-02/8602002.MOC")
#d<-tail(d,-3)

#QC: impossible values####
is.wholenumber<-function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

moc_qc_impossible_values = function(input) {
  c<-read.csv(input)
  #checks for impossible values
  c<-filter(c,is.wholenumber(net_number))
  c<-filter(c, net_number <= 9)
  c<-filter(c, depth_m>=0) #depths should be positive numbers
  c$station_number<-as.numeric(c$station_number)
  c<-filter(c,is.wholenumber(station_number))
  c$date_time<-as.POSIXct(c$date_time)
  c<-filter(c, "1984-01-01 00:00:00"<date_time & date_time<"1986-12-31 25:59:59") #w2020
  #c<-filter(c, -2.5<temp_DegC & temp_DegC<40.0) #w2020
  c<-filter(c, 2<sal& sal<41) #w2020
  #regional range test #w2020
  #c<-filter(c, (12)<temp_DegC & temp_DegC<26) #w2020, values from HOTS CTD potential temperature 0-1000m, 1989-2019
  c<-filter(c, 34.5<sal& sal<35.4)  #w2020, values from HOTS CTD potential sal 0-1000m, 1989-2019, https://hahana.soest.hawaii.edu/hot/trends/trends.html
  fixed_input<-str_sub(input,1, -9)
  outname = paste(fixed_input, '.csv', sep = "")
  write.csv(x=c, file=outname) # drop last 4 char from input
}
#c<-read.csv("~/M&B_larval_dist_1996/MOCNESS data/TC-86-02/TC8602005.MOC.csv") #individual testing
setwd("~/M&B_larval_dist_1996/MOCNESS Data/TC-86-04")
files<-list.files(path=".", pattern = "*.csv") 

for (i in 1:length(files)) {
  moc_qc_impossible_values(files[i])
}


#used Wong et al. 2020 publication on ARGO float data to help establish qc protocol, ideas from them are credited with #w2020
#https://archimer.ifremer.fr/doc/00228/33951/32470.pdf

#Statistics#####
#c<-read.csv("~/M&B_larval_dist_1996/MOCNESS data/TC-86-02/TC8602031.csv") #to test single file
setwd("~/M&B_larval_dist_1996/MOCNESS Data/TC-86-04")
files<-list.files(path=".", pattern = "*.csv")#test
exclude<-".MOC"
files<-files[str_detect(files, exclude, negate = TRUE)]
#call again before running so that it gets the nice new csvs from the "impossible numbers test"

ci<-0.05 #change this to change confidence interval
moc_qc_stats= function(input) {  
  c<-read.csv(input)
  index=which(files==input)
  netty<-c %>%
    count(net_number) 
  meanie<-c %>%
    group_by(net_number) %>%
    summarise_at(vars("sal", "temp_DegC", "net_angle"),mean) %>%
    rename("sal_mean"="sal", "temp_mean"= "temp_DegC", "angle_mean"="net_angle")
  meanie<-meanie[,-1]
  seedie<-c %>%
    group_by(net_number) %>%
    summarise_at(vars("sal", "temp_DegC", "net_angle"),sd) %>%
    rename("sal_sd"="sal", "temp_sd"= "temp_DegC", "angle_sd"="net_angle")
  seedie <-seedie[,-1]
  meedie<-c %>%
    group_by(net_number) %>%
    summarise_at(vars("sal", "temp_DegC", "net_angle"),median) %>%
    rename("sal_med"="sal", "temp_med"= "temp_DegC", "angle_med"="net_angle")
  meedie<-meedie[,-1]
  statty<-cbind(netty,meanie,seedie,meedie)
  statty %>%
    mutate(across(c(sal_sd,temp_sd,angle_sd))/n)
  statty<-statty %>%
    mutate(sal_ste=sal_sd/n,
           temp_ste=temp_sd/n,
           angle_ste=angle_sd/n)
  statty<-statty %>%
    mutate(sal_005= sal_mean*ci,
           temp_005=temp_mean*ci,
           angle_005=angle_mean*ci) #calculates 5% of mean
  statty<-statty %>%
    mutate(sal_pos_ci=sal_005+sal_mean,
           sal_neg_ci=sal_mean-sal_005,
           temp_pos_ci=temp_005+temp_mean,
           temp_neg_ci=temp_mean-temp_005,
           angle_pos_ci=angle_005+angle_mean,
           angle_neg_ci=angle_mean-angle_005)
  statty<-statty %>%
    mutate(sal_pos_ste=sal_mean+(sal_ste*1),
            sal_neg_ste=sal_mean+(sal_ste*-1),
            temp_pos_ste=temp_mean+(temp_ste*1),
            temp_neg_ste=temp_mean+(temp_ste*-1),
            angle_pos_ste=angle_mean+(angle_ste*1),
            angle_neg_ste=angle_mean+(angle_ste*-1))
  #move these values into a column in c
  c<-left_join(statty, c, by="net_number")
#goal: test depth sepcific ci value against any given value in that same depth 
  #check MSc work for how to do this against treatment levels??
  c<-c%>%mutate(sal_ci_test=ifelse(sal<sal_pos_ci & sal>sal_neg_ci,print("ok"),print("fail"))) #works with individual files but not with list???
  c<-c%>%mutate(temp_ci_test=ifelse(temp_DegC<temp_pos_ci & temp_DegC> temp_neg_ci,print("ok"),print("fail")))
  c<-c%>%mutate(angle_ci_test=ifelse(net_angle<angle_pos_ci & net_angle> angle_neg_ci,print("ok"),print("fail")))
  c<-c%>%mutate(sal_ste_test=ifelse(sal<sal_pos_ste & sal>sal_neg_ste,print("ok"),print("fail"))) #works with individual files but not with list???
  c<-c%>%mutate(temp_ste_test=ifelse(temp_DegC<temp_pos_ste & temp_DegC>temp_neg_ste,print("ok"),print("fail")))
  c<-c%>%mutate(angle_ste_test=ifelse(net_angle<angle_pos_ste & net_angle> angle_neg_ste,print("ok"),print("fail")))
  nrow(c)
  print(paste(input, nrow(c)))
  ciy<-filter(c, sal_ci_test=="ok" & temp_ci_test=="ok")
  stey<-filter(c, sal_ste_test=="ok" & temp_ste_test=="ok")
  print(paste(input, nrow(ciy)))
  print(paste(input, nrow(stey)))
  nrow(ciy)
  nrow(stey) #standard error specifications are too stringent
  fixed_input<-str_sub(input,1, -4)
  outname = paste(input,"95ci", '.csv', sep = "")
  write.csv(x=ciy, file=outname)
  }
for (i in 1:length(files)) {
  moc_qc_stats(files[i])
  
}
#note! once you run this code it will refilter the newly created .csv files

#CTD_lite#####
setwd("~/M&B_larval_dist_1996/MOCNESS Data/TC-86-04")
files<-list.files(path=".", pattern = "*.csv")
include<-"95ci"
files<-files[str_detect(files, include, negate = F)]
#files<-c("TC8604002.csv95ci.csv","TC8604003.csv95ci.csv","TC8604004.csv95ci.csv","TC8604045.csv95ci.csv") #test
ctd_lite = function(input) {
  c<-read.csv(input)
  c<-select(c,cruise,moc_id,date_time,station_number, net_number,depth_m, temp_DegC, sal)
  c<-rename(c, "STATION_NUMBER"="station_number")
  fixed_input<-str_sub(input,1, -13)
  outname = paste(fixed_input,"_","lite",'.csv', sep = "")
  write.csv(x=c, file=outname) # drop last 4 char from input
}
for (i in 1:length(files)) {
  ctd_lite(files[i])
}

