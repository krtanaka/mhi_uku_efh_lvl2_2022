#this script should combine many CTD.csv files with the existing metadata for 
#cruises which caught uku
#My goal was to make this into a function that would process through each of the CTD 
#data files and add them onto the exisiting larval distribution and metadata file
#Currently this script is in a shambles of my trying to get this to work with a 
#single file first
#CTD data was arranged into their current constellation with mocness_function.R
#@Kisei, you'll probably have to change the file paths for these, sorry!


#housekeeping####
library(tidyverse)
library(lubridate)

###filter larval distribution  data####
#filters all data about the larval distribution and associated meta down to 
#the only cruise where uku were caught (cast_data_with_coord.csv)
big<-read.csv("~/M&B_larval_dist_1996/MOCNESS Data/TC-86-04/cast_data_with_coord.csv")
big<-filter(big, CRUISE_NUMBER=="8604") #this is the only cruise that we have both mocness data and uku data for
big<-big[,4:49] #filter it down to fewer columns to be slightly easier to work with/ generate fewer duplicate column names
#write.csv(big,"~/M&B_larval_dist_1996/MOCNESS Data/TC-86-04/big.csv") #write into new csv that function can loop over iteratively
#add larval distribution data to CTD data#####

#test file path:
#list of three files to test together
#files<-c("TC8604002_lite.csv", "TC8604003_lite.csv", "TC8604004_lite.csv")#test

#single file to try with to start
ctd_sta<-read.csv("~/M&B_larval_dist_1996/MOCNESS Data/TC-86-04/TC8604002_lite.csv")

#load cast and coord data for only tc86-04 (from ###filter larval distribution  data#### section)
cast_coord<-read.csv("~/M&B_larval_dist_1996/MOCNESS Data/TC-86-04/big.csv")

#setting expected number of rows:
#should end up with: number of rows in ctd_sta* number of records for whichever station's ctd data this is 
n<-unique(ctd_sta$net_number) #this should be 9 each time
uniquenets<-nrow(as_tibble(n)) 
unique_dist_points<-nrow(filter(cast_coord, STATION_NUMBER=="2")) #number of rows from 
#the distribution data which are associated with the station number who's CTD data we're looking at
ctd_points<-(nrow(ctd_sta)) #number of rows of ctd data
#to see the expected number of rows
unique_dist_points*ctd_points
#merge it####
#moc_id is unique to each CTD cast by depth range (assigned using net number)
#below are my tests for both inner and left joins with CTD station 8 data, I've changed it to st.2
cast_coord_inner<-inner_join(x=ctd_sta,y=cast_coord, by="moc_id")# keep=F, copy=T,na_matches = "never")
cast_coord_left<-left_join(x=ctd_sta,y=cast_coord, by="moc_id")# keep=F, copy=T,na_matches = "never")

#check to make sure only stations from the target CTD data are represented
left_sta<-filter(cast_coord_left, STATION_NUMBER.x==2) #same as orig merge type
inner_sta<-filter(cast_coord_inner, STATION_NUMBER.x==2) #same as orig

#below is my trying to figure out why the size of the merged df's do not match the expected value
nrow(cast_coord)/nrow(ctd_sta)  

#thought perhaps depth numbers might be at play since they are nested within net number
m<-(unique(ctd_sta$depth_m)) #79 values
uniquedepths<-nrow(as_tibble(m))
n<-unique(ctd_sta$net_number) 
uniquenets<-nrow(as_tibble(n))

#tested and failed merge types#####
#cast_coord_ctd<-inner_join(x=ctd_sta,y=cast_coord, by="moc_id")# keep=F, copy=T,na_matches = "never")
#cast_coord_right<-right_join(x=ctd_sta,y=cast_coord, by="moc_id")# keep=F, copy=T,na_matches = "never")
#cast_coord_full<-full_join(x=ctd_sta,y=cast_coord, by="moc_id")# keep=F, copy=T,na_matches = "never")
#full_sta<-filter(cast_coord_full, STATION_NUMBER.x==6) #less than orig
#right_sta<-filter(cast_coord_right, STATION_NUMBER.x==6) #less than orig

#skeleton of function once test merges work:#####
#essentially it will call the larval distribution CSV and continually add
#CTD data to itself each go around
#hence why its saved as the same name each time

#ctd_function<-function(input) {
  #c<-read.csv(input)
  #cast_coord_left<-left_join(x=ctd_sta,y=cast_coord, by="moc_id")
  #write.csv(cast_coord_ctd,"~/M&B_larval_dist_1996/MOCNESS Data/TC-86-04/big.csv")
#}
for (i in 1:length(files)) {
  ctd_function(files[i])
}
#big<-read.csv("~/M&B_larval_dist_1996/MOCNESS Data/TC-86-04/big.csv")
#str(big)

#misc####
#file path for the real data:
# setwd("~/M&B_larval_dist_1996/MOCNESS Data/TC-86-04")
# files<-list.files(path=".", pattern = "*.csv")
# include<-"lite"
# files<-files[str_detect(files, include, negate = F)]

#tried and failed merge ideas to remove duplicate columns#####
#(replace_na(cast_coord_ctd$depth_m, c$depth_m))
#mutate(date_time = coalesce(date_time.x, date_time.y)) %>% 
#select(-date_time.x, -date_time.y) %>%
#mutate(coalesce(STATION_NUMBER.x, STATION_NUMBER.y))# %>% 
#select(-station_number.x, -station_number.y) %>%
#mutate(depth_m = coalesce(depth_m.x, depth_m.y)) %>% 
#select(-depth_m.x, -depth_m.y)%>%
#mutate(temp_DegC = coalesce(temp_DegC.x, temp_DegC.y)) %>% 
#select(-temp_DegC.x, -temp_DegC.y) %>%
#mutate(sal = coalesce(sal.x, sal.y)) %>% 
#select(-sal.x, -sal.y)
