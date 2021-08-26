library(data.table)
library(stringr)
library(utf8)
library(dplyr)
countSpaces <- function(s) { sapply(gregexpr(" ", s), function(p) { sum(p>=0) } ) }

data.dir = "~/github/tcpd_data/data/"
parties_normalized = fread(paste0(data.dir,"all_normalized_party_names.csv",na=""))

dt =unique(parties_normalized[,c("ID","Expanded.Party.Name")])
dt[,freq := .N,by=c("ID")]

party.names = dt[,list(Party_Name = names(sort(summary(as.factor(.SD$Expanded.Party.Name)), decreasing=T))[1]) ,by=c("ID")]

party.types = parties_normalized[,list(Party_Type = names(sort(summary(as.factor(.SD$Party_type_TCPD[which(!is.na(.SD$Party_type_TCPD))])), decreasing=T))[1]) ,by=c("ID")]
party.types[,freq := .N,by=c("ID")]
if(max(party.types$freq ==1)){
  party.types$freq = NULL
  party.names = left_join(party.names,party.types)
}

party.abbs = parties_normalized[!is.na(ECI_Abb),list(Frequent_Abbreviation= names(sort(summary(as.factor(.SD$ECI_Abb[which(!is.na(.SD$ECI_Abb))])), decreasing=T))[1],
                                      Last_Abbreviation = .SD[order(-.SD$Year)]$ECI_Abb[1]),by=c("ID")]

which(is.na(party.names$Party_Name))

all.ae = fread(paste0(data.dir,"AE/Analysis_Data/Consolidated_AE_mastersheet.csv",na=""))
all.ae$abb_spaces = sapply(all.ae$Party, countSpaces)
ae.party = all.ae[,list(
  Abbreviations= list(unique(.SD$Party[which(.SD$abb_spaces<=2)])),
  No_Assemblies_Contested = length(unique(.SD$Assembly_No)),
  Assemblies_Contested = list(unique(.SD$Assembly_No)),
  Candidates_Contested = .N,
  Candidates_Represented = length(which(.SD$Position ==1)),
  Females_Contested = length(which(.SD$Sex =="F")),
  Females_Represented = length(which(.SD$Position ==1 & .SD$Sex =="F")),
  SC_Seats_Contested = length(which(.SD$Constituency_Type =="SC")),
  SC_Seats_Represented = length(which(.SD$Position ==1 & .SD$Constituency_Type =="SC")),
  ST_Seats_Contested = length(which(.SD$Constituency_Type =="ST")),
  ST_Seats_Represented = length(which(.SD$Position ==1 & .SD$Constituency_Type =="ST")),
  BiPoll_Contested = length(which(.SD$Poll_No >=1)),
  Start_Year = as.integer(min(.SD$Year,na.rm=T)),
  Last_Year = as.integer(max(.SD$Year,na.rm=T))
),by=c("Party_ID","State_Name")]
which(is.na(ae.party$Start_Year))
print(ae.party[which(is.na(ae.party$Start_Year)),])


ae.party$Assembly = "Vidhan_Sabha"
ae.party = ae.party[order(ae.party$State_Name, -ae.party$Candidates_Represented),]
all.ge = fread(paste0(data.dir, "GE/Data/derived/mastersheet.csv",na=""))
all.ge$abb_spaces = sapply(all.ge$Party, countSpaces)
ge.party = all.ge[,list(
  Abbreviations= list(unique(.SD$Party[which(.SD$abb_spaces<=2)])),
  No_Assemblies_Contested = length(unique(.SD$Assembly_No)),
  Assemblies_Contested = list(unique(.SD$Assembly_No)),
  Candidates_Contested = .N,
  Candidates_Represented = length(which(.SD$Position ==1)),
  Females_Contested = length(which(.SD$Sex =="F")),
  Females_Represented = length(which(.SD$Position ==1 & .SD$Sex =="F")),
  SC_Seats_Contested = length(which(.SD$Constituency_Type =="SC")),
  SC_Seats_Represented = length(which(.SD$Position ==1 & .SD$Constituency_Type =="SC")),
  ST_Seats_Contested = length(which(.SD$Constituency_Type =="ST")),
  ST_Seats_Represented = length(which(.SD$Position ==1 & .SD$Constituency_Type =="ST")),
  BiPoll_Contested = length(which(.SD$Poll_No >=1)),
  Start_Year = as.integer(min(.SD$Year,na.rm=T)),
  Last_Year = as.integer(max(.SD$Year,na.rm=T))
),by=c("Party_ID")]
ge.party$State_Name = "All_States"
ge.party.st = all.ge[,list(
  Abbreviations= list(unique(.SD$Party[which(.SD$abb_spaces<=2)])),
  No_Assemblies_Contested = length(unique(.SD$Assembly_No)),
  Assemblies_Contested = list(unique(.SD$Assembly_No)),
  Candidates_Contested = .N,
  Candidates_Represented = length(which(.SD$Position ==1)),
  Females_Contested = length(which(.SD$Sex =="F")),
  Females_Represented = length(which(.SD$Position ==1 & .SD$Sex =="F")),
  SC_Seats_Contested = length(which(.SD$Constituency_Type =="SC")),
  SC_Seats_Represented = length(which(.SD$Position ==1 & .SD$Constituency_Type =="SC")),
  ST_Seats_Contested = length(which(.SD$Constituency_Type =="ST")),
  ST_Seats_Represented = length(which(.SD$Position ==1 & .SD$Constituency_Type =="ST")),
  BiPoll_Contested = length(which(.SD$Poll_No >=1)),
  Start_Year = as.integer(min(.SD$Year,na.rm=T)),
  Last_Year = as.integer(max(.SD$Year,na.rm=T))
),by=c("Party_ID","State_Name")]
all.ge.party = rbind(ge.party,ge.party.st)
all.ge.party = all.ge.party[order(all.ge.party$State_Name, -all.ge.party$Candidates_Represented),]
all.ge.party$Assembly= "Lok_Sabha"
all.party.meta = rbind(all.ge.party,ae.party)

names(party.names)
all.party.names = left_join(all.party.meta,party.names,by=c("Party_ID"= "ID"))
all.party.abbs = left_join(all.party.names,party.abbs,by=c("Party_ID"= "ID"))
cols = c("Assembly","State_Name","Party_Name","Party_Type","Party_ID","Frequent_Abbreviation","Last_Abbreviation","Abbreviations","Start_Year","Last_Year","No_Assemblies_Contested","Assemblies_Contested","Candidates_Contested","Candidates_Represented","Females_Contested","Females_Represented","SC_Seats_Contested","SC_Seats_Represented","ST_Seats_Contested","ST_Seats_Represented","BiPoll_Contested")
cols[which(!cols %in% names(all.party.abbs))]
all.party.meta = subset(all.party.abbs,select= cols)
fwrite(all.party.meta,"../TCPD-PPI_1962_2021.csv",na="")

idx = which((all.party.meta$Party_Name == "NA's" | all.party.meta$Party_Name == "XXX") & all.party.meta$BiPoll_Contested < all.party.meta$Candidates_Contested)
print(all.party.meta[idx,])




