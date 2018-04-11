# clean up the workspace memory
rm(list=ls())
#Sma comment: 
#Viljum vid nota dagsetningar i stad numera? Hvort er meira clean?
#Eg notadi dagsetningar..Ekkert mal ad breyta

#TODO:
#Param expectedWard
#Param expectedICU
#Param gotoWard
#Param surgeryTime
#Param demand
#param priority - working on it
#Param roster - Aetlum vid ad hafa thad hardkodad?
#----------------------------------------#
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

# Lesa söguleg gögn (sem lesa.R skrifar)
#load(file="adkort.Rdata")

# Lesa biðlista (nota þjónustu seinna)
get_base_parsed <- fromJSON(txt="getWaitingList.json", flatten = TRUE)
get_base_df <- as.data.frame(get_base_parsed$OrbitWaitingList, stringsAsFactors = TRUE)

# Endurskilgreina tegundir
get_base_df$OrbitOperation.RegistrDay <- as.POSIXct(get_base_df$OrbitOperation.RegistrDay, tz="gmt")
get_base_df$OrbitOperation.PlannedStartTime_Date <- as.POSIXct(get_base_df$OrbitOperation.PlannedStartTime_Date, tz="gmt")
get_base_df$OrbitOperation.PlannedFinishTime_Date <- as.POSIXct(get_base_df$OrbitOperation.PlannedFinishTime_Date, tz="gmt")
get_base_df$OrbitOperation.OperationCard_OpTime <- as.numeric(get_base_df$OrbitOperation.OperationCard_OpTime)
get_base_df$OrbitOperation.OperationCard_PostTime <- as.numeric(get_base_df$OrbitOperation.OperationCard_PostTime)
get_base_df$OrbitOperation.OperationCard_PreTime <- as.numeric(get_base_df$OrbitOperation.OperationCard_PreTime)

# Velja sérsvið
Bidlisti = get_base_df[get_base_df$OrbitOperation.OperationSpecialty %in% c("Hb. Alm.", "Hb. Þvagf."),]

#Byrjum ad sortera bidlistann eftir laekni, priority og skrasetningardagsetningu
Bidlisti <- arrange(Bidlisti, Bidlisti$OrbitOperation.OperationDepartment ,
                    Bidlisti$OrbitOperation.RequestedOperator_Name,Bidlisti$OrbitOperation.OperationPriority,
                    Bidlisti$OrbitOperation.RegistrDay)



# skrifa í AMPL dat skrá allt sem þarf til að besta
fname = c("kvid.dat")

# Skrifum niður lækna - thad er e-d sem heitir lika Surgeon..- veit ekki alveg muninn?
cat("set rSurgeon := \n",file=fname, sep=" ")
Laeknir = unique(Bidlisti$OrbitOperation.RequestedOperator_Name)
for (l in Laeknir) {
  if (is.na(l) == FALSE) {
    cat(paste0('"',l,'"\n'),file=fname, sep = " ", append = TRUE)
  }
}
cat(";", file=fname, sep="\n", append = TRUE)

#Skrifum nidur laekna
cat("set Surgeon := \n",file=fname, sep=" ")
uLaeknir = unique(Bidlisti$OrbitOperation.RequestedOperator_Name)
Laeknir = Bidlisti$OrbitOperation.RequestedOperator_Name
for (l in uLaeknir) {
  if (is.na(l) == FALSE) {
    cat(paste0('"',l,'"\n'),file=fname, sep = " ", append = TRUE)
  }
}
cat(";", file=fname, sep="\n", append = TRUE)

#Skrifum nidur vaktir - turfum vid ekki ad fa thetta fra spitalanum?


#Buum til runu med dagsetningum
numberOfDays = 30
StartDate = as.Date("2018-04-11")
RodDaga<- seq(StartDate, by=1, length.out = numberOfDays)
wday(RodDaga, label=TRUE)

#Skilgreinum mengi daga
cat("set Day := \n",file=fname, sep=" ", append=TRUE)
for (r in c(1:length(RodDaga))) {
    Dagar <- RodDaga[r]
    cat(paste0('"',Dagar,'"\n'),file=fname, sep = " ", append = TRUE)
}
cat(";", file=fname, sep="\n", append = TRUE)

#Holdum utan um helgar og setjum 1 ef dagsetning er lau eda sun annars 0
cat("param isWeekend := \n",file=fname, sep=" ", append=TRUE)
for(r in c(1:length(RodDaga))){
  Dagar <- wday(RodDaga)[r]
  Dagsetningar <- RodDaga[r]
  cat(paste0('"',Dagsetningar,'"'),file=fname, sep = " ", append = TRUE)
  if(Dagar==1 | Dagar==7)
    cat(" 1", file=fname, sep="\n", append=TRUE)
  else
    cat(" 0", file=fname, sep="\n", append=TRUE)
  
}
cat(";", file=fname, sep="\n", append = TRUE)


#Skrifum nidur fjolda min sem skurdstofur eru opnar á hverjum degi
#480 min virka daga, man-fim
#330 min a fostudogum,
#0 min um helgar

cat("param T := \n",file=fname, sep=" ", append=TRUE)
for(r in c(1:length(RodDaga))){
  Dagar <- wday(RodDaga)[r]
  Dagsetningar <- RodDaga[r]
  cat(paste0('"',Dagsetningar,'"'),file=fname, sep = " ", append = TRUE)
  if(Dagar %in% c('2','3','4','5')){
    cat(" 480", file=fname, sep="\n", append=TRUE)}
  else if (Dagar %in% c('1','7')){
    cat(" 0", file=fname, sep="\n", append=TRUE)}
  else
    cat(" 330", file=fname, sep="\n", append=TRUE)
}
cat(";", file=fname, sep="\n", append = TRUE)


# Skrifum kortin - ath thau innihalda kt og kort
Adgerdarkort = Bidlisti$OrbitOperation.OperationCard
#Kennitolur sjuklinga
Kennitala = Bidlisti$OrbitOperation.PatientSSN

cat("set Code := \n", file=fname, sep=" ", append=TRUE)
for(a in c(1:length(Adgerdarkort))){
  kt = Kennitala[a]
  adgek = Adgerdarkort[a]
  cat(paste0('"',kt,'-',adgek,'"\n'), file=fname, sep=" ", append=TRUE)
}
cat(";", file=fname, sep="\n", append = TRUE)


#Skrifum planadar adgerdir 
Adgerdardagur <- as.Date(Bidlisti$OrbitOperation.PlannedStartTime_Date)
Stofa <- Bidlisti$OrbitOperation.OperationRoom

cat("param realschedule := \n", file=fname, sep=" ", append=TRUE)
for(i in c(1:length(Adgerdardagur))){
  idx= which(!is.na(Adgerdardagur[i]))
  kt=Kennitala[i]
  adgek = Adgerdarkort[i]
  lak = Laeknir[i]
  Dagur = Adgerdardagur[i]
  stofa = Stofa[i]
  if(length(idx)>0){
    cat(paste0('"',kt,'-',adgek,'"','\t','"',Dagur,'"','\t','"',stofa,'"','\t','"',lak,'"'," 1",'\n'),file=fname, sep=" ", append=TRUE)
    
    
  }
}
cat(";", file=fname, sep="\n", append = TRUE)


##Skrifum Priority sjuklinga


#Skrifum nidur ef sjuklingur tharf ad fara i ICU yfir nott?? 
  #Hvad tydir thad? er thad ICU bara eftir adgerd
  #Thott thad vari skemur en igildi einnar naetur?
YfirNottAICU = Bidlisti$OrbitOperation.OvernightICU
cat("param gotoICU := \n", file=fname, sep=" ", append=TRUE)
for(i in c(1:length(YfirNottAICU))){
  kt = Kennitala[i]
  adgek = Adgerdarkort[i]
  icu = YfirNottAICU[i]
  cat(paste0('"',kt,'-',adgek,'"',"\t", icu,"\n"), file=fname, sep=" ", append=TRUE)
}
cat(";", file=fname, sep="\n", append = TRUE)



# Skrifum niður verkefni (kt.)

#OLD wdname = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
#OLD DaysName = sprintf("%s_%d_%d_%d ",wdname[wday(DAYS)],day(DAYS),month(DAYS),year(DAYS))
#OLD nuDaysName = sprintf("%s_%d_%d_%d ",wdname[wday(NUDAYS)],day(NUDAYS),month(NUDAYS),year(NUDAYS))
#OLD for (i in c(1:length(nuDaysName))) {
#OLD   nuDaysName[i] <- sprintf("%d", which(nuDaysName[i] == DaysName)[1])
#OLD }

#OLD DaysName = sprintf("%d ", c(1:length(DAYS)));

#OLD daybuffer = sprintf("%d ", 1+c(length(DAYS):(length(DAYS)+5)));

#OLD cat("set Day := ",file="kvid.dat",sep="\n")
#OLD for (d in DaysName) {
#OLD   cat(d, file="kvid.dat", sep = "", append=TRUE)
#OLD }
#OLD for (d in daybuffer) {
#OLD   cat(d, file="kvid.dat", sep = "", append=TRUE)
#OLD }
#OLD cat(";",file="kvid.dat", sep = "\n", append=TRUE)
#OLD 
#OLD cat("param isWeekend := ",file="kvid.dat",sep="\n", append=TRUE)
#OLD for (i in c(1:length(DAYS))) {
#OLD   d <- wday(DAYS)[i]
#OLD   dn <- DaysName[i]
#OLD   cat(dn, file="kvid.dat", sep = "", append=TRUE)
#OLD   if (d == 1 | d == 7)
#OLD     cat(" 1", file="kvid.dat", sep = "\n", append=TRUE)
#OLD   else
#OLD     cat(" 0", file="kvid.dat", sep = "\n", append=TRUE)
#OLD }
#OLD for (d in daybuffer) {
#OLD   cat(sprintf("%s 1",d), file="kvid.dat", sep = "\n", append=TRUE)
#OLD }
#OLD cat(";",file="kvid.dat", sep = "\n", append=TRUE)S
