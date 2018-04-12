#clean up the workspace memory
rm(list=ls())
options(encoding = "UTF-8") 

#Param roster - Aetlum vid ad hafa thad hardkodad?

#Importum pakka
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

#---------------------------------Biðlistaþjónusta----------------------------------------#

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


#Rodum bidlistandum upp eftir sersviidi, laekni, priority af sjukling og svo skrasetningardagsetning a bidlista.
#Gert til ad numera sjuklinga sidar
Bidlisti <- Bidlisti[order(Bidlisti$OrbitOperation.OperationSpecialty,
                           Bidlisti$OrbitOperation.RequestedOperator_Name, 
                           Bidlisti$OrbitOperation.OperationPriority,
                           Bidlisti$OrbitOperation.RegistrDay),]

#----------------------------------Skrifum gögn--------------------------------------#

# skrifa í AMPL dat skrá allt sem þarf til að besta
fname = c("kvid.dat")
#Lesa söguleg gögn (sem lesa.R skrifar)
load("adkort.Rdata")

#--------------------------------LAEKNAR-----------------------------------------------#
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
cat("set Surgeon := \n",file=fname, sep=" ", append=TRUE)
uLaeknir = unique(Bidlisti$OrbitOperation.RequestedOperator_Name)
Laeknir = Bidlisti$OrbitOperation.RequestedOperator_Name
for (l in uLaeknir) {
  if (is.na(l) == FALSE) {
    cat(paste0('"',l,'"\n'),file=fname, sep = " ", append = TRUE)
  }
}
cat(";", file=fname, sep="\n", append = TRUE)

#Skrifum nidur vaktir - turfum vid ekki ad fa thetta fra spitalanum?
cat("param roster := \n",file=fname, sep=" ", append=TRUE)


# Skrifum kortin - ath thau innihalda kt og kort
Adgerdarkort = Bidlisti$OrbitOperation.OperationCard
#Kennitolur sjuklinga
Kennitala = Bidlisti$OrbitOperation.PatientSSN

#Skrifum ut demand theas hvada sjuklingur og adgerd er per laekni
cat("param Demand : ", file = "kvid.dat", sep = "", append=TRUE)
for (i in c(1:length(Bidlisti$OrbitOperation.PatientSSN))) {
  adgek = Adgerdarkort[i]
  kt = Kennitala[i]
  cat(paste0('"',kt,'-',adgek,'" '), file="kvid.dat", sep = "\t", append=TRUE)
}
cat(" := ", file = fname, sep = "\n", append=TRUE)

for (lak in uLaeknir) {
  cat(paste0('"',lak,'"'), file=fname, sep = c(" "), append=TRUE)
  # how many OR of type case
  id = which(Laeknir == lak)
  for (i in c(1:length(Adgerdarkort))) {
    if (i %in% id)
      cat(" 1 ", file=fname, sep = c(" "), append=TRUE)
    else
      cat(" 0 ", file=fname, sep = c(" "), append=TRUE)
  }
  cat("",file=fname, sep = "\n", append=TRUE)
  
}
cat(";",file=fname, sep = "\n", append=TRUE)

#---------------------------------DAGAR--------------------------------------------------#
#Buum til runu af dagsetningum - user defined..
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

#Merkjum helgar med binary breytum theas ef thad helgi tha 1 annars 0 
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


#---------------------------------Skurdstofur--------------------------------------------#

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

#Skrifum ut akut stofur - ekkert notad?
cat("set aRoom := HbStofa1 , HbStofa3 , HbStofa4 , HbStofa6;\n", file=fname, append=TRUE)

#Skrifum ut stofur sem almenna notar
cat("set Room := HbStofa3 , HbStofa6;\n", file=fname, append=TRUE)

#---------------------------------Sjuklingar og kort --------------------------------------#


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


#Skrifum planadar adgerdir i stofur og tima utfra bidlista
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
    cat(paste0('"',kt,'-',adgek,'"','\t','"',Dagur,'"','\t','"',stofa,'"','\t','"',lak,'"'," 1",'\n'),
        file=fname, sep=" ", append=TRUE)
  }
}
cat(";", file=fname, sep="\n", append = TRUE)




#-----------------------------------Priority---------------------------------------------#
#Thetta er ekki alveg rett - tharf ad utfaera betur
Priority <- Bidlisti$OrbitOperation.OperationPriority

##Skrifum Priority numer fyrir sjuklinga
Bidlisti$Priority  <- sequence(rle(Bidlisti$OrbitOperation.RequestedOperator_Name)$lengths)

#Baetum Priority vid df

#Reiknum ut hversu marga daga vidkomandi hefur thurft ad bida a bidlista

Bidlisti$Time.Diff.in.Days <- round(as.numeric(difftime(Sys.Date(), 
                                                        Bidlisti$OrbitOperation.RegistrDay,
                                                        units = 'days')))



#---------------------------------WARD og ICU -------------------------------------------#


#Skrifum ut ef sjuklingur tharf ad fara a ICU yfir nott utfra bidlista
#Eg skil ekki alveg hvad thetta tydir..
YfirNottAICU = Bidlisti$OrbitOperation.OvernightICU
cat("param gotoICU := \n", file=fname, sep=" ", append=TRUE)
for(i in c(1:length(YfirNottAICU))){
  kt = Kennitala[i]
  adgek = Adgerdarkort[i]
  icu = YfirNottAICU[i]
  cat(paste0('"',kt,'-',adgek,'"',"\t", icu,"\n"), file=fname, sep=" ", append=TRUE)
}
cat(";", file=fname, sep="\n", append = TRUE)



#Skrifum ut ef sjuklingur tharf legu ad halda eftir adgerd utfra bidlista
Lega = Bidlisti$OrbitOperation.PatientAdmission
cat("param gotoWard := ", file = "kvid.dat", sep = "\n", append=TRUE)
for(i in c(1:length(Lega))){
  lega = Lega[i]
  adge = Adgerdarkort[i]
  kt = Kennitala[i]
  if(lega %in% c("Legudeild")){
    cat(paste0('"',kt,'-',adge,'"',' 1\n'), file=fname, append=TRUE)
  }
  else if (lega %in% c("Dagdeild")){
    cat(paste0('"',kt,'-',adge,'"',' 0\n'), file=fname, append=TRUE)
  }
  else{
    #Thetta tilfelli veit eg ekki hvernig a ad tulka NaN fyrir hvort lega eda ekki?
    #Thurfum ad komast ad tvi
    cat(paste0('"',kt,'-',adge,'"',' 0\n'), file=fname, append=TRUE)
  }
}
cat(";",file=fname, sep = "\n", append=TRUE)


#Skrifum ut expectedWard fyrir hvern dag, viljum einungis tha sem eru merktir fyrir ad thurfa ward
#Ath her vantar fallid fyrir likur a legu..thad heitir CodeExpectedWard[case,j]
cat("param expectedWard : ",file=fname,sep="\n", append = TRUE)
for (j in c(1:7))
  cat(sprintf(" %d ", j), file=fname, sep = "", append=TRUE)
cat(":=",file=fname,sep="\n", append=TRUE)

for (i in c(1:length(Adgerdarkort))) {
  idx=which(Lega[i]==c("Legudeild"))
  if(length(idx)>0){
    adge=Adgerdarkort[i]
    kt = Kennitala[i]
    cat(paste0('"',kt,'-',adge,'"','\t',' 1'),file=fname ,sep="", append=TRUE)
    #cat(sprintf(" o%03d_%s 1",i,case), file="kvid.dat", sep = "", append=TRUE)
    for (j in c(1:6))
      cat(sprintf(" %.3f ",j), file="kvid.dat", sep =
            "", append=TRUE)
    cat(" ",file=fname,sep="\n", append=TRUE)
  }
}
cat(";",file=fname,sep="\n", append=TRUE)

#Skrifum ut expectedICU fyrir hvern dag, viljum einungis tha sem eru merktir fyrir ad thurfa ad fara i ICU utfra bidlista?
#Ath her vantar fallid fyrir likur a legu..thad heitir CodeExpectedGjor[case,j]
cat("param expectedICU : ",file=fname,sep="\n", append = TRUE)
for (j in c(1:7))
  cat(sprintf(" %d ", j), file=fname, sep = "", append=TRUE)
cat(":=",file=fname,sep="\n", append=TRUE)

for (i in c(1:length(Adgerdarkort))) {
  idx=which(YfirNottAICU[i]==c("1"))
  if(length(idx)>0){
    adge=Adgerdarkort[i]
    kt = Kennitala[i]
    cat(paste0('"',kt,'-',adge,'"','\t',' 1'),file=fname ,sep="", append=TRUE)
    #cat(sprintf(" o%03d_%s 1",i,case), file="kvid.dat", sep = "", append=TRUE)
    for (j in c(1:6))
      cat(sprintf(" %.3f ",j), file="kvid.dat", sep =
            "", append=TRUE)
    cat(" ",file=fname,sep="\n", append=TRUE)
  }
}
cat(";",file=fname,sep="\n", append=TRUE)


#---------------------------------Surgery times-------------------------------------------#

#Skrifum ut fjolda scenarios
scenarios=10
cat("param nscenarios :=",scenarios,";", file = "kvid.dat", sep = "\n", append=TRUE)

#Thurfum adeins ad utfaera thetta betur?
cat("param SurgeryTime := ",file="kvid.dat",sep="\n", append=TRUE)

Scenario = c(1:scenarios)

adkort2 <- adkort[order(-xtfrm(adkort$Dagsetning),adkort$Adgerdakort),] 

for (i in c(1:length(Adgerdarkort))){
  adge = Adgerdarkort[i] #ur bidlista
  laeknir = Laeknir[i] #ur bidlista
  kt = Kennitala[i]
  idx = which(adkort2$Adgerdakort==adge & adkort2$Laeknir==laeknir
              & adkort2$AdgerdaTimi<=24*60 & adkort2$AdgerdaTimi>0 & adkort2$Dagsetning>=Sys.Date()-(365*4))
  if(length(idx)<10){
    #Notum tima fyrir annara ef thad finnst ekki annar - forum 4 ar aftur i timann
    idx = which(adkort2$Adgerdakort==adge &
                  adkort2$AdgerdaTimi<=24*60 & adkort2$AdgerdaTimi>0 & adkort2$Dagsetning>=Sys.Date()-(365*4))
  }
  idx <- rev(idx)
  idx <- idx[1:min(10,length(idx))]
  for (s in Scenario){
    id = sample(idx,1)
    cat(paste0('"',kt,'-', adge,'"','\t' ,'"', laeknir,'"','\t' ,s,'\t',
               as.numeric(adkort2$AdgerdaTimi[id])), file="kvid.dat", sep = "\n",
        append=TRUE)
  }
}


cat(";",file="kvid.dat",sep="\n", append=TRUE)





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
