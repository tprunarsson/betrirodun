#clean up the workspace memory
rm(list=ls())
options(encoding = "UTF-8") 


#TODO: Priority fasti 2 

#Importum pakka
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(tictoc)
#bradabirgda:
load("Stuff.Rdata")


#---------------------------------Biðlistaþjónusta----------------------------------------#
tic()

# Lesa biðlista (nota þjónustu seinna)
#get_base_parsed <- fromJSON(txt="getWaitingList.json", flatten = TRUE)
#get_base_df <- as.data.frame(get_base_parsed$OrbitWaitingList, stringsAsFactors = TRUE)

# Endurskilgreina tegundir
#get_base_df$OrbitOperation.RegistrDay <- as.POSIXct(get_base_df$OrbitOperation.RegistrDay, tz="gmt")
#get_base_df$OrbitOperation.PlannedStartTime_Date <- as.POSIXct(get_base_df$OrbitOperation.PlannedStartTime_Date, tz="gmt")
#get_base_df$OrbitOperation.PlannedFinishTime_Date <- as.POSIXct(get_base_df$OrbitOperation.PlannedFinishTime_Date, tz="gmt")
#get_base_df$OrbitOperation.OperationCard_OpTime <- as.numeric(get_base_df$OrbitOperation.OperationCard_OpTime)
#get_base_df$OrbitOperation.OperationCard_PostTime <- as.numeric(get_base_df$OrbitOperation.OperationCard_PostTime)
#get_base_df$OrbitOperation.OperationCard_PreTime <- as.numeric(get_base_df$OrbitOperation.OperationCard_PreTime)


#Buum til runu af dagsetningum - user defined.. 
numberOfDays = 30
#StartDate = as.Date(Sys.Date())
StartDate = as.Date(c("2018-05-02"))
RodDaga<- seq(StartDate, by=1, length.out = numberOfDays)
RodDagaNumer <- seq(1,by=1, length.out = numberOfDays)
dfdays <- data.frame(RodDaga, RodDagaNumer)
EndDate = max(RodDaga)




#Bidlisti fyrir planadar ICU
Bidlisti_for_ICU = Bidlisti[Bidlisti$OrbitOperation.StatusName %in% c("Skipulagt"),]
Bidlisti_for_ICU = Bidlisti_for_ICU[Bidlisti_for_ICU$OrbitOperation.OvernightICU %in% c("1"),]
Bidlisti_for_ICU = Bidlisti_for_ICU[!Bidlisti_for_ICU$OrbitOperation.OperationSpecialty %in% c("Hb. Alm."),]
Bidlisti_for_ICU = Bidlisti_for_ICU[as.Date(Bidlisti_for_ICU$OrbitOperation.PlannedStartTime_Date) %in% RodDaga, ]

#Bidlisti adrir en Almenna sem nota sama legurrymi eda hafa planad inna thad
Bidlisti_for_WARD = Bidlisti[!Bidlisti$OrbitOperation.OperationSpecialty %in% c("Hb. Alm."),]
Bidlisti_for_WARD= Bidlisti_for_WARD[
  Bidlisti_for_WARD$OrbitOperation.OperationDepartment %in% c("13EG Kviðarhols og þvagfæra      Sími 7500",
                                                              "13G Alm                                 Sími 7360"),]

Bidlisti_for_WARD = Bidlisti_for_WARD[as.Date(Bidlisti_for_WARD$OrbitOperation.PlannedStartTime_Date) %in% RodDaga, ]


Bidlisti = Bidlisti[Bidlisti$OrbitOperation.OperationSpecialty %in% c("Hb. Alm."),]
Bidlisti = Bidlisti[Bidlisti$OrbitOperation.StatusName %in% c("Skipulagt"),]
Bidlisti = Bidlisti[as.Date(Bidlisti$OrbitOperation.PlannedStartTime_Date) %in% RodDaga, ]

#Remove surgeries planned to Kv Stofa 21
Bidlisti = Bidlisti[!Bidlisti$OrbitOperation.OperationRoom %in% c("Kv. Stofa 21"),]


#Rodum bidlistandum upp eftir sersviidi, laekni, priority af sjukling og svo skrasetningardagsetning a bidlista.
#Gert til ad numera sjuklinga sidar 

Bidlisti <- arrange(Bidlisti, Bidlisti$OrbitOperation.OperationType, Bidlisti$OrbitOperation.OperationSpecialty ,
                    Bidlisti$OrbitOperation.RequestedOperator_Name,Bidlisti$OrbitOperation.OperationPriority, 
                    Bidlisti$OrbitOperation.RegistrDay)


toc()
#----------------------------------Skrifum gögn--------------------------------------#
tic()
# skrifa í AMPL dat skrá allt sem þarf til að besta
fname = c("kvid.dat")
#Lesa söguleg gögn (sem lesa.R skrifar)
load("adkort.Rdata")


#----------------------------------Debug---------------------------------------------#
#Ef adgerdarkort finnst ekki i sogulegum gognum viljum vid henda tvi ut? Hvad er edlilegt?
#Her er thaer adgerdir sem finnast ekki i sogulegum gognum
MissingData = Bidlisti[!Bidlisti$OrbitOperation.OperationCard %in% adkort$Adgerdakort,]
#Uppfaerdur bidlisti
Bidlisti = Bidlisti[Bidlisti$OrbitOperation.OperationCard %in% adkort$Adgerdakort,]

#Lets remove this dummy ID? afhverju er thetta tharna?
Bidlisti = subset(Bidlisti, !(Bidlisti$OrbitOperation.PatientSSN %in% c('0101010109')))

#----------------------------------Foll---------------------------------------------#
#Fall sem tekur inn dagsetningu og gefur ut tolu dags teas ef hann er 
#Innan skipulagstima
DateNameByNumber <- function(day) {
  idx = which(day==dfdays$RodDaga)
  if(length(idx)>0){
    day <- dfdays$RodDagaNumer[idx]
  }
  return(day)
}

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
 # if (is.na(l) == FALSE) {
    cat(paste0('"',l,'"\n'),file=fname, sep = " ", append = TRUE)
#  }
}
cat(";", file=fname, sep="\n", append = TRUE)



# Skrifum kortin - ath thau innihalda kt og kort
Adgerdarkort = Bidlisti$OrbitOperation.OperationCard
#Kennitolur sjuklinga
Kennitala = Bidlisti$OrbitOperation.PatientSSN


tic()
cat("param Demand := \n", file = "kvid.dat", sep = "", append=TRUE)
for (i in c(1:length(Bidlisti$OrbitOperation.PatientSSN))) {
  adgek = Adgerdarkort[i]
  kt = Kennitala[i]
  lak = Laeknir[i]
  cat(paste0('"',lak,'"','\t','"',kt,'-',adgek,'" ','1'), file=fname, sep = "\n", append=TRUE)
}
cat(";", file = fname, sep = "\n", append=TRUE)
toc()


#---------------------------------DAGAR--------------------------------------------------#

#Skilgreinum mengi daga
cat("set Day := \n",file=fname, sep=" ", append=TRUE)
for (r in c(1:length(RodDaga))) {
  #Dagar <- RodDaga[r]
  cat(paste0(r,'\n'),file=fname, sep = " ", append = TRUE)
}
cat(";", file=fname, sep="\n", append = TRUE)

#Ef vid viljum prenta ut nofn daga
#cat("param DayName := \n",file=fname, sep=" ", append=TRUE)
#for (r in c(1:length(RodDaga))) {
#  dagur <- dfdays$RodDagaNumer[r]
#  dagset <- dfdays$RodDaga[r]
  #Dagar <- RodDaga[r]
#  cat(paste0(dagur,"\t",'"',dagset,'"','\n'),file=fname, sep = " ", append = TRUE)
#}
#cat(";", file=fname, sep="\n", append = TRUE)


#Merkjum helgar med binary breytum theas ef thad helgi tha 1 annars 0 
cat("param isWeekend := \n",file=fname, sep=" ", append=TRUE)
for(r in c(1:length(RodDaga))){
  Dagar <- wday(RodDaga)[r]
  #Dagsetningar <- RodDaga[r]
  cat(paste0(r),file=fname, sep = " ", append = TRUE)
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
#Thurfum lika fridaga!!

#Skrifum ut akut stofur - ekkert notad?
#cat("set aRoom := HbStofa1 , HbStofa3 , HbStofa4 , HbStofa6;\n", file=fname, append=TRUE)


#cat("\n", file=fname, append=TRUE)
#cat("set aRoom:=\n", file=fname, append=TRUE)
#cat(paste0('"',"Hb. Stofa 2",'"\n','"',"Hb. Stofa 5",'"\n'),file=fname, append=TRUE)
#cat(";\n", file=fname, append=TRUE)

Stofur = unique(Bidlisti$OrbitOperation.OperationRoom)
#Hreinsum NA
Stofur <- Stofur[!is.na(Stofur)]

#Stofur sem vid notum fyrir elektifar adgerdir og viljum plana inn
cat("set eRoom := \n",file=fname, sep=" ", append=TRUE)
for(r in Stofur){
  idx=which(r %in% c('Hb. Stofa 3','Hb. Stofa 6'))
  if(length(idx)>0){
    cat(paste0('"',r,'"\n'),file=fname, sep=" ", append=TRUE)
  }
}
cat(";", file=fname, sep="\n", append = TRUE)


cat("set aRoom := \n",file=fname, sep=" ", append=TRUE)
for(r in Stofur){
  cat(paste0('"',r,'"\n'),file=fname, sep=" ", append=TRUE)
}
cat(";", file=fname, sep="\n", append = TRUE)



cat("param T := \n",file=fname, sep=" ", append=TRUE)
for(r in c(1:length(RodDaga))){
  Dagar <- wday(RodDaga)[r]
    cat(paste0(r,'"','Hb. Stofa 3','"'),file=fname, sep = " ", append = TRUE)
  if(Dagar %in% c('2','3','4','5')){
    cat(paste0("480"), file=fname, sep="\n", append=TRUE)}
  else if (Dagar %in% c('1','7')){
    cat(" 0", file=fname, sep="\n", append=TRUE)}
  else
    cat(" 330", file=fname, sep="\n", append=TRUE)
}
for(r in c(1:length(RodDaga))){
  Dagar <- wday(RodDaga)[r]
  cat(paste0(r,'"','Hb. Stofa 6','"'),file=fname, sep = " ", append = TRUE)
  if(Dagar %in% c('2','3','4','5')){
    cat(paste0("480"), file=fname, sep="\n", append=TRUE)}
  else if (Dagar %in% c('1','7')){
    cat(" 0", file=fname, sep="\n", append=TRUE)}
  else
    cat(" 330", file=fname, sep="\n", append=TRUE)
}
cat(";", file=fname, sep="\n", append = TRUE)




#---------------------------------VAKTIR--------------------------------------------#
cat("param Roster := ",file=fname,sep="\n", append=TRUE)
for(i in c(1:length(RodDaga))){
  DagarNum <- wday(RodDaga)[i]
  Dagsetning <- RodDaga[i] 
  NumerDags <- DateNameByNumber(Dagsetning)
  #print(DagaNr)
  #print(Dagsetning)
  if(DagarNum==2){
    cat(paste0(NumerDags,'\t','"',"Tómas Jónsson",'"','\t','"',"Hb. Stofa 6",'"','\t','1\n'), file=fname, sep=" ", append=TRUE)
    cat(paste0(NumerDags,'\t','"',"Sigurður Blöndal",'"','\t','"',"Hb. Stofa 3",'"','\t','1\n'), file=fname, sep=" ", append=TRUE)
  }
  if(DagarNum==3){
    cat(paste0(NumerDags,'\t','"',"Elsa Björk Valsdóttir",'"','\t','"',"Hb. Stofa 6",'"','\t','1\n'), file=fname, sep=" ", append=TRUE)
    cat(paste0(NumerDags,'\t','"',"Kristín Huld Haraldsdóttir",'"','\t','"',"Hb. Stofa 3",'"','\t','1\n'), file=fname, sep=" ", append=TRUE)
  }
  if(DagarNum==4){
    cat(paste0(NumerDags,'\t','"',"Jórunn Atladóttir",'"','\t','"',"Hb. Stofa 6",'"','\t','1\n'), file=fname, sep=" ", append=TRUE)
    cat(paste0(NumerDags,'\t','"',"Guðjón Birgisson",'"','\t','"',"Hb. Stofa 3",'"','\t','1\n'), file=fname, sep=" ", append=TRUE)
  }
  if(DagarNum==5){
    cat(paste0(NumerDags,'\t','"',"Helgi Kjartan Sigurðsson",'"','\t','"',"Hb. Stofa 6",'"','\t','1\n'), file=fname, sep=" ", append=TRUE)
    cat(paste0(NumerDags,'\t','"',"Aðalsteinn Arnarson",'"','\t','"',"Hb. Stofa 3",'"','\t','1\n'), file=fname, sep=" ", append=TRUE)
  }
  if(DagarNum==6){
    cat(paste0(NumerDags,'\t','"',"Höskuldur Kristvinsson",'"','\t','"',"Hb. Stofa 6",'"','\t','1\n'), file=fname, sep=" ", append=TRUE)
    cat(paste0(NumerDags,'\t','"',"Páll Helgi Möller",'"','\t','"',"Hb. Stofa 3",'"','\t','1\n'), file=fname, sep=" ", append=TRUE)
  }
}
cat(";",file=fname, sep = "\n", append=TRUE)



#---------------------------------Sjuklingar og kort --------------------------------------#

# Skrifum kortin - ath thau innihalda kt og kort
Adgerdarkort = Bidlisti$OrbitOperation.OperationCard
#Kennitolur sjuklinga
Kennitala = Bidlisti$OrbitOperation.PatientSSN

cat("set eCode := \n", file=fname, sep=" ", append=TRUE)
for(a in c(1:length(Adgerdarkort))){
  kt = Kennitala[a]
  adgek = Adgerdarkort[a]
  idx = which(Bidlisti$OrbitOperation.StatusName %in% c('Skipulagt'))
  cat(paste0('"',kt,'-',adgek,'"\n'), file=fname, sep=" ", append=TRUE)
}
cat(";", file=fname, sep="\n", append = TRUE)


#Skrifum planadar adgerdir i stofur og tima utfra bidlista
Adgerdardagur <- as.Date(Bidlisti$OrbitOperation.PlannedStartTime_Date)
Stofa <- Bidlisti$OrbitOperation.OperationRoom




#cat("param Xfix := \n", file=fname, sep=" ", append=TRUE)
#for(i in c(1:length(Adgerdardagur))){
#  #viljum ekki skoda gamlar adgerdir heldur bara thad sem er yfir dagsetningunni i dag
 # idx= which(!is.na(Adgerdardagur[i]) & Adgerdardagur[i]>=StartDate & Adgerdardagur[i]<=EndDate)
  #kt=Kennitala[i]
  #adgek = Adgerdarkort[i]
  #lak = Laeknir[i]
  #Dagur = Adgerdardagur[i]
  #NumerDags = DateNameByNumber(Dagur)
  #stofa = Stofa[i]
  #if(length(idx)>0){
  #  print(kt)
  #  print(NumerDags)
  #  cat(paste0(NumerDags,'\t','"',stofa,'"','\t','"',lak,'"','\t','"',kt,'-',adgek,'"','\t'," 1",'\n'),
   #     file=fname, sep=" ", append=TRUE)
  #}
#}
#cat(";", file=fname, sep="\n", append = TRUE)


cat("param realschedule := \n", file=fname, sep=" ", append=TRUE)
for(i in c(1:length(Adgerdardagur))){
  #viljum ekki skoda gamlar adgerdir heldur bara thad sem er yfir dagsetningunni i dag
  idx= which(!is.na(Adgerdardagur[i]) & Adgerdardagur[i]>=StartDate & Adgerdardagur[i]<=EndDate)
  kt=Kennitala[i]
  adgek = Adgerdarkort[i]
  lak = Laeknir[i]
  Dagur = Adgerdardagur[i]
  NumerDags = DateNameByNumber(Dagur)
  stofa = Stofa[i]
  if(length(idx)>0){
    print(NumerDags)
    cat(paste0('"',kt,'-',adgek,'"','\t',NumerDags,'\t','"',stofa,'"','\t','"',lak,'"'," 1",'\n'),
        file=fname, sep=" ", append=TRUE)
  }
}
cat(";", file=fname, sep="\n", append = TRUE)




#---------------------------------WARD og ICU -------------------------------------------#


#Sameinum bidlista fyrir ward og icu
Legubidlisti <-  rbind(Bidlisti_for_ICU, Bidlisti_for_WARD)
#Sameinum bidlista fyrir alla
AllirBidlisti <- rbind(Bidlisti, Bidlisti_for_ICU, Bidlisti_for_WARD)


cat("set oCode := \n", file=fname, sep=" ", append=TRUE)
for(i in c(1:length(Legubidlisti$OrbitOperation.optillfalle_id))){
  kt = Legubidlisti$OrbitOperation.PatientSSN[i]
  adgek = Legubidlisti$OrbitOperation.OperationCard[i]
  cat(paste0('"',kt,'-', adgek,'"',"\n"), file=fname, sep= " ", append=TRUE)
}
cat(";", file=fname, sep="\n", append = TRUE)


cat("param OtherSurgeries:=\n", file=fname, sep=" ", append=TRUE)
for(i in c(1:length(Legubidlisti$OrbitOperation.optillfalle_id))){
  kt = Legubidlisti$OrbitOperation.PatientSSN[i]
  adgek = Legubidlisti$OrbitOperation.OperationCard[i]
  Dagur = as.Date(Legubidlisti$OrbitOperation.PlannedStartTime_Date[i])
  NumerDags = DateNameByNumber(Dagur)
  dagset <- 
    cat(paste0('"',kt,'-', adgek,'"','\t',NumerDags,"\t","1","\n"), file=fname, sep= " ", append=TRUE)
}
cat(";", file=fname, sep="\n", append = TRUE)

YfirNottAICU = AllirBidlisti$OrbitOperation.OvernightICU
cat("param gotoICU := \n", file=fname, sep=" ", append=TRUE)
for(i in c(1:length(YfirNottAICU))){
  kt = AllirBidlisti$OrbitOperation.PatientSSN[i]
  adgek = AllirBidlisti$OrbitOperation.OperationCard[i]
  icu = YfirNottAICU[i]
  cat(paste0('"',kt,'-',adgek,'"',"\t", icu,"\n"), file=fname, sep=" ", append=TRUE)
}
cat(";", file=fname, sep="\n", append = TRUE)



#Skrifum ut ef sjuklingur tharf legu ad halda eftir adgerd utfra bidlista
Lega = AllirBidlisti$OrbitOperation.PatientAdmission
cat("param gotoWard := ", file = "kvid.dat", sep = "\n", append=TRUE)
for(i in c(1:length(Lega))){
  lega = Lega[i]
  deild = AllirBidlisti$OrbitOperation.OperationDepartment[i]
  adge = AllirBidlisti$OrbitOperation.OperationCard[i]
  kt = AllirBidlisti$OrbitOperation.PatientSSN[i]
  if(lega %in% c("Legudeild") & deild %in% c("13EG Kviðarhols og þvagfæra      Sími 7500",
                                             "13G Alm                                 Sími 7360") ){
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
cat(";", file=fname, sep="\n", append = TRUE)


#Skrifum ut expectedWard fyrir hvern dag, viljum einungis tha sem eru merktir fyrir ad thurfa ward
cat("param expectedWard : ",file=fname,sep="\n", append = TRUE)
for (j in c(1:7))
  cat(sprintf(" %d ", j), file=fname, sep = "", append=TRUE)
cat(":=",file=fname,sep="\n", append=TRUE)
for (i in c(1:length(AllirBidlisti$OrbitOperation.optillfalle_id))) {
  #We must also check if the operation card exist in our data
  idx=which(Lega[i]==c("Legudeild") & Adgerdarkort[i] %in% adkort$Adgerdakort &AllirBidlisti$OrbitOperation.OperationDepartment[i] %in% c("13EG Kviðarhols og þvagfæra      Sími 7500", "13G Alm                                 Sími 7360"))
  if(length(idx)>0){
    adge=AllirBidlisti$OrbitOperation.OperationCard[i]
    kt = AllirBidlisti$OrbitOperation.PatientSSN[i]
    cat(paste0('"',kt,'-',adge,'"','\t',' 1'),file=fname ,sep="", append=TRUE)
    for (j in c(1:6))
      cat(sprintf(" %.3f ",LeguLikur[adge,j]), file=fname, sep =
            "", append=TRUE)
    cat(" ",file=fname,sep="\n", append=TRUE)
  }
}
cat(";",file=fname,sep="\n", append=TRUE)


#Skrifum ut expectedICU fyrir hvern dag, viljum einungis tha sem eru merktir fyrir ad thurfa ad fara i ICU utfra bidlista?
cat("param expectedICU : ",file=fname,sep="\n", append = TRUE)
for (j in c(1:7))
  cat(sprintf(" %d ", j), file=fname, sep = "", append=TRUE)
cat(":=",file=fname,sep="\n", append=TRUE)

for (i in c(1:length(AllirBidlisti$OrbitOperation.optillfalle_id))) {
  idx=which(YfirNottAICU[i]==c("1") & AllirBidlisti$OrbitOperation.OperationCard[i] %in% adkort$Adgerdakort)
  if(length(idx)>0){
    adge=AllirBidlisti$OrbitOperation.OperationCard[i]
    kt = AllirBidlisti$OrbitOperation.PatientSSN[i]
    cat(paste0('"',kt,'-',adge,'"','\t',' 1'),file=fname ,sep="", append=TRUE)
    #cat(sprintf(" o%03d_%s 1",i,case), file="kvid.dat", sep = "", append=TRUE)
    for (j in c(1:6))
      cat(sprintf(" %.3f ",GjorLikur[adge,j]), file="kvid.dat", sep =
            "", append=TRUE)
    cat(" ",file=fname,sep="\n", append=TRUE)
  }
}
cat(";",file=fname,sep="\n", append=TRUE)


#---------------------------------Surgery times-------------------------------------------#

#Skrifum ut fjolda scenarios
scenarios=10
cat("param numscenarios :=",scenarios,";", file = "kvid.dat", sep = "\n", append=TRUE)

#Eg set her skilyrdid ad adgerdarkortid turfi ad vera til i gognunum..
cat("param SurgeryTime := ",file="kvid.dat",sep="\n", append=TRUE)
Scenario = c(1:scenarios)
for (i in c(1:length(Adgerdarkort))){
  adge = Adgerdarkort[i] #ur bidlista
  laeknir = Laeknir[i] #ur bidlista
  kt = Kennitala[i] #ur bidlista
  idx = which(adkort$Adgerdakort==adge & adkort$Laeknir==laeknir
              & adkort$AdgerdaTimi<=24*60 & adkort$AdgerdaTimi>0 &adge %in% adkort$Adgerdakort
              & adkort$Skurdstofutimi>0)
  if(length(idx)<10){
    #Notum tima  annara ef thad finnst ekki annar 
    idx = which(adkort$Adgerdakort==adge &
                  adkort$AdgerdaTimi<=24*60 & adkort$AdgerdaTimi>0 & adge %in% adkort$Adgerdakort
                & adkort$Skurdstofutimi>0)
  }
  idx <- rev(idx)
  idx <- idx[1:min(10,length(idx))]
  for (s in Scenario){
    
    #Tokum 10 sidustu gildi
    id= idx[s]
    if(is.na(adkort$AdgerdaTimi[id])==FALSE){
      
      cat(paste0('"',kt,'-', adge,'"','\t' ,'"', laeknir,'"','\t' ,s,'\t',
                 as.numeric(adkort$Skurdstofutimi[id])), file="kvid.dat", sep = "\n",
          append=TRUE)}
    else  #nota utreiknadann medaltima fyrir adgerdarkortid ur bidlista?
      #hvad ef hann er ekki til?
      cat(paste0('"',kt,'-', adge,'"','\t' ,'"', laeknir,'"','\t' ,s,'\t',round((Bidlisti$OrbitOperation.OperationCard_OpTime[i]*60))), file="kvid.dat", sep = "\n",
          append=TRUE)
  }
}

cat(";",file="kvid.dat",sep="\n", append=TRUE)

#Post og pre-operation times


toc()
#19.62sec


#-----------------------------------Priority---------------------------------------------#
#Verdum ad setja i enska bokstafi til thess ad geta talid




uLaeknir = unique(Bidlisti$OrbitOperation.RequestedOperator_Name)
laeknar = rep(NA, length(Bidlisti$OrbitOperation.RequestedOperator_Name))
for (i in c(1:length(Bidlisti$OrbitOperation.RequestedOperator_Name))){
  tmpstr = Bidlisti$OrbitOperation.RequestedOperator_Name[i]
  nafn<- paste(strsplit(tmpstr," ")[[1]], sep="", collapse ="")
  tmp <- chartr(c('ÍÁÆÖÝÐÞÓÚÉíáæöýðþóúé-'),c('IAAOYDPOUEiaaoydpoue_'), nafn)
  laeknar[i] = tmp
}
ulaeknar <- unique(laeknar)

tic()
#Bidlisti <- Bidlisti[!is.na(Bidlisti$OrbitOperation.RequestedOperator_Name),]
for (i in unique(ulaeknar)){ 
 Bidlisti$PriorityOfPatient[laeknar == i] <- seq_len(sum(laeknar == i))
}
toc()


cat("param Priority := \n", file=fname, sep=" ", append=TRUE)
for(i in c(1:length(Adgerdarkort))){
  adge = Adgerdarkort[i] #ur bidlista
  kt = Kennitala[i] #ur bidlista
  pri = Bidlisti$PriorityOfPatient[i]
  cat(paste0('"',kt,'-',adge,'"', '\t', pri,'\n'), file=fname, sep=" ", append=TRUE) 
  }
cat(";", file=fname, sep="\n", append = TRUE)

#cat("param Priority2 := \n", file=fname, sep=" ", append=TRUE)
#for(i in c(1:length(Adgerdarkort))){
#adge = Adgerdarkort[i] #ur bidlista
#kt = Kennitala[i] #ur bidlista
#pri = Bidlisti$OrbitOperation.OperationPriority
#idx= which(pri %in% c('"2. Fjórar vikur, brýn þörf"'))
#if(length(idx)>0){
#cat(paste0('"',kt,'-',adge,'"', '\t', '1','\n'), file=fname, sep=" ", append=TRUE) 
#}
#else
#  cat(paste0('"',kt,'-',adge,'"', '\t', '10','\n'), file=fname, sep=" ", append=TRUE) 
#}
#cat(";", file=fname, sep="\n", append = TRUE)




