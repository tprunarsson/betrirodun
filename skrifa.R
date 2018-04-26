#--- núllstilla og pakkar ---#

rm(list=ls())
options(encoding = "UTF-8") 


library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(tictoc)
lubridate::force_tz(Sys.time(), tzone = "UCT")
lubridate::with_tz(Sys.time(), tzone = "UCT")


#---------------------------------Biðlistaþjónusta----------------------------------------#
tic()

# Þangað til að þjónustan virkar
load("Stuff.Rdata")

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


# Búum til runu af dagsetningum - user defined.. 
numberOfDays = 15
# StartDate = as.Date(Sys.Date())
StartDate = ymd(c("2018-04-30"),tz = "GMT")
RodDagaStr = StartDate
for (i in c(1:numberOfDays)) {
  RodDagaStr = c(RodDagaStr, tail(RodDagaStr, n=1) + hours(24))
}
RodDaga<- seq(yday(StartDate), by=1, length.out = numberOfDays)
RodDagaNumer <- seq(1, by=1, length.out = numberOfDays)
dfdays <- data.frame(RodDaga, RodDagaNumer)
StartDate <- yday(StartDate)
EndDate = max(RodDaga)

# Búum til merki sem segir til um hvort um elective alm eða annað
Merki <- rep(NA,nrow(Bidlisti))

# Veljum aðeins úr biðlista það sem er á hringbraut er á Almennu, notar gjörgæslu og sameiginlegt legupláss

# Tökum það sem er skipulagt á hringbraut! 
idx1 <- substr(Bidlisti$OrbitOperation.OperationSpecialty, start = 1, stop = 3) %in% c("Hb.","Kve","Þja") &
        yday(Bidlisti$OrbitOperation.PlannedStartTime_Date) %in% RodDaga &
        Bidlisti$OrbitOperation.StatusName %in% c("Skipulagt")
Merki[idx1] <- 1

# Aðgerðir með dagsetningu og þurfa gjörgæslu
idx2 <- Bidlisti$OrbitOperation.OvernightICU %in% c("1") & 
        yday(Bidlisti$OrbitOperation.PlannedStartTime_Date) %in% RodDaga
Merki[idx2] <- 2

# Aðgerður sem eru með legu á samastað og Almenna...
idx3 <- Bidlisti$OrbitOperation.OperationDepartment %in% c("13EG Kviðarhols og þvagfæra      Sími 7500",
                                                            "13G Alm                                 Sími 7360") &
        yday(Bidlisti$OrbitOperation.PlannedStartTime_Date) %in% RodDaga & 
        !(Bidlisti$OrbitOperation.OperationSpecialty %in% c("Hb. Alm."))
Merki[idx3] <- 3

# Valaðgerðir sem við ætlum að raða, allar aðgerðir á biðlista eru valaðgerðir ?!
idx4 <- Bidlisti$OrbitOperation.OperationSpecialty %in% c("Hb. Alm.") &
        Bidlisti$OrbitOperation.OperationType %in% c("Valaðgerð") &
        !is.na(Bidlisti$OrbitOperation.RequestedOperator_Name)
Merki[idx4] <- 4

# Tökum þetta allt saman
Bidlisti$Merki <- Merki
idx = idx1 | idx2 | idx3 | idx4
Bidlisti <- Bidlisti[idx,]

# DEBUG find OR that does not have a card
ii = which(is.na(Bidlisti$OrbitOperation.OperationCard))
if (length(ii)> 0) {
  print("removing these patients from the list, they don't have a OR card!")
  print(t(Bidlisti[ii,]))
  Bidlisti <- Bidlisti[is.na(Bidlisti$OrbitOperation.OperationCard)== FALSE,]
}



toc()
#----------------------------------Skrifum gögn--------------------------------------#
tic()
# skrifa í AMPL dat skrá allt sem þarf til að besta
fname = c("kvid.dat")
# Lesa söguleg gögn (sem lesa.R skrifar)
load("adkort.Rdata")
# remove funny surgery times
adkort <- adkort[adkort$Skurdstofutimi > 0 & adkort$Skurdstofutimi <= 480,]


#----------------------------------Debug---------------------------------------------#
#Ef adgerdarkort finnst ekki i sogulegum gognum viljum vid henda tvi ut? Hvad er edlilegt?
#Her er thaer adgerdir sem finnast ekki i sogulegum gognum
#MissingData = Bidlisti[!Bidlisti$OrbitOperation.OperationCard %in% adkort$Adgerdakort,]
#Uppfaerdur bidlisti
#Bidlisti = Bidlisti[Bidlisti$OrbitOperation.OperationCard %in% adkort$Adgerdakort,]

#Lets remove this dummy ID? afhverju er thetta tharna?
#Bidlisti = subset(Bidlisti, !(Bidlisti$OrbitOperation.PatientSSN %in% c('0101010109')))

#--------------------------------LAEKNAR-----------------------------------------------#
# Skrifum niður lækna, rSurgeon eru þeir með fasta daga
cat("set Surgeon := \n",file=fname, sep=" ")
Laeknir = unique(Bidlisti$OrbitOperation.RequestedOperator_Name[Bidlisti$Merki == 4])
for (l in Laeknir) {
  if (is.na(l) == FALSE) {
    cat(paste0('"',l,'"\n'),file=fname, sep = " ", append = TRUE)
  }
}
cat(";", file=fname, sep="\n", append = TRUE)

tic()
cat("param Demand := \n", file = fname, sep = "", append=TRUE)
for (i in c(1:nrow(Bidlisti))) {
  if (Bidlisti$Merki[i] == 4) {
    adgek = Bidlisti$OrbitOperation.OperationCard[i]
    kt = Bidlisti$OrbitOperation.PatientSSN[i]
    lak = Bidlisti$OrbitOperation.RequestedOperator_Name[i]
    cat(paste0('"',lak,'"','\t','"',kt,'-',adgek,'" ','1'), file=fname, sep = "\n", append=TRUE)
  }
}
cat(";", file = fname, sep = "\n", append=TRUE)
toc()


#---------------------------------DAGAR--------------------------------------------------#

# Skilgreinum mengi daga og heiti
cat("set Day := \n",file=fname, sep=" ", append=TRUE)
for (r in c(1:length(RodDaga))) {
  cat(paste0(r,'\n'),file=fname, sep = " ", append = TRUE)
}
cat(";", file=fname, sep="\n", append = TRUE)
cat("param DayName := \n",file=fname, sep=" ", append=TRUE)
for (r in c(1:length(RodDaga))) {
  dagur <- dfdays$RodDagaNumer[r]
  dagset <- RodDagaStr[r]
  cat(paste0(dagur,"\t",'"',dagset,'"','\n'),file=fname, sep = " ", append = TRUE)
}
cat(";", file=fname, sep="\n", append = TRUE)

# Merkjum helgar med binary breytum theas ef thad helgi tha 1 annars 0 
cat("param isWeekend := \n",file=fname, sep=" ", append=TRUE)
for(r in c(1:length(RodDaga))){
  Dagar <- wday(RodDagaStr)[r]
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

Stofur = unique(Bidlisti$OrbitOperation.OperationRoom[Bidlisti$Merki == 4])
# Hreinsum út NA
Stofur <- Stofur[!is.na(Stofur)]

#Stofur sem vid notum fyrir elektifar adgerdir og viljum plana inn
cat("set eRoom := \n",file=fname, sep=" ", append=TRUE)
for(r in Stofur){
  idx = which(r %in% c('Hb. Stofa 3','Hb. Stofa 6'))
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
  Dagar <- wday(RodDagaStr)[r]
    cat(paste0(r,'\t','"','Hb. Stofa 3','"'),file=fname, sep = " ", append = TRUE)
  if(Dagar %in% c('2','3','4','5')){
    cat(paste0(" 480"), file=fname, sep="\n", append=TRUE)}
  else if (Dagar %in% c('1','7')){
    cat(" 0", file=fname, sep="\n", append=TRUE)}
  else
    cat(" 330", file=fname, sep="\n", append=TRUE)
}
for(r in c(1:length(RodDaga))){
  Dagar <- wday(RodDagaStr)[r]
  cat(paste0(r,'\t','"','Hb. Stofa 6','"'),file=fname, sep = " ", append = TRUE)
  if(Dagar %in% c('2','3','4','5')){
    cat(paste0(" 480"), file=fname, sep="\n", append=TRUE)}
  else if (Dagar %in% c('1','7')){
    cat(" 0", file=fname, sep="\n", append=TRUE)}
  else
    cat(" 330", file=fname, sep="\n", append=TRUE)
}
cat(";", file=fname, sep="\n", append = TRUE)


#---------------------------------VAKTIR--------------------------------------------#
cat("set rSurgeon := ",file=fname,sep="\n", append=TRUE)
cat('"Tómas Jónsson"', file=fname, sep="\n", append=TRUE)
cat('"Sigurður Blöndal"', file=fname, sep="\n", append=TRUE)
cat('"Elsa Björk Valsdóttir"', file=fname, sep="\n", append=TRUE)
cat('"Kristín Huld Haraldsdóttir"', file=fname, sep="\n", append=TRUE)
cat('"Jórunn Atladóttir"', file=fname, sep="\n", append=TRUE)
cat('"Guðjón Birgisson"', file=fname, sep="\n", append=TRUE)
cat('"Helgi Kjartan Sigurðsson"', file=fname, sep="\n", append=TRUE)
cat('"Aðalsteinn Arnarson"', file=fname, sep="\n", append=TRUE)
cat('"Höskuldur Kristvinsson"', file=fname, sep="\n", append=TRUE)
cat('"Páll Helgi Möller"', file=fname, sep="\n", append=TRUE)
cat(";",file=fname, sep = "\n", append=TRUE)

cat("param Roster := ",file=fname,sep="\n", append=TRUE)
for(i in c(1:length(RodDaga))){
  DagarNum <- wday(RodDagaStr)[i]
  Dagsetning <- RodDaga[i]
  idxx = which(Dagsetning==dfdays$RodDaga)
  if(length(idxx)>0){
    NumerDags <- as.character(dfdays$RodDagaNumer[idxx])
  }
  else {
    stop("dags error")
  }
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

cat("set eCode := \n", file=fname, sep=" ", append=TRUE)
for(i in c(1:nrow(Bidlisti))){
  if (Bidlisti$Merki[i] == 4) {
    kt = Bidlisti$OrbitOperation.PatientSSN[i]
    adgek = Bidlisti$OrbitOperation.OperationCard[i]
    idx = which(Bidlisti$OrbitOperation.StatusName %in% c('Skipulagt'))
    cat(paste0('"',kt,'-',adgek,'"\n'), file=fname, sep=" ", append=TRUE)
  }
}
cat(";", file=fname, sep="\n", append = TRUE)
cat("set oCode := \n", file=fname, sep=" ", append=TRUE)
for(i in c(1:nrow(Bidlisti))){
  if ((Bidlisti$Merki[i] == 2) | (Bidlisti$Merki[i] == 3)) {
    kt = Bidlisti$OrbitOperation.PatientSSN[i]
    adgek = Bidlisti$OrbitOperation.OperationCard[i]
    cat(paste0('"',kt,'-', adgek,'"',"\n"), file=fname, sep= " ", append=TRUE)
  }
}
cat(";", file=fname, sep="\n", append = TRUE)

# Skrifum planadar adgerdir i stofur og tima utfra bidlista

# Festum nidur adgerdadaga
cat("param plannedSurgery := \n", file=fname, sep=" ", append=TRUE)
for(i in c(1:nrow(Bidlisti))){
  if (Bidlisti$Merki[i] == 4) {
    # viljum ekki skoda gamlar adgerdir heldur bara thad sem er yfir dagsetningunni i dag
    idx = which(!is.na(Bidlisti$OrbitOperation.PlannedStartTime_Date[i]) & yday(Bidlisti$OrbitOperation.PlannedStartTime_Date[i])>=StartDate & yday(Bidlisti$OrbitOperation.PlannedStartTime_Date[i])<=EndDate)
    if(length(idx)>0) {
      kt = Bidlisti$OrbitOperation.PatientSSN[i]
      adgek = Bidlisti$OrbitOperation.OperationCard[i]
      lak = Bidlisti$OrbitOperation.RequestedOperator_Name[i]
      Dagur = yday(Bidlisti$OrbitOperation.PlannedStartTime_Date[i])
      idxx = which(Dagur==dfdays$RodDaga)
      if(length(idxx)>0){
        NumerDags <- dfdays$RodDagaNumer[idxx]
      }
      else {
        stop("day error")
      }
      stofa = Bidlisti$OrbitOperation.OperationRoom[i]
      print(NumerDags)
      cat(paste0(NumerDags,'\t','"',kt,'-',adgek,'"','\t'," 1",'\n'),
          file=fname, sep=" ", append=TRUE)
    }
  }
}
cat(";", file=fname, sep="\n", append = TRUE)


#---------------------------------WARD og ICU -------------------------------------------#


cat("param OtherSurgeries:=\n", file=fname, sep=" ", append=TRUE)
for(i in c(1:nrow(Bidlisti))){
  if ((Bidlisti$Merki[i] == 2) | (Bidlisti$Merki[i] == 3)) {
    kt = Bidlisti$OrbitOperation.PatientSSN[i]
    adgek = Bidlisti$OrbitOperation.OperationCard[i]
    Dagur = yday(Bidlisti$OrbitOperation.PlannedStartTime_Date[i])
    idxx = which(Dagur==dfdays$RodDaga)
    if(length(idxx)>0){
      NumerDags <- dfdays$RodDagaNumer[idxx]
      cat(paste0(NumerDags,'\t','"',kt,'-', adgek,'"','\t',"\t","1","\n"), file=fname, sep= " ", append=TRUE)
    }
    #else {
    #  stop("day error 2")
    #}
  }
}
cat(";", file=fname, sep="\n", append = TRUE)


cat("param gotoICU := \n", file=fname, sep=" ", append=TRUE)
for (i in c(1:nrow(Bidlisti))) {
  icu = Bidlisti$OrbitOperation.OvernightICU[i]
  if (icu == 1) {
    kt = Bidlisti$OrbitOperation.PatientSSN[i]
    adgek = Bidlisti$OrbitOperation.OperationCard[i]
    cat(paste0('"',kt,'-',adgek,'"',"\t", icu,"\n"), file=fname, sep=" ", append=TRUE)
  }
}
cat(";", file=fname, sep="\n", append = TRUE)

#Skrifum ut ef sjuklingur tharf legu ad halda eftir adgerd utfra bidlista
cat("param gotoWard := ", file = fname, sep = "\n", append=TRUE)
for (i in c(1:nrow(Bidlisti))) {
  lega = Bidlisti$OrbitOperation.PatientAdmission[i]
  deild = Bidlisti$OrbitOperation.OperationDepartment[i]
  adge = Bidlisti$OrbitOperation.OperationCard[i]
  kt = Bidlisti$OrbitOperation.PatientSSN[i]
  if (lega %in% c("Legudeild") & deild %in% c("13EG Kviðarhols og þvagfæra      Sími 7500",
                                             "13G Alm                                 Sími 7360") ){
    cat(paste0('"',kt,'-',adge,'"',' 1\n'), file=fname, append=TRUE)
  }
  else if (lega %in% c("Dagdeild")){
#    cat(paste0('"',kt,'-',adge,'"',' 0\n'), file=fname, append=TRUE)
  }
  else{
    #Thetta tilfelli veit eg ekki hvernig a ad tulka NaN fyrir hvort lega eda ekki?
    #Thurfum ad komast ad tvi
    print("NaN í legu?")
 #   cat(paste0('"',kt,'-',adge,'"',' 0\n'), file=fname, append=TRUE)
  }
}
cat(";", file=fname, sep="\n", append = TRUE)


#Skrifum ut expectedWard fyrir hvern dag, viljum einungis tha sem eru merktir fyrir ad thurfa ward
cat("param expectedWard : ",file=fname,sep="\n", append = TRUE)
for (j in c(1:7))
  cat(sprintf(" %d ", j), file=fname, sep = "", append=TRUE)
cat(":=",file=fname,sep="\n", append=TRUE)
for (i in c(1:nrow(Bidlisti))) {
  if (Bidlisti$Merki[i] > 1) {
    #We must also check if the operation card exist in our data
    idx = which(Bidlisti$OrbitOperation.PatientAdmission[i]==c("Legudeild") & Bidlisti$OrbitOperation.OperationCard[i] %in% adkort$Adgerdakort) 
    # & Bidlisti$OrbitOperation.OperationDepartment[i] %in% c("13EG Kviðarhols og þvagfæra      Sími 7500", "13G Alm                                 Sími 7360"))
    if(length(idx)>0){
      adge = Bidlisti$OrbitOperation.OperationCard[i]
      kt = Bidlisti$OrbitOperation.PatientSSN[i]
      cat(paste0('"',kt,'-',adge,'"','\t',' 1'),file=fname ,sep="", append=TRUE)
      for (j in c(1:6))
        cat(sprintf(" %.3f ",LeguLikur[adge,j]), file=fname, sep = "", append=TRUE)
      cat(" ",file=fname,sep="\n", append=TRUE)
    }
    else {
      adge = Bidlisti$OrbitOperation.OperationCard[i]
      kt = Bidlisti$OrbitOperation.PatientSSN[i]
      cat(paste0('"',kt,'-',adge,'"','\t',' 1'),file=fname ,sep="", append=TRUE)
      for (j in c(1:6))
        cat(sprintf(" %.3f ",0.5^j), file=fname, sep = "", append=TRUE)
      cat(" ",file=fname,sep="\n", append=TRUE)
    }
  }
}
cat(";",file=fname,sep="\n", append=TRUE)


#Skrifum ut expectedICU fyrir hvern dag, viljum einungis tha sem eru merktir fyrir ad thurfa ad fara i ICU utfra bidlista?
cat("param expectedICU : ",file=fname,sep="\n", append = TRUE)
for (j in c(1:7))
  cat(sprintf(" %d ", j), file=fname, sep = "", append=TRUE)
cat(":=",file=fname,sep="\n", append=TRUE)

for (i in c(1:nrow(Bidlisti))) {
  if (Bidlisti$Merki[i] > 1) {
    idx = which(Bidlisti$OrbitOperation.OvernightICU[i]==c("1") & Bidlisti$OrbitOperation.OperationCard[i] %in% adkort$Adgerdakort)
    if(length(idx)>0){
      adge=Bidlisti$OrbitOperation.OperationCard[i]
      kt = Bidlisti$OrbitOperation.PatientSSN[i]
      cat(paste0('"',kt,'-',adge,'"','\t',' 1'),file=fname ,sep="", append=TRUE)
      #cat(sprintf(" o%03d_%s 1",i,case), file=fname, sep = "", append=TRUE)
      for (j in c(1:6))
        cat(sprintf(" %.3f ",GjorLikur[adge,j]), file=fname, sep =
              "", append=TRUE)
      cat(" ",file=fname,sep="\n", append=TRUE)
    }
    else {
      adge=Bidlisti$OrbitOperation.OperationCard[i]
      kt = Bidlisti$OrbitOperation.PatientSSN[i]
      cat(paste0('"',kt,'-',adge,'"','\t',' 1'),file=fname ,sep="", append=TRUE)
      #cat(sprintf(" o%03d_%s 1",i,case), file=fname, sep = "", append=TRUE)
      for (j in c(1:6))
        cat(sprintf(" %.3f ",0), file=fname, sep =
              "", append=TRUE)
      cat(" ",file=fname,sep="\n", append=TRUE)
    }
  }
}
cat(";",file=fname,sep="\n", append=TRUE)


#---------------------------------Surgery times-------------------------------------------#

#Skrifum ut fjolda scenarios
scenarios=10
cat("param numscenarios :=",scenarios,";", file = fname, sep = "\n", append=TRUE)

#Eg set her skilyrdid ad adgerdarkortid turfi ad vera til i gognunum..
cat("param SurgeryTime := ",file=fname,sep="\n", append=TRUE)
Scenario = c(1:scenarios)
for (i in c(1:nrow(Bidlisti))){
  if (Bidlisti$Merki[i] == 4) {
    adge = Bidlisti$OrbitOperation.OperationCard[i] #ur bidlista
    laeknir = Bidlisti$OrbitOperation.RequestedOperator_Name[i] #ur bidlista
    kt = Bidlisti$OrbitOperation.PatientSSN[i] #ur bidlista
    idx = which(adkort$Adgerdakort==adge & adkort$Laeknir==laeknir
              & adkort$AdgerdaTimi<=24*60 & adkort$AdgerdaTimi>0 &adge %in% adkort$Adgerdakort
              & adkort$Skurdstofutimi>0)
    if(length(idx)<10){
      # Notum tima  annara ef thad finnst ekki annar 
      idx = which(adkort$Adgerdakort==adge &
                  adkort$AdgerdaTimi<=24*60 & adkort$AdgerdaTimi>0 & adge %in% adkort$Adgerdakort
                & adkort$Skurdstofutimi>0)
    }
    idx <- rev(idx)
    idx <- idx[1:min(10,length(idx))]
    for (s in Scenario){
    
      #Tokum 10 sidustu gildi
      id = idx[s]
      if (is.na(adkort$AdgerdaTimi[id])==FALSE){
      
        cat(paste0('"',kt,'-', adge,'"','\t' ,'"', laeknir,'"','\t' ,s,'\t',
                   as.numeric(adkort$Skurdstofutimi[id])), file=fname, sep = "\n",append=TRUE)}
      else  #nota utreiknadann medaltima fyrir adgerdarkortid ur bidlista?
        #hvad ef hann er ekki til?
        cat(paste0('"',kt,'-', adge,'"','\t' ,'"', laeknir,'"','\t' ,s,'\t',round((Bidlisti$OrbitOperation.OperationCard_OpTime[i]*60))), file=fname, sep = "\n", append=TRUE)
    }
  }
}

cat(";",file=fname,sep="\n", append=TRUE)

#-----------------------------------Priority---------------------------------------------#

cat("param Priority := \n", file=fname, sep=" ", append=TRUE)
for(i in c(1:nrow(Bidlisti))) {
  if (Bidlisti$Merki[i] == 4) {
    adge = Bidlisti$OrbitOperation.OperationCard[i] #ur bidlista
    kt = Bidlisti$OrbitOperation.PatientSSN[i] #ur bidlista
#  [1] "2. Viðbót, bráða"                  "3. Þrír mán, þörf"                 "2. Fjórar vikur, brýn þörf"       
#  [4] "6. Flýting (hjarta-inniliggjandi)" "1. Ein vika, mjög brýn þörf"       "5. Ekki flýting (hjarta)"         
#  [7] NA                                  "4. Flýting (hjarta)"              
    if (Bidlisti$OrbitOperation.OperationPriority[i] %in% c("1. Ein vika, mjög brýn þörf", "2. Viðbót, bráða", "6. Flýting (hjarta-inniliggjandi)", "4. Flýting (hjarta)"))
      pri = 100
    else if (Bidlisti$OrbitOperation.OperationPriority[i] %in% c("2. Fjórar vikur, brýn þörf", "5. Ekki flýting (hjarta)"))
      pri = 50
    else if (Bidlisti$OrbitOperation.OperationPriority[i] %in% c("3. Þrír mán, þörf"))
      pri = 1
    else if (is.na(Bidlisti$OrbitOperation.OperationPriority[i]) == TRUE)
      pri = 0
    else
      stop("unknown priority")

    cat(paste0('"',kt,'-',adge,'"', '\t', pri,'\n'), file=fname, sep=" ", append=TRUE) 
  }
}
cat(";", file=fname, sep="\n", append = TRUE)

