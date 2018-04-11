library(httr)
library(jsonlite)

# Lesa söguleg gögn (sem lesa.R skrifar)
load(file="adkort.Rdata")

# Lesa biðlista (nota þjónustu seinna)
get_base_parsed <- fromJSON(txt="getWaitingList.json", flatten = TRUE)
get_base_df <- as.data.frame(get_base_parsed$OrbitWaitingList, stringsAsFactors = TRUE)

# Endurskilgreina tegundir
get_base_df$OrbitOperation.RegistrDay <- as.POSIXlt(get_base_df$OrbitOperation.RegistrDay, tz="gmt")
get_base_df$OrbitOperation.PlannedStartTime_Date <- as.POSIXct(get_base_df$OrbitOperation.PlannedStartTime_Date, tz="gmt")
get_base_df$OrbitOperation.PlannedFinishTime_Date <- as.POSIXct(get_base_df$OrbitOperation.PlannedFinishTime_Date, tz="gmt")
get_base_df$OrbitOperation.OperationCard_OpTime <- as.numeric(get_base_df$OrbitOperation.OperationCard_OpTime)
get_base_df$OrbitOperation.OperationCard_PostTime <- as.numeric(get_base_df$OrbitOperation.OperationCard_PostTime)
get_base_df$OrbitOperation.OperationCard_PreTime <- as.numeric(get_base_df$OrbitOperation.OperationCard_PreTime)

# Velja sérsvið
Bidlisti = get_base_df[get_base_df$OrbitOperation.OperationSpecialty %in% c("Hb. Alm.", "Hb. Þvagf."),]

# skrifa í AMPL dat skrá allt sem þarf til að besta
fname = c("kvid.dat")

# Skrifum niður lækna
cat("set rSurgeon := ",file=fname, sep=" ")
Laeknir = unique(Bidlisti$OrbitOperation.RequestedOperator_Name)
for (l in Laeknir) {
  if (is.na(l) == FALSE) {
    cat(paste0('"',l,'" '),file=fname, sep = " ", append = TRUE)
  }
}
cat(";", file=fname, sep="\n", append = TRUE)

# Skrifum niður kortin
Adgerdarkort = Bidlisti$OrbitOperation.OperationCard

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
#OLD cat(";",file="kvid.dat", sep = "\n", append=TRUE)
