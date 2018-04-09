# Les Excel skjöl og mynda data.frame sem er notað í greiningu
require(openxlsx)
require(lubridate) # https://www.r-statistics.com/2012/03/do-more-with-dates-and-times-in-r-with-lubridate-1-1-0/

# User defined function for correcting times that run after midnight
midnightrun <- function(starttime, endtime) {
  idx <- (starttime > endtime)
  idx[is.na(endtime)] <- FALSE
  endtime[idx] <- endtime[idx] + hms('24:00:00')
  return(endtime)
}



setwd("~/projects/betrirodun")
# Hvar eru skjölin og hvað heita þau?
filepath = c("./gogn/")
# Eyða "bilum" út nafni, nota hér "_" 
filename = c("_með_aðgerðakortum.xlsx")

# Lestum eitt ár í einu:
ORBIT = NULL
SAGALEGUDEILD = NULL
SAGAGJORGAESLA = NULL

for (ar in seq(2015,2015)) {
  fname = paste0(filepath,as.character(ar),filename, sep="")
  print(fname)

  O <- read.xlsx(fname, sheet = "Skurðaðgerðir - ORBIT", startRow = 2, colNames = TRUE)
  O <- O[-nrow(O),]                                     # remove last row, its not data
  ORBIT = rbind(ORBIT,O)
  
  S <- read.xlsx(fname, sheet = "Legudeild - SAGA", startRow = 2, colNames = TRUE)
  S <- S[-nrow(S),]                                     # remove last row, its not data
  SAGALEGUDEILD = rbind(SAGALEGUDEILD,S)
  
  G <- read.xlsx(fname, sheet = "Gjörgæsla - SAGA", startRow = 2, colNames = TRUE)
  G <- G[-nrow(G),]                                     # remove last row, its not data
  SAGAGJORGAESLA = rbind(SAGAGJORGAESLA,G)
  
}
rm(list = c("O", "S", "G", "fname", "filepath", "filename"))

# Finna einkvæmt aðgerðakort
adgerdakort = unique(ORBIT$Aðgerðarkort)

# Varpa N/A í nan
for (n in names(ORBIT)) {
  i = (ORBIT[n] == "N/A")
  ORBIT[n][i] = NaN
}

# Búum til data frame fyrir hvert kort með upplýsingum ...
adkort = list()
for (a in adgerdakort) {
  i <- which(ORBIT$Aðgerðarkort == a)
  Dagsetning <- ymd(convertToDateTime(ORBIT$Dagsetning.aðgerðar[i]))
  InnASkurdgang <- Dagsetning + hm(ORBIT$Inn.á.skurðgang[i], quiet = TRUE)
  InnAStofu <- Dagsetning + hm(ORBIT$Inn.á.stofu[i], quiet = TRUE)
  SvaefingHefst <- Dagsetning + hm(ORBIT$Svæfing.hefst[i], quiet = TRUE)
  AdgerdHefst <- Dagsetning + hm(ORBIT$Aðgerð.hefst[i], quiet = TRUE)
  # Aðgerð getur farið yfir á næsta dag ... notum midnightrun til að athuga það og laga
  AdgerdLykur <- midnightrun(AdgerdHefst, Dagsetning + hm(ORBIT$Aðgerð.lýkur[i], quiet = TRUE))
  InnAVoknun <- midnightrun(AdgerdHefst, Dagsetning + hm(ORBIT$Inn.á.vöknun[i], quiet = TRUE))
  SvaefingLykur <- midnightrun(AdgerdHefst,Dagsetning + hm(ORBIT$Svæfingu.lýkur[i], quiet = TRUE))
  UtAfVoknun <- midnightrun(AdgerdHefst,Dagsetning + hm(ORBIT$Út.af.vöknun[i], quiet = TRUE))
  UtAfSkurdgangi <- midnightrun(AdgerdHefst,Dagsetning + hm(ORBIT$Út.af.skurðgangi[i], quiet = TRUE))
  
  # Eiginleikar viðkomandi
  ASA <- ORBIT$ASA.flokkun[i]
  Age <- ORBIT$Age.at.operation[i]
  
  # Teymi tengt ...
  Laeknir <- NULL
  
  # Legutengt ...
  LeguNumer <- ORBIT$`Legu-.eða.komunúmer`[i]
  # Athuga hvort viðkomandi haf farið á legudeild, legu númer 
  LeguInnritunartimi = as_datetime(rep(NaN, length(LeguNumer)))
  LeguUtskriftartimi = as_datetime(rep(NaN, length(LeguNumer)))
  for (k in seq(1,length(LeguNumer))) {
    i = which(SAGALEGUDEILD$Legunúmer == LeguNumer[k])
    if (length(i) == 1) {  # default er NaN
      LeguInnritunartimi[k] =  ymd(convertToDateTime(SAGALEGUDEILD$Dagsetning.innskriftar[i]))
      LeguInnritunartimi[k] = LeguInnritunartimi[k] + hm(SAGALEGUDEILD$`Innritunar-tími`[i])
      LeguUtskriftartimi[k] =  ymd(convertToDateTime(SAGALEGUDEILD$Dagsetning.útskriftar[i]))
      LeguUtskriftartimi[k] = LeguUtskriftartimi[k] + hm(SAGALEGUDEILD$`Útskriftar-tími`[i])
    }
    else if (length(i) > 1) { # villutékk
      warning("legunúmer we ekki einkvæmt!")
      print(SAGALEGUDEILD$Dagsetning.innskriftar[i])
    }
  }
  LeguDagar = as.numeric(difftime(LeguUtskriftartimi,LeguInnritunartimi,units = "days"))
  
  # Gjörgæslutengt ...
  
  adkort[[a]] = data.frame(Dagsetning, 
                           InnASkurdgang,InnAStofu,SvaefingHefst, 
                           AdgerdHefst,AdgerdLykur,SvaefingLykur,
                           UtAfSkurdgangi,InnAVoknun,UtAfVoknun, LeguUtskriftartimi, LeguInnritunartimi, Legudagar)
  
  
}
  

