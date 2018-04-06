# Les Excel skjöl og mynda data.frame sem er notað í greiningu
require(openxlsx)
require(lubridate) # https://www.r-statistics.com/2012/03/do-more-with-dates-and-times-in-r-with-lubridate-1-1-0/

setwd("~/projects/betrirodun")
# Hvar eru skjölin og hvað heita þau?
filepath = c("./gogn/")
# Eyða "bilum" út nafni, nota hér "_" 
filename = c("_með_aðgerðakortum.xlsx")

# Lestum eitt ár í einu:
ORBIT = NULL
for (ar in seq(2009,2009)) {
  fname = paste0(filepath,as.character(ar),filename, sep="")
  print(fname)
  O <- read.xlsx(fname, sheet = "Skurðaðgerðir - ORBIT", startRow = 2, colNames = TRUE)
  O <- O[-nrow(O),]                                     # remove last row, its not data
  ORBIT = rbind(ORBIT,O)
}
rm(list = c("O", "fname", "filepath", "filename"))

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
  AdgerdLykur <- Dagsetning + hm(ORBIT$Aðgerð.lýkur[i], quiet = TRUE)
  SvaefingLykur <- Dagsetning + hm(ORBIT$Svæfingu.lýkur[i], quiet = TRUE)
  UtAfSkurdgangi <- Dagsetning + hm(ORBIT$Út.af.skurðgangi[i], quiet = TRUE)
  InnAVoknun <- Dagsetning + hm(ORBIT$Inn.á.vöknun[i], quiet = TRUE)
  UtAfVoknun <- Dagsetning + hm(ORBIT$Út.af.vöknun[i], quiet = TRUE)
  adkort[[a]] = data.frame(Dagsetning, 
                            InnASkurdgang,InnAStofu,SvaefingHefst, 
                            AdgerdHefst,AdgerdLykur,SvaefingLykur,
                            UtAfSkurdgangi,InnAVoknun,UtAfVoknun)
}
  

