# Les Excel skjöl og mynda data.frame sem er notað í greiningu
require(openxlsx)
require(lubridate) # https://www.r-statistics.com/2012/03/do-more-with-dates-and-times-in-r-with-lubridate-1-1-0/

options(expressions = 5e5)
rm(list=ls())

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
STARFSMENN = NULL
BIDLISTI = NULL

for (ar in seq(2009,2018)) {
  
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
  
  M <- read.xlsx(fname, sheet = "Skurðaðgerðir - starfsmenn", startRow = 2, colNames = TRUE)
  M <- M[-nrow(M),] # remove last row, its not data
  STARFSMENN <- rbind(STARFSMENN,M)
  
  B <- read.xlsx(fname, sheet = "Biðlistar", startRow = 5, colNames = TRUE)
  B <- B[-nrow(B),] # remove last row, its not data
  BIDLISTI <- rbind(BIDLISTI,B)
  
}

rm(list = c("O", "S", "G", "M", "B", "fname", "filepath", "filename"))

# Varpa N/A í nan
for (n in names(ORBIT)) {
  i = (ORBIT[n] == "N/A")
  ORBIT[n][i] = NaN
}

# Búum til data frame fyrir hvert kort með upplýsingum ...

# Eiginleikar sem tengjast aðgerð
Adgerdakort <- ORBIT$Aðgerðarkort
AcuteElective <- ORBIT$`Acute.-.elective`
ID <- ORBIT$Kennitala
Dagsetning <- ymd(convertToDateTime(ORBIT$Dagsetning.aðgerðar))
InnASkurdgang <- Dagsetning + hm(ORBIT$Inn.á.skurðgang, quiet = TRUE)
InnAStofu <- Dagsetning + hm(ORBIT$Inn.á.stofu, quiet = TRUE)
SvaefingHefst <- Dagsetning + hm(ORBIT$Svæfing.hefst, quiet = TRUE)
AdgerdHefst <- Dagsetning + hm(ORBIT$Aðgerð.hefst, quiet = TRUE)
# Aðgerð getur farið yfir á næsta dag ... notum midnightrun til að athuga það og laga
AdgerdLykur <- midnightrun(AdgerdHefst, Dagsetning + hm(ORBIT$Aðgerð.lýkur, quiet = TRUE))
InnAVoknun <- midnightrun(AdgerdHefst, Dagsetning + hm(ORBIT$Inn.á.vöknun, quiet = TRUE))
SvaefingLykur <- midnightrun(AdgerdHefst,Dagsetning + hm(ORBIT$Svæfingu.lýkur, quiet = TRUE))
UtAfVoknun <- midnightrun(AdgerdHefst,Dagsetning + hm(ORBIT$Út.af.vöknun, quiet = TRUE))
UtAfSkurdgangi <- midnightrun(AdgerdHefst,Dagsetning + hm(ORBIT$Út.af.skurðgangi, quiet = TRUE))

# Undirbúningstími
UndirTimi <- as.numeric(difftime(AdgerdHefst,InnAStofu, units = "mins"))
#  UndirTimi[is.na(UndirTimi)] <- as.numeric(difftime(AdgerdHefst[is.na(UndirTimi)],SvaefingHefst[is.na(UndirTimi)], units = "mins"))
#  UndirTimi[is.na(UndirTimi)] <- as.numeric(difftime(AdgerdHefst[is.na(UndirTimi)],InnASkurdgang[is.na(UndirTimi)], units = "mins"))
#  UndirTimi[is.na(UndirTimi)] <- 0 # some times are missing here ...
#  UndirTimi <- pmax(UndirTimi,0) # just in case
AdgerdaTimi <- as.numeric(difftime(AdgerdLykur, AdgerdHefst, units = "mins"))
LokaTimi <- as.numeric(difftime(InnAVoknun, AdgerdLykur,units = "mins"))
#  LokaTimi[is.na(LokaTimi)] <- as.numeric(difftime(SvaefingLykur[is.na(LokaTimi)],AdgerdLykur[is.na(LokaTimi)],units="mins"))
#  LokaTimi[is.na(LokaTimi)] <- as.numeric(difftime(UtAfVoknun[is.na(LokaTimi)],AdgerdLykur[is.na(LokaTimi)],units="mins"))
#  LokaTimi[is.na(LokaTimi)] <- 0 # some times are missing here ...
#  LokaTimi <- pmax(LokaTimi,0) # hefur komið -ve timi ?!
Skurdstofutimi <- UndirTimi + AdgerdaTimi + LokaTimi

#  if ((sum(is.na(UndirTimi)) > 0) | (sum(is.na(AdgerdaTimi)) > 0) | (sum(is.na(LokaTimi)) > 0)) {
#    stop("just stopped because of NaN in timing")
#  }

Sergrein <- ORBIT$Skurðsérgreinar
Stofa <- ORBIT$`Aðgerða-stofa`

# Eiginleikar viðkomandi
KT <- ORBIT$Kennitala
ASA <- ORBIT$ASA.flokkun
Aldur <- ORBIT$Age.at.operation
Kyn <- ORBIT$Gender

# Teymi tengt ... mögulega væri hægt að nota komunúmer eða legunúmer (ekki í öllum röðum merkt)
Laeknir = ORBIT$Aðalskurðlæknir

# Legutengt ...
LeguNumer <- ORBIT$`Legu-.eða.komunúmer`
# Athuga hvort viðkomandi haf farið á legudeild, legu númer 
LeguInnritunartimi <- rep(NaN, length(LeguNumer))
LeguInnritun <- rep(NaN, length(LeguNumer))
LeguUtskrift <- rep(NaN, length(LeguNumer))
LeguUtskriftartimi <- rep(NaN, length(LeguNumer))

for (k in seq(1,length(LeguNumer))) {
  j <- which(SAGALEGUDEILD$Legunúmer == LeguNumer[k])
  if (length(j) == 1) {  # default er NaN
    LeguInnritun[k] <-  SAGALEGUDEILD$Dagsetning.innskriftar[j]
    LeguInnritunartimi[k] <- SAGALEGUDEILD$`Innritunar-tími`[j]
    LeguUtskrift[k] <-  SAGALEGUDEILD$Dagsetning.útskriftar[j]
    LeguUtskriftartimi[k] <- SAGALEGUDEILD$`Útskriftar-tími`[j]
  }
  else if (length(j) > 1) { # villutékk
    warning("legunúmer we ekki einkvæmt!")
    print(SAGALEGUDEILD$Dagsetning.innskriftar[j])
  }
}
LeguInnritun <- ymd(convertToDateTime(LeguInnritun))+hm(LeguInnritunartimi)
LeguUtskrift <- ymd(convertToDateTime(LeguUtskrift))+hm(LeguUtskriftartimi)
LeguDagar <- as.numeric(difftime(LeguUtskrift,LeguInnritun,units = "days"))

# Reikna líkur á að vera n-daga

ukort = unique(Adgerdakort)
LeguLikur <- matrix(rep(0,length(ukort)*14), nrow = length(ukort)); rownames(LeguLikur) <- ukort
LeguFjoldi <- matrix(rep(0,length(ukort)), nrow = length(ukort)); rownames(LeguFjoldi) <- ukort
for (uk in ukort) {
  idx = which(uk == Adgerdakort)
  Dagar <- LeguDagar[idx]
  tbl <- table(cut(Dagar,seq(0,14)))
  LeguFjoldi[uk] <- sum(tbl)  
  if (sum(tbl) > 0)
    tbl <- 1-cumsum(tbl / sum(tbl))
  LeguLikur[uk,] <- tbl
}

# Gjörgæslutengt ...

LeguNumer <- ORBIT$`Legu-.eða.komunúmer`
# Athuga hvort viðkomandi haf farið á legudeild, legu númer 
GjorInnritunartimi = rep(NaN, length(LeguNumer))
GjorUtskriftartimi = rep(NaN, length(LeguNumer))
for (k in seq(1,length(LeguNumer))) {
  j = which(SAGAGJORGAESLA$`Kennitala/gervikennitala` == KT[k])
  if (length(j) == 1) {  # default er NaN
    GjorInnritunartimi[k] =  SAGAGJORGAESLA$Dagsetning.innskriftar.dvalar[j]
    GjorUtskriftartimi[k] =  SAGAGJORGAESLA$Dagsetning.útskriftar.dvalar[j]
  }
  else if (length(j) > 1) { # villutékk
    ##     print(ymd_hms(convertToDateTime(SAGAGJORGAESLA$Dagsetning.innskriftar.dvalar[j])))
    ##     print("margir möguleikar fyrir gjörgæslu... tek að sem kemur eftir aðgerð og innan við 48 tíma.")
    # hvað skal gera, ef viðkomani er núþegar á gjörgæslu að bíða eftir þessa aðgerð ??? ATH.
    tmptimi <- ymd_hms(convertToDateTime(SAGAGJORGAESLA$Dagsetning.innskriftar.dvalar[j]))
    jj <- which((Dagsetning[k] < tmptimi) & (tmptimi < (Dagsetning[k] + hms('48:00:00') )))
    if (length(jj)>0) {
      j = j[jj[1]]
      GjorInnritunartimi[k] =  SAGAGJORGAESLA$Dagsetning.innskriftar.dvalar[j]
      GjorUtskriftartimi[k] =  SAGAGJORGAESLA$Dagsetning.útskriftar.dvalar[j]
    }
  }
}

GjorInnritunartimi <- ymd_hms(convertToDateTime(GjorInnritunartimi))
GjorUtskriftartimi <- ymd_hms(convertToDateTime(GjorUtskriftartimi))
GjorDagar = as.numeric(difftime(GjorUtskriftartimi,GjorInnritunartimi,units = "days"))

ukort = unique(Adgerdakort)
GjorLikur <- matrix(rep(0,length(ukort)*14), nrow = length(ukort))
rownames(GjorLikur) <- ukort
GjorFjoldi <- matrix(rep(0,length(ukort)), nrow = length(ukort))
rownames(GjorFjoldi) <- ukort
for (uk in ukort) {
  idx <- which(uk == Adgerdakort)
  Dagar <- GjorDagar[idx]
  tbl <- table(cut(Dagar,seq(0,14)))
  GjorFjoldi[uk] = sum(tbl)
  if (sum(tbl) > 0)
    tbl <- 1-cumsum(tbl / sum(tbl))
  GjorLikur[uk,] <- tbl
}

# Biðlistatengt ...
## í vinnslu


adkort = data.frame(Dagsetning, AdgerdHefst, Adgerdakort, Skurdstofutimi, LeguDagar, GjorDagar, Laeknir, 
                    Kyn, Stofa, ASA, Aldur, Sergrein,
                    UndirTimi, AdgerdaTimi, LokaTimi, AcuteElective, ID)


# save data frames to file
save(file="adkort.Rdata", list = c("adkort", "LeguLikur", "LeguFjoldi", "GjorLikur", "GjorFjoldi", "AdgerdLykur", "AdgerdHefst", "InnASkurdgang", "UtAfSkurdgangi"))


