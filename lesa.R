# Les Excel skjöl og mynda data.frame sem er notað í greiningu
require(openxlsx)
require(lubridate) # https://www.r-statistics.com/2012/03/do-more-with-dates-and-times-in-r-with-lubridate-1-1-0/


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
rm(list = c("O"))

# Finna einkvæma aðgerðakort
adgerdakort = unique(ORBIT$Aðgerðarkort)

# Búum til data frame fyrir hvert kort með upplýsingum ...
adkort = list()
for (a in adgerdakort) {
  i <- which(ORBIT$Aðgerðarkort == a)
  dt <- hm(ORBIT$Svæfing.hefst[i]) - hm(ORBIT$Svæfingu.lýkur[i])
  adkort[[adgerdakort[1]]] = data.frame(skurdtimi = c(1:10), undir=c(1:10))
}
  

