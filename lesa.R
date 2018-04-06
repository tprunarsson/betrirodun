# Les Excel skjöl og mynda data.frame sem er notað í greiningu
require(openxlsx)

# Hvar eru skjölin og hvað heita þau?
filepath = c("./gogn/")
filename = c("_með_aðgerðakortum.xlsx")

# Lestum eitt ár í einu:
ORBIT = NULL
for (ar in seq(2009,2010)) {
  fname = paste0(filepath,as.character(ar),filename, sep="")
  print(fname)
  O <- read.xlsx(fname, sheet = "Skurðaðgerðir - ORBIT", startRow = 2, colNames = TRUE)
  O <- O[-nrow(O),]                                     # remove last row, its not data
  ORBIT = rbind(ORBIT,O)
}
