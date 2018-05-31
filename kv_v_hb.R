require(openxlsx)
require(lubridate) # https://www.r-statistics.com/2012/03/do-more-with-dates-and-times-in-r-with-lubridate-1-1-0/

load('adkort.Rdata')
# the week day starts at 1 (Monday)
vd <- wday(adkort$Dagsetning,week_start = getOption("lubridate.week.start", 1))
ar <- year(adkort$Dagsetning)
kv <- grepl('Kv.', adkort$Stofa)
hb <- grepl('Hb.', adkort$Stofa)
Start <- InnASkurdgang
Start[is.na(Start)] <- AdgerdHefst[is.na(Start)]
End <- UtAfSkurdgangi
End[is.na(End)] <- AdgerdLykur[is.na(End)]

stats_vd <- NULL
stats_he <- NULL

cat("...", file="arekstur.txt", sep="\n")

for (yr in seq(2009,2018,1)) {
  idx_kv = ((kv == TRUE) & (ar == yr) & (vd <= 5) & ((hour(AdgerdHefst) < 8) | (hour(Start) > 20)))
  idx_hb = ((hb == TRUE) & (ar == yr) & (vd <= 5) & ((hour(AdgerdHefst) < 8) | (hour(Start) > 20))) 
  overlap = 0
  for (id in which(idx_kv)) {
    # byrjar aðgerð á samatíma og eitthvað er í gangi á hringbraut ?
    overlap = overlap +(any((adkort$Dagsetning[id]==adkort$Dagsetning[idx_hb])&(Start[id] > Start[idx_hb]) & (Start[id] < End[idx_hb])) | any((adkort$Dagsetning[id]==adkort$Dagsetning[idx_hb])&(Start[idx_hb] > Start[id]) & (Start[idx_hb] < End[id])))
    if ((any((adkort$Dagsetning[id]==adkort$Dagsetning[idx_hb])&(Start[id] > Start[idx_hb]) & (Start[id] < End[idx_hb])) | any((adkort$Dagsetning[id]==adkort$Dagsetning[idx_hb])&(Start[idx_hb] > Start[id]) & (Start[idx_hb] < End[id]))) == TRUE) {
      print(AdgerdHefst[id])
      cat("kvennadeild:", file="arekstur.txt", append = T, sep="\n")
      tmp<-print(adkort[id,c(2,7,9)])
      cat(sprintf("%s %s %s", tmp$AdgerdHefst, tmp$Laeknir,tmp$Stofa), file="arekstur.txt", append = T, sep = "\n")
      cat("hringbraut:", file="arekstur.txt", append = T, sep="\n")
      idx_which = which(idx_hb)
      tmp<-print(adkort[idx_which[(((adkort$Dagsetning[id]==adkort$Dagsetning[idx_hb])&(Start[id] > Start[idx_hb]) & (Start[id] < End[idx_hb])) | ((adkort$Dagsetning[id]==adkort$Dagsetning[idx_hb])&(Start[idx_hb] > Start[id]) & (Start[idx_hb] < End[id])))
],c(2,7,9)])
      cat(sprintf("%s %s %s", tmp$AdgerdHefst, tmp$Laeknir,tmp$Stofa), file="arekstur.txt", append = T, sep = "\n")
      cat("...", file="arekstur.txt", append = T, sep="\n")
    }
  }
  stats_vd = c(stats_vd,overlap)
  idx_kv = ((kv == TRUE) & (ar == yr) & (vd > 5))
  idx_hb = ((hb == TRUE) & (ar == yr) & (vd > 5))
  overlap = 0
  for (id in which(idx_kv)) {
    overlap = overlap+(any((adkort$Dagsetning[id]==adkort$Dagsetning[idx_hb])&(Start[id] > Start[idx_hb]) & (Start[id] < End[idx_hb])) | any((adkort$Dagsetning[id]==adkort$Dagsetning[idx_hb])&(Start[idx_hb] > Start[id]) & (Start[idx_hb] < End[id])))
    if ((any((adkort$Dagsetning[id]==adkort$Dagsetning[idx_hb])&(Start[id] > Start[idx_hb]) & (Start[id] < End[idx_hb])) | any((adkort$Dagsetning[id]==adkort$Dagsetning[idx_hb])&(Start[idx_hb] > Start[id]) & (Start[idx_hb] < End[id]))) == TRUE) {
      cat("kvennadeild (helgi):", file="arekstur.txt", append = T, sep = "\n")
      tmp<-print(adkort[id,c(2,7,9)])
      cat(sprintf("%s %s %s", tmp$AdgerdHefst, tmp$Laeknir,tmp$Stofa), file="arekstur.txt", append = T, set="\n")
      cat("hringbraut (helgi):", file="arekstur.txt", append = T, sep = "\n")
      idx_which = which(idx_hb)
      tmp<-print(adkort[idx_which[(((adkort$Dagsetning[id]==adkort$Dagsetning[idx_hb])&(Start[id] > Start[idx_hb]) & (Start[id] < End[idx_hb])) | ((adkort$Dagsetning[id]==adkort$Dagsetning[idx_hb])&(Start[idx_hb] > Start[id]) & (Start[idx_hb] < End[id])))],c(2,7,9)])
      cat(sprintf("%s %s %s", tmp$AdgerdHefst, tmp$Laeknir,tmp$Stofa), file="arekstur.txt", append = T, sep = "\n")
      cat("...", file="arekstur.txt", append = T, sep = "\n")
    }
  }
  stats_he = c(stats_he,overlap)
}

tolfr <- data.frame(Ar = seq(2009,2018,1), Overlap_viku_daga = stats_vd, Overlap_helgar = stats_he)
