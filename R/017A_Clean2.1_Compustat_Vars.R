# last run: 11.18.2018

setwd("/Volumes/RESEARCH_HD/017/raw_data")
regrrr::load.pkgs(c("readr","data.table","xts","tidyr","dplyr","stringr"))

# merge Acq_Ali with Car File #
Acq_Ali_Merged <- read.csv("Acq_Ali_Merged.csv", stringsAsFactors = FALSE)
car.file <- read.csv("car.file.017A_11282018.csv", stringsAsFactors = FALSE)
car.file <- car.file[ ,c("cusipAup", "date_ann", "window1", "window3", "window5", "window7", "window9", "window11", "cusipAup_10", "Acquirer.Flag")]
Acq_Ali_Merged <- merge(Acq_Ali_Merged, car.file, by = c("cusipAup", "date_ann"))
Acq_Ali_Merged$Target.Flag <- ifelse(Acq_Ali_Merged$target.cusip_UP == Acq_Ali_Merged$cusipAup, 1, 0)

## 1.1: download compustat data #####
# go.to.compustat <- unique(Acq_Ali_Merged$cusipAup_10) # use this file to download daily stock return
# write.table(go.to.compustat,"go.to.compustat.txt", col.names = FALSE) # use { =LEFT(RIGHT(A1,9),8) } in excel. set to text, then paste
# download done ###

compu_info <- read.csv("compustat_11282018.csv", stringsAsFactors = FALSE)
compu_info <- compu_info %>% dplyr::select(fyear, cusip, at, lt, ni, xrd, ipodate, state, sic)
compu_info$cusipAup_10 <- substr(compu_info$cusip, 1, 8)
compu_info$age <- compu_info$fyear - as.numeric(substr(compu_info$ipodate, 1, 4))

Acq_Ali_Merged <- merge(Acq_Ali_Merged, compu_info, by.x = c("cusipAup_10", "year"), by.y = c("cusipAup_10", "fyear"))
Acq_Ali_Merged <- Acq_Ali_Merged[-which(duplicated(Acq_Ali_Merged[, c("cusipAup_10", "date_ann")])),]

