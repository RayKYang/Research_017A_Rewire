# last run: 11.18.2018

setwd("/Volumes/RESEARCH_HD/017/raw_data")
regrrr::load.pkgs(c("readr","data.table","xts","tidyr","dplyr","stringr"))

Acq_Ali_Merged <- read.csv("Acq_Ali_Merged_11282018.csv", stringsAsFactors = FALSE)

######################################################################################################################
# selection_file: merge Acq_Ali_Merged with unimpacting MnAs
Acq_Ali_Merged_w_unimpacting_MnAs <- read.csv("MnA_centrality_chang_all_11302018.csv", stringsAsFactors = FALSE)
Acq_Ali_Merged_w_unimpacting_MnAs <- Acq_Ali_Merged_w_unimpacting_MnAs %>% dplyr::filter(impacting_or_not == FALSE)

Acq_Ali_Merged_w_unimpacting_MnAs$Impact_or_Not <- 0
Acq_Ali_Merged$Impact_or_Not <- 1

common_col <- dplyr::intersect(names(Acq_Ali_Merged), names(Acq_Ali_Merged_w_unimpacting_MnAs))
Acq_Ali_Merged_w_unimpacting_MnAs <- Acq_Ali_Merged_w_unimpacting_MnAs %>% dplyr::select(common_col)
selection_file <- dplyr::bind_rows(Acq_Ali_Merged_w_unimpacting_MnAs, Acq_Ali_Merged[, common_col])

# to add cumpustat data, searched the entire data base, because many unimpacting firms are not public firms
pad.6digit <- function(str){ifelse(nchar(str)<6, stringr::str_pad(str, width=6, side="left", pad="0"),str)} # pad 0's to the left
selection_file$cusipAup_10 <-  paste0(pad.6digit(selection_file$cusipAup), "10")
selection_file$fyear <- as.integer(substr(selection_file$date_ann, 1, 4)) - 1
# # go.to.compustat_selection <- unique(selection_file$cusipAup_10) # use this file to download compustat data
# # write.table(go.to.compustat_selection,"go.to.compustat_selection.txt", col.names = FALSE) # use { =LEFT(RIGHT(A1,9),8) } in excel. set to text, then paste

# selection_file merged with compu
compu_selection <- read.csv("compustat_selection_12022018.csv", stringsAsFactors = FALSE)
compu_selection <- compu_selection %>% dplyr::select(fyear, cusip, at, lt, ni, xrd, ipodate, state, sic)
compu_selection$cusip <- substr(as.character(compu_selection$cusip), 1, 8)

selection_file <- merge(selection_file, compu_selection, by.x = c("cusipAup_10", "fyear"),  by.y = c("cusip", "fyear"))
selection_file <- selection_file[-which(duplicated(selection_file[,c("cusipAup_10", "date_ann")], fromLast = TRUE)),]

rm(Acq_Ali_Merged_w_unimpacting_MnAs)
rm(compu_selection)
######################################################################################################################

# 2.1 Acq_Ali merged with Car File #
car.file <- read.csv("car.file.017A_11282018.csv", stringsAsFactors = FALSE)
car.file <- car.file[ ,c("cusipAup", "date_ann", "window1", "window3", "window5", "window7", "window9", "window11", "cusipAup_10", "Acquirer.Flag")]
Acq_Ali_Merged <- merge(Acq_Ali_Merged, car.file, by = c("cusipAup", "date_ann"))
Acq_Ali_Merged$Target.Flag <- ifelse(Acq_Ali_Merged$target.cusip_UP == Acq_Ali_Merged$cusipAup, 1, 0)

## download compustat data #####
# go.to.compustat <- unique(Acq_Ali_Merged$cusipAup_10) # use this file to download compustat data
# write.table(go.to.compustat,"go.to.compustat.txt", col.names = FALSE) # use { =LEFT(RIGHT(A1,9),8) } in excel. set to text, then paste
# download done ###

# 2.2 Acq_Ali merged with Compustat File #
compu_info <- read.csv("compustat_11282018.csv", stringsAsFactors = FALSE)
compu_info <- compu_info %>% dplyr::select(fyear, cusip, at, lt, ni, xrd, ipodate, state, sic)
compu_info$cusipAup_10 <- substr(compu_info$cusip, 1, 8)
compu_info$age <- compu_info$fyear - as.numeric(substr(compu_info$ipodate, 1, 4))

Acq_Ali_Merged$fyear <- as.integer(substr(Acq_Ali_Merged$date_ann, 1, 4)) - 1
Acq_Ali_Merged <- merge(Acq_Ali_Merged, compu_info, by = c("cusipAup_10", "fyear"))
Acq_Ali_Merged <- Acq_Ali_Merged[-which(duplicated(Acq_Ali_Merged[, c("cusipAup_10", "date_ann")])),]

rm(car.file)
rm(compu_info)