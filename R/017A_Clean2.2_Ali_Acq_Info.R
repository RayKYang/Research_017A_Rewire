# last run: 12.02.2018

setwd("/Volumes/RESEARCH_HD/017/raw_data")
regrrr::load.pkgs(c("readr","data.table","xts","tidyr","dplyr","stringr","purrr","lubridate","regrrr"))

# ### read in ali info data #####
# al_info <- readxl::read_xlsx("017Alliance_11192018.xlsx", skip = 1) %>% as.data.frame()
# names(al_info) <- stringr::str_replace_all(names(al_info), "\r|\n", ".")
# names(al_info) <- stringr::str_replace_all(names(al_info), "-.| ", ".")

### 1.1 read in acq info data ###
aq_info <- readxl::read_xlsx("017MnA_11192018.xlsx", skip = 1) %>% filter(`% of\r\nShares\r\nAcq.` == 100) %>% as.data.frame() 
names(aq_info) <- stringr::str_replace_all(names(aq_info), "\r|\n", ".")
names(aq_info) <- stringr::str_replace_all(names(aq_info), "-.| ", ".")
# aq_info <- aq_info[, - which(check_na_in(aq_info) > 60)]
aq_info <- aq_info %>% dplyr::select(Date..Announced, Acquiror..Ultimate...Parent...CUSIP, Acquiror...CUSIP, 
                                     `Value.of..Transaction..($mil)`, Target..Ultimate...Parent...CUSIP, Target..CUSIP,
                                     Target..Nation)

### 1.2 acquisition experience ###
aq_info_by_acquirer <- split(aq_info, aq_info$Acquiror..Ultimate...Parent...CUSIP)
get.experience <- function(test){
# test <- aq_info_by_acquirer[[86]]
test$experience <- 0:(nrow(test)-1)
return(test)
}
aq_info <- do.call(rbind, purrr::map(aq_info_by_acquirer, get.experience))
aq_info_experience <- aq_info %>% dplyr::select(Acquiror..Ultimate...Parent...CUSIP, Date..Announced, experience, `Value.of..Transaction..($mil)`)
names(aq_info_experience)[4] <- "Value_of_Transaction"
aq_info_experience$Date..Announced <- as.character(aq_info_experience$Date..Announced)
aq_info_experience <- aq_info_experience[-which(duplicated(aq_info_experience[,c(1,2)])),]
  
### 1.3 merged with acquisition experience ###
all.equal(class(aq_info_experience$Date..Announced), class(selection_file$date_ann), class(Acq_Ali_Merged$date_ann))
selection_file <- selection_file %>% dplyr::left_join(aq_info_experience, by = c("cusipAup"="Acquiror..Ultimate...Parent...CUSIP", "date_ann"="Date..Announced"))
Acq_Ali_Merged <- Acq_Ali_Merged %>% dplyr::left_join(aq_info_experience, by = c("cusipAup"="Acquiror..Ultimate...Parent...CUSIP", "date_ann"="Date..Announced"))
selection_file$experience[which(is.na(selection_file$experience))] <- 0
Acq_Ali_Merged$experience[which(is.na(Acq_Ali_Merged$experience))] <- 0

# write.csv(Acq_Ali_Merged_info, "Acq_Ali_Merged_info_11282018.csv", row.names = FALSE)
