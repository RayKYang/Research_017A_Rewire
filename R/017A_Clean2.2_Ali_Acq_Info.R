# last run: 11.28.2018

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
  
### 1.3 acquisition intensity ###
# get Acq_Ali_Merged from 017A_Clean2.2
get_intensity <- function(row_){ # very slow
  # row_ <- 1
  acquirer <- Acq_Ali_Merged[row_, ]$acquirer.cusip_UP
  date_ann <- Acq_Ali_Merged[row_, ]$date_ann
  event_number_ <- Acq_Ali_Merged[row_, ]$event_number
  one_year_ago <- as.Date(date_ann) - 365
  n_events_global <- aq_info %>% filter(Date..Announced > one_year_ago & Date..Announced < date_ann) %>% nrow()
  n_events_US <- aq_info %>% filter(Date..Announced > one_year_ago & Date..Announced < date_ann & Target..Nation == "United States") %>% nrow()
  return(data.frame(event_number_, n_events_global, n_events_US, stringsAsFactors = FALSE))
}
intensity <- do.call(rbind, purrr::map(1:nrow(Acq_Ali_Merged), get_intensity))
Acq_Ali_Merged <- cbind(Acq_Ali_Merged, intensity[, c(2,3)])
aq_info_experience$Date..Announced <- as.Date(aq_info_experience$Date..Announced)
Acq_Ali_Merged$date_ann <- as.Date(Acq_Ali_Merged$date_ann)
Acq_Ali_Merged_info <- dplyr::inner_join(Acq_Ali_Merged, aq_info_experience, by = c("acquirer.cusip_UP" = "Acquiror..Ultimate...Parent...CUSIP", "date_ann" = "Date..Announced"))
write.csv(Acq_Ali_Merged_info, "Acq_Ali_Merged_info_11282018.csv", row.names = FALSE)
