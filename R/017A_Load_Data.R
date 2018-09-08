setwd("/Volumes/RESEARCH_HD/017/raw_data")

acq.event <- readxl::read_excel("017_Heathercare_Acquisition.xls", sheet = 1, skip = 1, na = "")
ali.event <- readxl::read_excel("017_Heathercare_Alliance.xls", sheet = 1, skip = 1, na = "")
names(acq.event) <- stringr::str_replace_all(names(acq.event), pattern = "\n| \n| \n | ", replacement = "_")
names(ali.event) <- stringr::str_replace_all(names(ali.event), pattern = "\n| \n| \n | ", replacement = "_")

partcipants_1 <- unlist(purrr::map(ali.event$Parti._CUSIP, function(x){stringr::str_split(x, pattern = "\n")}))
partcipants_2 <- unlist(purrr::map(ali.event$`Partici-_pant_Ultimate_Parent_CUSIP`, function(x){stringr::str_split(x, pattern = "\n")}))

mean(partcipants_1 %in% acq.event$Target_CUSIP)
mean(partcipants_2 %in% acq.event$Target_CUSIP)

mean(acq.event$Target_CUSIP %in% partcipants_1)
mean(acq.event$Target_CUSIP %in% partcipants_2)

paste0("There are ", length(unique(partcipants_1[partcipants_1 %in% acq.event$Target_CUSIP])), " alliance participants that are related to acquisitions.")
paste0("There are ", length(unique(partcipants_2[partcipants_2 %in% acq.event$Target_CUSIP])), " alliance participants that are related to acquisitions.")
