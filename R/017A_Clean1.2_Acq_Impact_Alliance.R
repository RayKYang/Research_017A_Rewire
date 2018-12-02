# last run: 11.25.2018

# source function first
# 0.1 run 017A_Clean1.1 1~2.1 to get pair_n_year #####
head(pair_year)

# 0.2 get MnA events #####
ACQ_raw <- readxl::read_xls("017MnA_11052018.xls", skip = 1) %>% as.data.frame()
names(ACQ_raw) <- stringr::str_replace_all(names(ACQ_raw), "\n", ".")
names(ACQ_raw) <- stringr::str_replace_all(names(ACQ_raw), "-.| ", ".")
ACQ_raw <- ACQ_raw %>% filter( `%.of.Shares.Acq.` == 100)
MnA <- ACQ_raw[, c("Acquiror.Ultimate..Parent..CUSIP", "Target.Ultimate..Parent..CUSIP",
                   "Acquiror..CUSIP", "Target.CUSIP",
                   "Date.Announced", "Date.Effective", "Acquiror.Primary...SIC...Code", "Target.Primary...SIC..Code")]
names(MnA) <- c("acquirer.cusip_UP", "target.cusip_UP", 
                "acquirer.cusip", "target.cusip", 
                "date_ann", "date_eff", "acquirer.SIC", "target.SIC")
MnA$year <- as.integer(substr(MnA$date_ann, 1, 4))
MnA <- MnA %>% filter(!is.na(date_eff))

aq_info <- readxl::read_xlsx("017MnA_11192018.xlsx", skip = 1) %>% filter(`% of\r\nShares\r\nAcq.` == 100) %>% as.data.frame() 
names(aq_info) <- stringr::str_replace_all(names(aq_info), "\r|\n", ".")
names(aq_info) <- stringr::str_replace_all(names(aq_info), "-.| ", ".")
aq_info <- aq_info %>% dplyr::select(Date..Announced, Acquiror...CUSIP, Acquiror..Ultimate...Parent...CUSIP, 
                                     Target..CUSIP, Target..Ultimate...Parent...CUSIP, Acquiror...State, Target..State)
MnA <- merge(MnA, aq_info, by.x = c("date_ann", "acquirer.cusip", "acquirer.cusip_UP", "target.cusip", "target.cusip_UP"), 
                           by.y = c("Date..Announced", "Acquiror...CUSIP", "Acquiror..Ultimate...Parent...CUSIP", 
                                                "Target..CUSIP", "Target..Ultimate...Parent...CUSIP"))  
MnA <- MnA[-which(duplicated(MnA[,c("date_ann", "acquirer.cusip", "target.cusip")])),]

rm(ACQ_raw)
rm(aq_info)

### 1 Acquisition-rewired Alliance Network ######
# 1.1 check row by row: if target is in the alliance network #####
check.impact <- unlist(purrr::map(1:nrow(MnA), impact_or_not_MnA))
cat(scales::percent(sum(check.impact, na.rm = TRUE)/nrow(MnA), accuracy = 0.01), "MnAs (targets) impacted the alliance network") # 6.72% 

# 1.2 build the part in pair_n_year under acquisition impact ######
length(check.impact) == nrow(MnA)
impacting.MnAs <- MnA[which(check.impact), ]
cat(nrow(impacting.MnAs), "MnAs impacted the alliance network.")

updated_blocks <- do.call(rbind, purrr::map(1:nrow(impacting.MnAs), update.alliance.pair))
updated_blocks$MnA_adjusted <- TRUE

# 1.3 update pair_year ######
affected_Ali_row <- unlist(purrr::map(1:nrow(MnA), affected_Alliance_row_by_each_MnA))
if(length(affected_Ali_row)*2 == nrow(updated_blocks)){"Perfect!"}
pair_year_unaffected <- pair_year[-affected_Ali_row, ]
pair_year_unaffected$MnA_adjusted <- FALSE
pair_year_updated <- rbind(pair_year_unaffected, updated_blocks)
cat(scales::percent(sum(pair_year_updated$MnA_adjusted)/2/nrow(pair_year), accuracy = 0.01), "pairs in alliance network got affected by MnA") #11.27% 

rm(pair_year_unaffected)
rm(updated_blocks)
rm(pair_year)

################################################
### 2.1 Acquisition's Impact on Alliance Network #
impacting.MnAs$event_number <- paste0("MnA_", 1:nrow(impacting.MnAs))
impacting.MnAs$Branch_Acquired  <- with(impacting.MnAs, ifelse(target.cusip_UP == target.cusip, "N", "Y"))
impacting.MnAs$Branch_Acquiring <- with(impacting.MnAs, ifelse(acquirer.cusip_UP == acquirer.cusip, "N", "Y"))

impacting.MnAs$Acquiring_Itself <- with(impacting.MnAs, ifelse(acquirer.cusip_UP == target.cusip_UP, "Y", "N"))
impacting.MnAs <- impacting.MnAs %>% dplyr::filter(Acquiring_Itself == "N") %>% dplyr::select(-Acquiring_Itself)

start <- Sys.time()
result_list <- purrr::map(1:nrow(impacting.MnAs), safely(add_centrality_change))
which(unlist(purrr::map(result_list, function(x) !is.null(x$error))))
impacting.MnAs_centrality_change <- do.call(rbind, purrr::map(result_list, function(x) x$result))
# impacting.MnAs_centrality_change <- do.call(rbind, purrr::map(1:nrow(impacting.MnAs), add_centrality_change))
Sys.time() - start # 3 hours

Acq_Ali_Merged <- merge(impacting.MnAs_centrality_change, impacting.MnAs, by = "event_number")
write.csv(Acq_Ali_Merged, "Acq_Ali_Merged_11282018.csv", row.names = FALSE)

###############################
# 2.2 for sample selection bias
MnA$impacting_or_not <- check.impact # this indicates whether a MnA impacted the alliance network
start <- Sys.time()
MnA_result_list <- purrr::map(1:nrow(MnA), safely(add_centrality_change_2.2))
which(unlist(purrr::map(MnA_result_list, function(x) !is.null(x$error))))

# add acquisition information
for(i in which_OK){
  if(!is.null(nrow(MnA_result_list[[i]]$result)) && nrow(MnA_result_list[[i]]$result) > 0){
  MnA_result_list[[i]]$result$date_ann <- MnA$date_ann[i]
  MnA_result_list[[i]]$result$acquirer.cusip <- MnA$acquirer.cusip[i]
  MnA_result_list[[i]]$result$acquirer.cusip_UP <- MnA$acquirer.cusip_UP[i]
  MnA_result_list[[i]]$result$target.cusip <- MnA$target.cusip[i]
  MnA_result_list[[i]]$result$target.cusip_UP <- MnA$target.cusip_UP[i]
  }
  print(i)
}

which_OK <- which(unlist(purrr::map(MnA_result_list, function(x){is.null(x$error) && ncol(x$result) == 179})))
MnA_centrality_change <- do.call(rbind, purrr::map(MnA_result_list[which_OK], function(x) x$result))
write.csv(MnA_centrality_change, "MnA_centrality_chang_all_11302018.csv", row.names = FALSE)
Sys.time() - start # 